;;; kubed-ext-core.el --- Environment-safe asynchronous transport -*- lexical-binding: t; -*-
;;; Commentary:
;; `kubed-ext--async-kubectl' takes FULL argv, including the executable,
;; and returns a request struct, including on synchronous spawn failure.
;; Callbacks receive stdout or an error string, once at most.  Cancellation
;; suppresses callbacks.  Use --with-environment for temporary bindings and
;; --apply-environment-to-new-buffer only for extension-owned new buffers.
;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'kubed)
(defgroup kubed-ext nil "Responsive Kubed extensions." :group 'kubed)
(defcustom kubed-ext-request-timeout 20
  "Positive request timeout in seconds, including queue wait."
  :type 'number :group 'kubed-ext)
(defcustom kubed-ext-request-concurrency 4
  "Positive integer maximum number of concurrent extension requests."
  :type 'integer :group 'kubed-ext)
(defcustom kubed-ext-request-queue-limit 128
  "Nonnegative integer maximum number of waiting requests."
  :type 'natnum :group 'kubed-ext)
(defcustom kubed-ext-request-byte-limit (* 8 1024 1024)
  "Positive maximum combined stdout and stderr bytes per finite request.
Exceeding this limit fails visibly and discards the incomplete response."
  :type 'integer :group 'kubed-ext)
(defcustom kubed-ext-production-context-pattern "prod\\|production"
  "Case-insensitive context pattern highlighted as production."
  :type 'regexp :group 'kubed-ext)
(defun kubed-ext--scope-label (scope)
  "Return bounded, credential-free presentation of SCOPE."
  (let* ((context (or (plist-get scope :context) "unresolved"))
         (name (or (plist-get scope :name) (plist-get scope :query)))
         (label (format "%s%s [%s/%s]"
                        (or (plist-get scope :type) "")
                        (if (and name (not (string-empty-p name))) (concat "/" name) "")
                        context (or (plist-get scope :namespace) "cluster/all")))
         (case-fold-search t))
    (setq label (truncate-string-to-width label 240 nil nil "…"))
    (if (string-match-p kubed-ext-production-context-pattern context)
        (propertize (concat "PRODUCTION — " label) 'face 'warning) label)))
(defvar kubed-ext--owners (make-hash-table :test 'eq)
  "Buffer to cleanup functions; independent of mode-local variables.")

(defun kubed-ext--own-buffer (cleanup)
  "Register CLEANUP for this buffer before major mode locals disappear."
  (puthash (current-buffer)
           (cl-adjoin cleanup (gethash (current-buffer) kubed-ext--owners))
           kubed-ext--owners)
  (add-hook 'kill-buffer-hook #'kubed-ext--release-buffer nil t)
  (add-hook 'change-major-mode-hook #'kubed-ext--release-buffer nil t))

(defun kubed-ext--release-buffer ()
  "Release authoritative ownership before buffer-local state is destroyed."
  (let ((functions (gethash (current-buffer) kubed-ext--owners)))
    (remhash (current-buffer) kubed-ext--owners)
    (mapc #'funcall functions)))

(defun kubed-ext--stop-owned-buffers (cleanup)
  "Run CLEANUP in all registered buffers owning that function."
  (maphash (lambda (buffer functions)
             (when (and (buffer-live-p buffer) (memq cleanup functions))
               (with-current-buffer buffer (funcall cleanup))))
           (copy-hash-table kubed-ext--owners)))

(defvar-local kubed-ext--environment nil "Persistent environment of an owned buffer.")
(defvar kubed-ext--requests nil "Owned unfinished requests.")
(defvar kubed-ext--request-queue nil "FIFO of waiting requests.")
(defvar kubed-ext--unloading nil "Non-nil while shutting down requests.")
(defvar kubed-ext--draining nil "Guard against recursive queue draining.")

(defun kubed-ext--reset-transport-after-reload ()
  "Discard only extension transport state retained across a source reload.

Old `kubed-ext-request' instances may have an obsolete `cl-defstruct' layout,
so this deliberately does not use request accessors.  Process and timer
ownership is recovered only from extension-named runtime objects."
  (dolist (timer (append timer-list timer-idle-list))
    (when (eq (timer--function timer) #'kubed-ext--finish-request)
      (cancel-timer timer)))
  (dolist (process (process-list))
    (when (string-prefix-p "kubed-ext-request" (process-name process))
      (set-process-sentinel process #'ignore)
      (set-process-query-on-exit-flag process nil)
      (when (process-live-p process) (delete-process process))))
  (dolist (buffer (buffer-list))
    (when (member (buffer-name buffer)
                  '(" *kubed-ext stdout*" " *kubed-ext stderr*"))
      (kill-buffer buffer)))
  (setq kubed-ext--requests nil
        kubed-ext--request-queue nil
        kubed-ext--draining nil
        kubed-ext--unloading nil))
(defvar kubed-ext-shutdown-hook nil
  "Module cleanup functions called before request shutdown; no work may launch.")
(cl-defstruct (kubed-ext-request (:constructor kubed-ext--make-request))
  argv success error environment process stdout-buffer stderr-buffer timer
  finished completing cancelled failure started (bytes 0))

(defun kubed-ext--capture-environment ()
  "Snapshot executable, directory, configured Kubed locals and environment key.
Unbound configured variables remain unbound under temporary restoration."
  (let* ((variables (delete-dups
                     (append '(default-directory process-environment exec-path
                                kubed-kubectl-program
                                kubed-default-context-and-namespace-alist
                                kubed--context-and-namespace-alist
                                kubed-execution-environment-buffer-locals
                                kubed-execution-environment-key-function
                                connection-local-profile-alist
                                connection-local-criteria-alist)
                             kubed-execution-environment-buffer-locals)))
         (locals (mapcar (lambda (var)
                           (cons var (and (boundp var)
                                          (copy-tree (symbol-value var) t))))
                         variables)))
    (setcdr (assq 'kubed-kubectl-program locals) (kubed-ext--kubectl-program))
    (list :default-directory default-directory
          :process-environment (copy-sequence process-environment)
          :exec-path (copy-sequence exec-path)
          :kubectl (kubed-ext--kubectl-program)
          :locals locals :unbound (cl-remove-if #'boundp variables)
          :eenv (copy-tree (kubed-execution-environment-key) t))))

(defmacro kubed-ext--with-environment (environment &rest body)
  "Evaluate BODY with temporary dynamic bindings from ENVIRONMENT.
Evaluate ENVIRONMENT once.  Never create or persist callback-buffer locals."
  (declare (indent 1) (debug (form body)))
  (let ((snapshot (make-symbol "snapshot")) (bindings (make-symbol "bindings")))
    `(let* ((,snapshot ,environment)
            (,bindings (cl-remove-if
                        (lambda (binding)
                          (memq (car binding) (plist-get ,snapshot :unbound)))
                        (plist-get ,snapshot :locals))))
       (cl-progv (append '(kubed-ext--environment)
                         (mapcar #'car ,bindings)
                         (plist-get ,snapshot :unbound))
           (cons ,snapshot
                 (mapcar (lambda (binding) (copy-tree (cdr binding) t)) ,bindings))
         ;; Emacs cl-progv supplies nil for missing values.  Making the
         ;; temporary binding void preserves its original binding on unwind.
         (mapc #'makunbound (plist-get ,snapshot :unbound))
         ,@body))))

(defun kubed-ext--apply-environment-to-new-buffer (environment &optional buffer)
  "Persist ENVIRONMENT in extension-owned BUFFER, defaulting to current buffer.
Do not call this helper on unrelated native or user buffers."
  (with-current-buffer (or buffer (current-buffer))
    (dolist (binding (plist-get environment :locals))
      (unless (memq (car binding) (plist-get environment :unbound))
        (set (make-local-variable (car binding)) (copy-tree (cdr binding) t))))
    (setq-local kubed-ext--environment environment)
    ;; Avoid re-resolving connection-local settings after the originating
    ;; connection's configuration changes.
    (setq-local kubed-kubectl-program (plist-get environment :kubectl))
    (dolist (variable (plist-get environment :unbound))
      (make-local-variable variable)
      (makunbound variable))))

(defun kubed-ext--kubectl-program ()
  "Return the captured or connection-local Kubed executable."
  (or (plist-get kubed-ext--environment :kubectl) (kubed-kubectl-program)))


(defun kubed-ext--resolve-scope (scope namespace-policy callback)
  "Resolve SCOPE once asynchronously, then call CALLBACK with an explicit scope.
NAMESPACE-POLICY is `default' for row operations or `all' for lists.  Read
only compact kubeconfig metadata, never cluster helpers or credential data.
Honor Kubed 0.7.1's environment cache and host-configured valid defaults."
  (let* ((environment (or (plist-get scope :environment)
                          kubed-ext--environment (kubed-ext--capture-environment)))
         (scope (plist-put (copy-sequence scope) :environment environment))
         (context (plist-get scope :context))
         (namespace (plist-get scope :namespace))
         (origin (current-buffer)))
    (if (and context (or namespace (eq namespace-policy 'all)))
        (kubed-ext--with-environment environment (funcall callback scope))
      (kubed-ext--with-environment environment
        (let ((configured (copy-tree
                           (or (alist-get (plist-get environment :eenv)
                                          kubed--context-and-namespace-alist nil nil #'equal)
                               (alist-get (file-remote-p default-directory)
                                          kubed-default-context-and-namespace-alist nil nil #'equal)))))
          (kubed-ext--async-kubectl
           (list (kubed-ext--kubectl-program) "config" "view" "-o"
                 "jsonpath={.current-context}{\"\\n\"}{range .contexts[*]}{.name}{\"\\t\"}{.context.namespace}{\"\\n\"}{end}")
           (lambda (text)
             (let* ((lines (split-string text "\n"))
                    (contexts (mapcar (lambda (line) (split-string line "\t"))
                                      (cl-remove-if #'string-empty-p (cdr lines))))
                    (valid-default (and (assoc (car configured) contexts) configured))
                    (effective (or context (car valid-default) (car lines))))
               (unless (and (stringp effective) (not (string-empty-p effective)))
                 (error "Kubeconfig has no effective context"))
               (setq scope (plist-put scope :context effective))
               (when (and (eq namespace-policy 'default) (not namespace))
                 (setq scope (plist-put scope :namespace
                                        (or (and (equal effective (car valid-default))
                                                 (cdr valid-default))
                                            (let ((value (cadr (assoc effective contexts))))
                                              (unless (or (null value) (string-empty-p value)) value))
                                            "default"))))
               (when (buffer-live-p origin)
                 (with-current-buffer origin
                   (kubed-ext--with-environment environment (funcall callback scope))))))
           (lambda (failure) (message "Kubed scope resolution failed: %s" failure))))))))

(defmacro kubed-ext--at-scope (scope &rest body)
  "Evaluate BODY at SCOPE's origin with its captured environment."
  (declare (indent 1) (debug (form body)))
  `(let ((snapshot-scope ,scope))
     (with-current-buffer (or (plist-get snapshot-scope :origin) (current-buffer))
       (kubed-ext--with-environment
           (or (plist-get snapshot-scope :environment) kubed-ext--environment
               (kubed-ext--capture-environment))
         ,@body))))

(defmacro kubed-ext--resolved (context namespace policy &rest body)
  "Run BODY after resolving CONTEXT and NAMESPACE according to POLICY."
  (declare (indent 3) (debug (form form form body)))
  `(kubed-ext--resolve-scope
    (list :context ,context :namespace ,namespace) ,policy
    (lambda (resolved)
      (let ((,context (plist-get resolved :context))
            (,namespace (plist-get resolved :namespace)))
        ,@body))))

(defun kubed-ext-request-finished-p (request)
  "Return whether REQUEST has completed and released its resources."
  (kubed-ext-request-finished request))

(defun kubed-ext--validate-request-config ()
  "Signal an error for invalid transport numeric settings."
  (unless (and (numberp kubed-ext-request-timeout)
               (> kubed-ext-request-timeout 0)
               (< kubed-ext-request-timeout most-positive-fixnum)
               (integerp kubed-ext-request-byte-limit)
               (> kubed-ext-request-byte-limit 0)
               (integerp kubed-ext-request-concurrency)
               (> kubed-ext-request-concurrency 0)
               (integerp kubed-ext-request-queue-limit)
               (>= kubed-ext-request-queue-limit 0))
    (user-error "Timeout must be positive; concurrency positive integer; queue limit nonnegative integer")))

(defun kubed-ext--active-request-count ()
  "Count reserved process slots, including requests currently completing."
  (cl-count-if (lambda (request)
                 (and (kubed-ext-request-started request)
                      (not (kubed-ext-request-finished request))))
               kubed-ext--requests))

(defun kubed-ext--request-cleanup (request)
  "Cancel REQUEST's timer and release only its owned processes and buffers."
  (when (timerp (kubed-ext-request-timer request))
    (cancel-timer (kubed-ext-request-timer request)))
  (setf (kubed-ext-request-timer request) nil)
  (let ((process (kubed-ext-request-process request)))
    (when (processp process)
      (set-process-sentinel process #'ignore)
      (when (process-live-p process) (delete-process process))))
  (dolist (buffer (list (kubed-ext-request-stdout-buffer request)
                        (kubed-ext-request-stderr-buffer request)))
    (when (buffer-live-p buffer)
      (let ((process (get-buffer-process buffer)))
        (when (processp process)
          (set-process-query-on-exit-flag process nil)
          (delete-process process)))
      (kill-buffer buffer)))
  (setq kubed-ext--request-queue (delq request kubed-ext--request-queue)
        kubed-ext--requests (delq request kubed-ext--requests)))

(defun kubed-ext--finish-request (request &optional failure)
  "Finish REQUEST once with FAILURE or stdout, cleaning up despite callback errors."
  (unless (or (kubed-ext-request-finished request)
              (kubed-ext-request-completing request))
    (setf (kubed-ext-request-completing request) t
          (kubed-ext-request-failure request) failure)
    (let ((stdout (if (buffer-live-p (kubed-ext-request-stdout-buffer request))
                      (with-current-buffer (kubed-ext-request-stdout-buffer request)
                        (buffer-substring-no-properties (point-min) (point-max))) ""))
          (stderr (if (buffer-live-p (kubed-ext-request-stderr-buffer request))
                      (with-current-buffer (kubed-ext-request-stderr-buffer request)
                        (buffer-substring-no-properties (point-min) (point-max))) "")))
      ;; Kill a timed-out process BEFORE invoking user callbacks.
      (unwind-protect
          (progn
            (kubed-ext--request-cleanup request)
            (unless (or kubed-ext--unloading (kubed-ext-request-cancelled request))
              (condition-case err
                  (kubed-ext--with-environment (kubed-ext-request-environment request)
                    (if failure
                        (when (functionp (kubed-ext-request-error request))
                          (funcall (kubed-ext-request-error request)
                                   (if (string-empty-p (string-trim stderr))
                                       failure
                                     (concat failure ": " (string-trim stderr)))))
                      (when (functionp (kubed-ext-request-success request))
                        (funcall (kubed-ext-request-success request) stdout))))
                ((error quit)
                 (message "Kubed-ext callback error: %s" (error-message-string err))))))
        (setf (kubed-ext-request-finished request) t
              (kubed-ext-request-completing request) nil)
        (kubed-ext--start-queued-requests)))))

(defun kubed-ext-cancel-request (request)
  "Cancel REQUEST without callbacks, whether queued or running."
  (when (and (kubed-ext-request-p request)
             (not (kubed-ext-request-finished request)))
    (setf (kubed-ext-request-cancelled request) t)
    (kubed-ext--finish-request request "request cancelled")))

(defun kubed-ext--request-filter (request buffer text)
  "Retain TEXT in BUFFER only within REQUEST's combined byte allowance."
  (unless (or (kubed-ext-request-finished request)
              (kubed-ext-request-completing request))
    (cl-incf (kubed-ext-request-bytes request) (string-bytes text))
    (if (> (kubed-ext-request-bytes request) kubed-ext-request-byte-limit)
        (kubed-ext--finish-request request
                                   (format "Response exceeds %d-byte limit; narrow the query"
                                           kubed-ext-request-byte-limit))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (goto-char (point-max)) (insert text))))))

(defun kubed-ext--start-request (request)
  "Reserve a slot and start REQUEST with file-handler support."
  (setf (kubed-ext-request-started request) t
        (kubed-ext-request-stdout-buffer request) (generate-new-buffer " *kubed-ext stdout*")
        (kubed-ext-request-stderr-buffer request) (generate-new-buffer " *kubed-ext stderr*"))
  (condition-case err
      (kubed-ext--with-environment (kubed-ext-request-environment request)
        (let ((process
               (make-process
                :name "kubed-ext-request"
                :buffer (kubed-ext-request-stdout-buffer request)
                :stderr (kubed-ext-request-stderr-buffer request)
                :command (kubed-ext-request-argv request)
                :filter (lambda (_process text)
                          (kubed-ext--request-filter request
                                                     (kubed-ext-request-stdout-buffer request) text))
                :file-handler t :connection-type 'pipe :noquery t
                :sentinel
                (lambda (process _event)
                  (when (memq (process-status process) '(exit signal failed closed))
                    (kubed-ext--finish-request
                     request (unless (and (eq (process-status process) 'exit)
                                          (zerop (process-exit-status process)))
                               (format "process exited %s" (process-exit-status process)))))))))
          (unless (processp process) (error "File handler did not create a process"))
          (setf (kubed-ext-request-process request) process)
          (when-let* ((stderr-process (get-buffer-process
                                      (kubed-ext-request-stderr-buffer request))))
            (set-process-filter
             stderr-process (lambda (_process text)
                              (kubed-ext--request-filter
                               request (kubed-ext-request-stderr-buffer request) text))))))
    (error (kubed-ext--finish-request request (error-message-string err)))))

(defun kubed-ext--start-queued-requests ()
  "Drain the FIFO iteratively; spawn failures never recursively drain it."
  (unless kubed-ext--draining
    (let ((kubed-ext--draining t))
      (while (and kubed-ext--request-queue (not kubed-ext--unloading)
                  (< (kubed-ext--active-request-count) kubed-ext-request-concurrency))
        (kubed-ext--start-request (pop kubed-ext--request-queue))))))

(defun kubed-ext--async-kubectl (argv success &optional error)
  "Run FULL ARGV asynchronously and return a `kubed-ext-request'.
SUCCESS receives stdout, ERROR receives a failure string.  Neither callback
runs after cancellation or shutdown.  Capture environment before queuing.
Reject invalid configuration, invalid argv or a full FIFO before admission."
  (kubed-ext--validate-request-config)
  (unless (and (consp argv) (cl-every #'stringp argv) (not (string-empty-p (car argv))))
    (user-error "Expected full argv including executable"))
  (when kubed-ext--unloading (user-error "Kubed-ext is shutting down"))
  (when (and (or kubed-ext--request-queue
                 (>= (kubed-ext--active-request-count) kubed-ext-request-concurrency)
                 kubed-ext--draining)
             (>= (length kubed-ext--request-queue) kubed-ext-request-queue-limit))
    (user-error "Kubed-ext request queue is full"))
  (let ((request (kubed-ext--make-request :argv (copy-sequence argv)
                                          :success success :error error
                                          :environment (kubed-ext--capture-environment))))
    (push request kubed-ext--requests)
    (setq kubed-ext--request-queue (nconc kubed-ext--request-queue (list request)))
    (setf (kubed-ext-request-timer request)
          (run-at-time kubed-ext-request-timeout nil
                       #'kubed-ext--finish-request request "request timed out"))
    (kubed-ext--start-queued-requests)
    request))

(defun kubed-ext--cancel-all-requests ()
  "Stop module timers and all owned requests without callbacks or new launches.
Run `kubed-ext-shutdown-hook' under the shutdown guard.  This is the disable
entry point; resource view cleanup is registered by the resource module."
  (let ((kubed-ext--unloading t))
    (dolist (cleanup kubed-ext-shutdown-hook)
      (condition-case err (funcall cleanup)
        (error (message "Kubed-ext shutdown cleanup: %s" (error-message-string err)))))
    (dolist (request (copy-sequence kubed-ext--requests))
      (kubed-ext-cancel-request request))
    (setq kubed-ext--request-queue nil)))

(defun kubed-ext--scope-valid-p (scope)
  "Return non-nil if SCOPE has an explicit nonempty type and resource name."
  (cl-every (lambda (key) (let ((value (plist-get scope key)))
                            (and (stringp value) (not (string-empty-p value)))))
            '(:type :name)))

(defun kubed-ext--scope-args (scope)
  "Return context/namespace flags for SCOPE, without executable or verb.
SCOPE is a plist: :context string or nil, :namespace string or nil,
:all-namespaces boolean.  Selected-row scopes have a namespace and no -A.
Nil namespace alone does not imply -A; list scopes must explicitly request it."
  (append (when (plist-get scope :context)
            (list (concat "--context=" (plist-get scope :context))))
          (cond ((plist-get scope :namespace)
                 (list (concat "--namespace=" (plist-get scope :namespace))))
                ((plist-get scope :all-namespaces) '("-A")))))
(kubed-ext--reset-transport-after-reload)

(provide 'kubed-ext-core)
;;; kubed-ext-core.el ends here
