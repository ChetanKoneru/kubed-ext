;;; kubed-ext-logs.el --- Managed native Stern sessions -*- lexical-binding: t; -*-
;;; Commentary:
;; Stern patterns are Go/RE2, not Emacs regular expressions.  No Kubed override.
;;; Code:
(require 'ansi-color)
(require 'kubed-ext-terminals)
(defcustom kubed-ext-stern-program "stern" "Stern executable."
  :type 'string :group 'kubed-ext)
(defcustom kubed-ext-log-retention-characters 200000 "Maximum retained decoded log characters."
  :type 'natnum :group 'kubed-ext)
(defcustom kubed-ext-log-default-tail 200 "Default initial lines per container."
  :type 'natnum :group 'kubed-ext)
(defcustom kubed-ext-log-default-since "1h" "Default initial log duration."
  :type 'string :group 'kubed-ext)
(defcustom kubed-ext-log-error-pattern "(?i)(exception|error|critical|failure|fatal|panic)"
  "RE2 inclusion expression used by error-log commands."
  :type 'string :group 'kubed-ext)
(defcustom kubed-ext-log-concurrency 4 "Maximum parallel container log requests."
  :type 'integer :group 'kubed-ext)
(defvar kubed-ext--log-buffers nil "Owned log sessions.")
(defconst kubed-ext--read-container-marker 'kubed-ext-read-container)
(defvar-local kubed-ext--log-process nil)
(defvar-local kubed-ext--log-spec nil)
(defvar-local kubed-ext--log-environment nil)
(defvar-local kubed-ext--log-follow-at-end t)
(defvar-local kubed-ext--log-status "stopped")
(defvar-local kubed-ext--log-marker nil)
(defvar-local kubed-ext--log-ansi-pending "")

(defun kubed-ext--re2-quote (text)
  "Quote literal TEXT for RE2, avoiding Emacs-specific quoting conventions."
  (mapconcat (lambda (c) (if (memq c '(?\\ ?. ?+ ?* ?? ?\( ?\) ?\[ ?\] ?\{ ?\} ?^ ?$ ?|))
                             (concat "\\" (char-to-string c)) (char-to-string c))) text ""))
(defun kubed-ext--stern-query (spec)
  "Build exact pod or supported workload query from SPEC."
  (let* ((type (or (plist-get spec :type) "pod"))
         (name (plist-get spec :query))
         (kind (cdr (assoc type '(("deployment" . "deployment") ("deployments" . "deployment")
                                 ("statefulset" . "statefulset") ("statefulsets" . "statefulset")
                                 ("daemonset" . "daemonset") ("daemonsets" . "daemonset")
                                 ("replicaset" . "replicaset") ("replicasets" . "replicaset")
                                 ("job" . "job") ("jobs" . "job") ("service" . "service") ("services" . "service")
                                 ("replicationcontroller" . "replicationcontroller")
                                 ("replicationcontrollers" . "replicationcontroller"))))))
    (cond ((plist-get spec :selector) ".*")
          ((plist-get spec :regex) (or name ".*"))
          ((plist-get spec :pods)
           (concat "^(" (mapconcat #'kubed-ext--re2-quote (plist-get spec :pods) "|") ")$"))
          ((member type '("pod" "pods")) (concat "^" (kubed-ext--re2-quote (or name "")) "$"))
          (kind (concat kind "/" name))
          (t (user-error "Stern does not support %s; use an explicit label selector" type)))))
(defun kubed-ext--stern-args (spec)
  "Build full Stern argv from SPEC using only verified native flags."
  (let ((concurrency (or (plist-get spec :concurrency) kubed-ext-log-concurrency))
        (tail (if (plist-member spec :tail) (plist-get spec :tail) kubed-ext-log-default-tail)))
    (unless (and (integerp concurrency) (> concurrency 0)) (user-error "Log concurrency must be positive"))
    (when tail (unless (and (integerp tail) (>= tail -1)) (user-error "Tail must be an integer >= -1")))
    (when (and (equal tail 0) (not (plist-get spec :follow)))
      (user-error "Stern snapshot requires nonzero tail; use follow for tail zero"))
    (append (list kubed-ext-stern-program (kubed-ext--stern-query spec) "--color=always")
            (when (plist-get spec :context) (list (concat "--context=" (plist-get spec :context))))
            (when (plist-get spec :namespace) (list (concat "--namespace=" (plist-get spec :namespace))))
            (when (plist-get spec :all-namespaces) '("--all-namespaces"))
            (when (plist-get spec :selector) (list (concat "--selector=" (plist-get spec :selector))))
            (when (stringp (plist-get spec :container))
              (list (concat "--container=^" (kubed-ext--re2-quote (plist-get spec :container)) "$")))
            (when tail (list (format "--tail=%d" tail)))
            (when-let* ((since (if (plist-member spec :since) (plist-get spec :since)
                                kubed-ext-log-default-since)))
              (list (concat "--since=" since)))
            (list (format "--max-log-requests=%d" concurrency))
            (unless (plist-get spec :follow) '("--no-follow"))
            (when (plist-get spec :timestamps)
              (list (concat "--timestamps=" (if (stringp (plist-get spec :timestamps))
                                                (plist-get spec :timestamps) "default"))))
            (if (plist-get spec :template) (list (concat "--template=" (plist-get spec :template)))
              (unless (plist-get spec :prefix) '("--output=raw")))
            (when (plist-get spec :filter) (list (concat "--include=" (plist-get spec :filter)))))))

(defun kubed-ext--log-decode (output)
  "Decode OUTPUT while retaining incomplete ANSI escape sequences locally."
  (let ((text (concat kubed-ext--log-ansi-pending output)))
    (setq kubed-ext--log-ansi-pending "")
    (when (string-match "\e\\(?:\\[[0-9;]*\\)?\\'" text)
      (setq kubed-ext--log-ansi-pending (substring text (match-beginning 0))
            text (substring text 0 (match-beginning 0))))
    (ansi-color-apply text)))
(defun kubed-ext--insert-log-output (buffer output)
  "Insert ANSI-colored OUTPUT into BUFFER, preserving browsing and markers."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unless (and (integerp kubed-ext-log-retention-characters) (> kubed-ext-log-retention-characters 0))
        (user-error "Log retention must be positive"))
      (let* ((inhibit-read-only t) (at-end (= (point) (point-max)))
             (position (copy-marker (point)))
             (windows (mapcar (lambda (w) (list w (copy-marker (window-point w))
                                               (copy-marker (window-start w))
                                               (= (window-point w) (point-max))))
                              (get-buffer-window-list buffer nil t))))
        (unwind-protect
            (progn
              (goto-char (or (and (markerp kubed-ext--log-marker) (marker-position kubed-ext--log-marker))
                             (point-max)))
              (insert (kubed-ext--log-decode output))
              (when (markerp kubed-ext--log-marker) (set-marker kubed-ext--log-marker (point)))
              (when (> (buffer-size) kubed-ext-log-retention-characters)
                (delete-region (point-min) (- (point-max) kubed-ext-log-retention-characters)))
              (goto-char (if (and at-end kubed-ext--log-follow-at-end) (point-max) position))
              (dolist (state windows)
                (when (window-live-p (car state))
                  (set-window-point (car state) (if (and (nth 3 state) kubed-ext--log-follow-at-end)
                                                   (point-max) (nth 1 state)))
                  (set-window-start (car state) (nth 2 state) t))))
          (set-marker position nil)
          (dolist (state windows) (set-marker (nth 1 state) nil) (set-marker (nth 2 state) nil)))))))

(define-derived-mode kubed-ext-log-mode special-mode "Kubed logs"
  "Managed Stern log session.  Live filters use RE2; occur uses Emacs regexps."
  (setq-local truncate-lines t)
  (setq-local kubed-ext--log-marker (copy-marker (point-max) t))
  (setq-local header-line-format '(:eval (format "%s  %s  [s stop / g restart / c clear / f RE2 / F follow]"
                                                kubed-ext--log-status (kubed-ext--scope-label kubed-ext--log-spec))))
  (kubed-ext--own-buffer #'kubed-ext--log-kill))
(keymap-set kubed-ext-log-mode-map "s" #'kubed-ext-stop-logs)
(keymap-set kubed-ext-log-mode-map "g" #'kubed-ext-restart-logs)
(keymap-set kubed-ext-log-mode-map "c" #'kubed-ext-clear-logs)
(keymap-set kubed-ext-log-mode-map "f" #'kubed-ext-log-set-filter)
(keymap-set kubed-ext-log-mode-map "F" #'kubed-ext-log-toggle-follow)
(keymap-set kubed-ext-log-mode-map "o" #'kubed-ext-filter-log-buffer)
(defun kubed-ext-stop-logs ()
  "Stop this buffer's process and reject any late output from it."
  (interactive)
  (let ((p kubed-ext--log-process))
    (setq kubed-ext--log-process nil kubed-ext--log-status "stopped")
    (when (process-live-p p) (delete-process p))))
(defun kubed-ext--log-kill ()
  "Clean up current log session and registry entry."
  (kubed-ext-stop-logs)
  (setq kubed-ext--log-buffers (delq (current-buffer) kubed-ext--log-buffers))
  (when (markerp kubed-ext--log-marker) (set-marker kubed-ext--log-marker nil)))
(defun kubed-ext-stop-all-logs ()
  "Stop all extension-owned log sessions."
  (interactive)
  (dolist (b (copy-sequence kubed-ext--log-buffers))
    (when (buffer-live-p b) (with-current-buffer b (kubed-ext-stop-logs))))
  (kubed-ext--stop-owned-buffers #'kubed-ext--log-kill)
  (kubed-ext--cancel-action-prompts))
(defalias 'kubed-ext-logs-stop-all #'kubed-ext-stop-all-logs)
(defun kubed-ext-clear-logs ()
  "Clear retained text without restarting the stream."
  (interactive)
  (let ((inhibit-read-only t)) (erase-buffer))
  (when (markerp kubed-ext--log-marker) (set-marker kubed-ext--log-marker (point-max))))
(defun kubed-ext-log-toggle-follow ()
  "Toggle following the end of the buffer, without changing stream selection."
  (interactive)
  (setq kubed-ext--log-follow-at-end (not kubed-ext--log-follow-at-end))
  (when kubed-ext--log-follow-at-end (goto-char (point-max))))
(defun kubed-ext-log-set-filter (pattern)
  "Restart this session with RE2 inclusion PATTERN; empty clears it."
  (interactive (list (read-string "Live include pattern (Go/RE2, empty clears): "
                                  (plist-get kubed-ext--log-spec :filter))))
  (unless kubed-ext--log-spec (user-error "Not a managed log session"))
  (setq kubed-ext--log-spec (plist-put kubed-ext--log-spec :filter
                                     (unless (string-empty-p pattern) pattern)))
  (kubed-ext-restart-logs))
(defun kubed-ext-restart-logs ()
  "Restart the captured launch specification and environment."
  (interactive)
  (unless kubed-ext--log-spec (user-error "Not a managed log session"))
  (kubed-ext-stop-logs)
  (setq kubed-ext--log-ansi-pending "" ansi-color-context nil)
  (let ((buffer (current-buffer)))
    (condition-case error
        (kubed-ext--with-environment kubed-ext--log-environment
          (setq kubed-ext--log-status "starting"
                kubed-ext--log-process
                (make-process
                 :name (generate-new-buffer-name "kubed-ext-stern") :buffer nil
                 :command (kubed-ext--stern-args kubed-ext--log-spec)
                 :file-handler t :connection-type 'pipe :noquery t
                 :filter (lambda (process output)
                           (when (and (buffer-live-p buffer)
                                      (eq process (buffer-local-value 'kubed-ext--log-process buffer)))
                             (with-current-buffer buffer (setq kubed-ext--log-status "streaming"))
                             (kubed-ext--insert-log-output buffer output)))
                 :sentinel (lambda (process event)
                             (when (and (buffer-live-p buffer)
                                        (eq process (buffer-local-value 'kubed-ext--log-process buffer))
                                        (memq (process-status process) '(exit signal failed)))
                               (with-current-buffer buffer
                                 (setq kubed-ext--log-status (if (zerop (process-exit-status process)) "finished" "failed"))
                                 (kubed-ext--insert-log-output buffer (format "\n[Stern %s]\n" (string-trim event)))))))))
      (error (setq kubed-ext--log-status "failed")
             (kubed-ext--insert-log-output buffer (concat "\n" (error-message-string error) "\n")))))
  kubed-ext--log-process)

(defun kubed-ext--open-log-spec (spec)
  "Resolve SPEC once and create a managed environment-owned log buffer."
  (kubed-ext--resolve-scope spec 'default #'kubed-ext--open-log-spec-resolved))
(defun kubed-ext--open-log-spec-resolved (spec)
  "Create a managed log buffer for resolved SPEC, capturing launch environment."
  (unless (executable-find kubed-ext-stern-program) (user-error "Stern is required for this workflow"))
  (kubed-ext--stern-args spec)
  (let* ((env (or (plist-get spec :environment) (kubed-ext--capture-environment)))
         (b (generate-new-buffer (format "*Kubed logs %s/%s [%s/%s] %s*"
                                         (plist-get spec :type) (or (plist-get spec :query) (plist-get spec :selector))
                                         (plist-get spec :context) (plist-get spec :namespace)
                                         (or (plist-get spec :container) "all containers")))))
    (with-current-buffer b
      (kubed-ext-log-mode)
      (kubed-ext--apply-environment-to-new-buffer env)
      (setq kubed-ext--log-spec (copy-tree spec) kubed-ext--log-environment env)
      (push b kubed-ext--log-buffers)
      (kubed-ext-restart-logs))
    (pop-to-buffer b) b))
(defun kubed-ext--call-original-kubed-logs (&rest args)
  "Call unmodified upstream logs with ARGS; no extension override is installed."
  (unless (fboundp 'kubed-logs) (user-error "Neither Stern nor upstream Kubed logs is available"))
  (let ((environment (or kubed-ext--environment (kubed-ext--capture-environment)))
        (before (buffer-list)))
    (apply #'kubed-logs args)
    (dolist (buffer (buffer-list))
      (when (and (not (memq buffer before)) (get-buffer-process buffer))
        (with-current-buffer buffer
          (kubed-ext--apply-environment-to-new-buffer environment)
          (setq-local kubed-ext--log-process (get-buffer-process buffer)
                      kubed-ext--log-environment environment)
          (push buffer kubed-ext--log-buffers)
          (kubed-ext--own-buffer #'kubed-ext--log-kill))))))
(defun kubed-ext-stern-logs
    (type resource &optional context namespace container follow limit prefix since tail timestamps)
  "Show TYPE/RESOURCE logs with upstream-compatible options.
CONTEXT/NAMESPACE select scope.  CONTAINER is exact, t means all; FOLLOW,
PREFIX, TAIL and TIMESTAMPS map to Stern.  LIMIT or absolute SINCE use
upstream Kubed because Stern cannot preserve those options."
  (interactive (append (kubed-ext--action-interactive) (list nil t)))
  (kubed-ext--resolved context namespace 'default
    (kubed-ext--stern-logs-resolved type resource context namespace container follow limit prefix since tail timestamps)))
(defun kubed-ext--stern-logs-resolved
    (type resource context namespace container follow limit prefix since tail timestamps)
  "Launch logs with resolved scope and upstream-compatible options."
  (cond
   ((eq container kubed-ext--read-container-marker)
    (unless (member type '("pod" "pods")) (user-error "Container discovery requires a selected pod"))
    (kubed-ext--read-pod-container-async
     resource "Container" context namespace
     (lambda (c) (kubed-ext-stern-logs type resource context namespace c follow limit prefix since tail timestamps)) nil t))
   ;; An omitted legacy SINCE means Kubed's historical no-cutoff behavior.
   ;; Passing it to Stern would silently apply Stern's 48-hour default instead.
   ((or limit (null since)
        (not (executable-find kubed-ext-stern-program)))
    (kubed-ext--call-original-kubed-logs type resource context namespace container follow limit prefix since tail timestamps))
   (t
    (kubed-ext--action-scope type resource context namespace)
    (kubed-ext--open-log-spec
     (list :type type :query resource :context context :namespace namespace :container container
           :follow follow :prefix prefix :since since :tail tail :timestamps timestamps)))))
(defun kubed-ext-logs-for-pod
    (pod &optional context namespace container follow limit prefix since tail timestamps)
  "Show POD logs with CONTEXT/NAMESPACE and upstream-compatible log options."
  (interactive (let ((s (kubed-ext--action-selected)))
                 (list (plist-get s :name) (plist-get s :context) (plist-get s :namespace)
                       kubed-ext--read-container-marker t nil t)))
  (kubed-ext-stern-logs "pods" pod context namespace container follow limit prefix since tail timestamps))
(defun kubed-ext-filter-log-buffer (pattern)
  "Run Emacs `occur' using Emacs regexp PATTERN, not a live RE2 filter."
  (interactive "sOccur pattern (Emacs regexp): ") (occur pattern))
(defun kubed-ext--logs-scope (scope &optional filter)
  "Stream logs for exact SCOPE with optional RE2 FILTER."
  (kubed-ext--at-scope scope
    (kubed-ext--open-log-spec
     (append (list :query (plist-get scope :name) :selector nil :follow t :prefix t :filter filter)
             scope))))
(defun kubed-ext-pods-logs-errors (click)
  "Stream error logs for the exact pod selected at CLICK."
  (interactive (list last-nonmenu-event))
  (kubed-ext--logs-scope (kubed-ext--action-selected click) kubed-ext-log-error-pattern))
(defalias 'kubed-ext-deployments-logs-errors #'kubed-ext-pods-logs-errors)
(defun kubed-ext-pods-logs-custom-filter (click)
  "Stream selected CLICK resource with a prompted Go/RE2 inclusion filter."
  (interactive (list last-nonmenu-event))
  (kubed-ext--logs-scope (kubed-ext--action-selected click)
                        (read-string "Include pattern (Go/RE2): " kubed-ext-log-error-pattern)))
(defalias 'kubed-ext-deployments-logs-custom-filter #'kubed-ext-pods-logs-custom-filter)
(defun kubed-ext-logs-errors (&optional context namespace)
  "Show error logs for selected or explicitly prompted resource in scope."
  (interactive)
  (let ((s (kubed-ext--action-selected)))
    (when context (setq s (plist-put s :context context)))
    (when namespace (setq s (plist-put s :namespace namespace)))
    (kubed-ext--logs-scope s kubed-ext-log-error-pattern)))
(defun kubed-ext-logs-by-label (&optional context namespace)
  "Stream one Stern selector query in CONTEXT/NAMESPACE."
  (interactive)
  (let* ((s (kubed-ext--action-selected-scope))
         (selector (read-string "Label selector: " (plist-get s :selector))))
    (when (string-empty-p (string-trim selector)) (user-error "Selector cannot be empty"))
    (kubed-ext--open-log-spec (list :type "pods" :selector selector :follow t :prefix t
                                   :context (or context (plist-get s :context))
                                   :namespace (or namespace (plist-get s :namespace))))))
(defun kubed-ext-logs-by-label-filtered (&optional context namespace)
  "Stream a single selector query with a Go/RE2 include filter."
  (interactive)
  (let* ((s (kubed-ext--action-selected-scope)) (selector (read-string "Label selector: " (plist-get s :selector)))
         (filter (read-string "Include pattern (Go/RE2): " kubed-ext-log-error-pattern)))
    (when (string-empty-p (string-trim selector)) (user-error "Selector cannot be empty"))
    (kubed-ext--open-log-spec (list :type "pods" :selector selector :filter filter :follow t :prefix t
                                   :context (or context (plist-get s :context))
                                   :namespace (or namespace (plist-get s :namespace))))))
(defalias 'kubed-ext-logs-parallel-by-label #'kubed-ext-logs-by-label)
(defun kubed-ext-deployments-logs-parallel (click)
  "Stream all containers of workload at CLICK in one Stern session."
  (interactive (list last-nonmenu-event)) (kubed-ext--logs-scope (kubed-ext--action-selected click)))
(defun kubed-ext-logs-parallel-for-workload (&optional context namespace)
  "Stream the selected workload's containers in CONTEXT/NAMESPACE."
  (interactive)
  (let ((s (kubed-ext--action-selected)))
    (when context (setq s (plist-put s :context context)))
    (when namespace (setq s (plist-put s :namespace namespace))) (kubed-ext--logs-scope s)))
(defun kubed-ext-logs-marked ()
  "Stream marked pods with one exact alternation query and one process."
  (interactive)
  (let* ((items (kubed-ext--action-marked-scopes)) (first (car items)))
    (unless items (user-error "No marked pods"))
    (unless (cl-every (lambda (s) (and (member (plist-get s :type) '("pods" "pod"))
                                      (equal (plist-get s :context) (plist-get first :context))
                                      (equal (plist-get s :namespace) (plist-get first :namespace)))) items)
      (user-error "Marked logs require pods in one context and namespace"))
    (kubed-ext--open-log-spec (list :type "pods" :pods (mapcar (lambda (s) (plist-get s :name)) items)
                                   :context (plist-get first :context) :namespace (plist-get first :namespace)
                                   :follow t :prefix t))))
(defun kubed-ext-logs-init-container (&optional context namespace)
  "Discover and stream an init container of the selected pod."
  (interactive)
  (let* ((s (kubed-ext--action-selected)) (pod (plist-get s :name))
         (ctx (or context (plist-get s :context))) (ns (or namespace (plist-get s :namespace))))
    (unless (member (plist-get s :type) '("pod" "pods")) (user-error "Select a pod"))
    (kubed-ext--read-pod-container-async pod "Init container" ctx ns
                                        (lambda (c) (kubed-ext-logs-for-pod pod ctx ns c t nil t)) t t)))
(defun kubed-ext-pods-logs-init-container (click)
  "Discover init containers for the exact pod at CLICK."
  (interactive (list last-nonmenu-event))
  (let ((s (kubed-ext--action-selected click)))
    (kubed-ext--at-scope s
      (kubed-ext--read-pod-container-async
       (plist-get s :name) "Init container" (plist-get s :context) (plist-get s :namespace)
       (lambda (c) (kubed-ext-logs-for-pod (plist-get s :name) (plist-get s :context) (plist-get s :namespace) c t nil t)) t t))))

(provide 'kubed-ext-logs)
;;; kubed-ext-logs.el ends here
