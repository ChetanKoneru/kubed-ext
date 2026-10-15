;;; kubed-ext-terminals.el --- Container-qualified terminal workflows -*- lexical-binding: t; -*-
;;; Commentary:
;; Discover containers asynchronously before entering an optional terminal.
;;; Code:
(require 'kubed-ext-actions)
(require 'eshell)
(require 'esh-mode)
(require 'term)
(defcustom kubed-ext-pod-shell "/bin/sh" "Shell used inside a pod."
  :type 'string :group 'kubed-ext)
(defvar vterm-shell)
(defvar eshell-buffer-name)
(defvar kubed-ext--terminal-processes (make-hash-table :test 'eq)
  "Authoritative buffer to terminal process ownership.")
(defvar-local kubed-ext--terminal-scope nil)
(defvar-local kubed-ext--terminal-environment nil)
(declare-function vterm "ext:vterm" (&optional buffer-name))
(declare-function eat "ext:eat" (&optional program new-buffer-name))
(declare-function ghostel-exec "ext:ghostel" (buffer program &optional args))

(defun kubed-ext--read-pod-container-async
    (pod prompt context namespace callback &optional init-container guess)
  "Read POD containers asynchronously and pass selection to CALLBACK.
PROMPT labels completion in CONTEXT/NAMESPACE.  INIT-CONTAINER selects init
containers; GUESS accepts a sole returned container without prompting.
The deferred prompt and CALLBACK execute in the originating environment."
  (kubed-ext--resolved context namespace 'default
    (kubed-ext--read-pod-container-resolved pod prompt context namespace callback init-container guess)))
(defun kubed-ext--read-pod-container-resolved
    (pod prompt context namespace callback init-container guess)
  "Discover POD containers under PROMPT in CONTEXT/NAMESPACE.
Pass selection to CALLBACK; INIT-CONTAINER and GUESS select discovery policy."
  (let ((scope (kubed-ext--action-scope "pod" pod context namespace))
        (environment (kubed-ext--capture-environment)) (origin (current-buffer)))
    (kubed-ext--async-kubectl
     (append (list (kubed-ext--kubectl-program) "get" "pod" pod "-o"
                   (if init-container "jsonpath={.spec.initContainers[*].name}"
                     "jsonpath={.spec.containers[*].name}"))
             (kubed-ext--action-scope-args scope))
     (lambda (output)
       (let ((containers (split-string output "[[:space:]]+" t)))
         (if (not containers)
             (kubed-ext--show-action-output "container discovery" "No matching containers in the selected pod" t)
           (kubed-ext--defer-action-prompt
            environment origin
            (lambda ()
              (funcall callback
                       (if (and guess (= (length containers) 1)) (car containers)
                         (completing-read (concat prompt ": ") containers nil t))))))))
     (lambda (error) (kubed-ext--show-action-output "container discovery error" error t)))))

(defun kubed-ext--kubectl-exec-args (pod container context namespace &optional shell tty)
  "Build exec argv for POD/CONTAINER in CONTEXT/NAMESPACE using SHELL.
TTY explicitly requests interactive stdin and a terminal; absent TTY is safe
for asynchronous commands and never requests an interactive terminal."
  (append (list (kubed-ext--kubectl-program) "exec" pod)
          (kubed-ext--action-scope-args
           (kubed-ext--action-scope "pod" pod context namespace))
          (when container (list "--container" container))
          (when tty '("-i" "-t"))
          (list "--") (if (listp shell) shell (list (or shell kubed-ext-pod-shell)))))

(defun kubed-ext--kubectl-exec-command (pod container context namespace shell)
  "Return a quoted SHELL command for POD/CONTAINER in CONTEXT/NAMESPACE."
  (mapconcat #'shell-quote-argument
             (kubed-ext--kubectl-exec-args pod container context namespace shell t) " "))

(defun kubed-ext--terminal-name (backend pod container context namespace)
  "Return a BACKEND label for POD/CONTAINER in CONTEXT/NAMESPACE."
  (format "*kubed %s %s/%s [%s/%s]*" backend pod container
          (or context "default") (or namespace "default")))

(defun kubed-ext--terminal-launch (backend pod container context namespace environment)
  "Launch BACKEND for POD/CONTAINER in CONTEXT/NAMESPACE using ENVIRONMENT."
  (let* ((name (generate-new-buffer-name
                (kubed-ext--terminal-name backend pod container context namespace)))
         (argv (kubed-ext--kubectl-exec-args pod container context namespace nil t))
         (command (mapconcat #'shell-quote-argument argv " ")) buffer)
    (kubed-ext--with-environment environment
      (pcase backend
        ('vterm
         (unless (require 'vterm nil t) (user-error "Optional vterm package is not installed"))
         (let ((vterm-shell command)) (setq buffer (vterm name))))
        ('eat
         (unless (require 'eat nil t) (user-error "Optional eat package is not installed"))
         (setq buffer (eat command name)))
        ('ghostel
         (unless (require 'ghostel nil t) (user-error "Optional ghostel package is not installed"))
         (setq buffer (get-buffer-create name))
         (ghostel-exec buffer (car argv) (cdr argv)) (pop-to-buffer buffer))
        ('eshell
         (let ((eshell-buffer-name name)
               (default-directory
                (concat (if-let* ((host (file-remote-p default-directory)))
                            (concat (substring host 0 -1) "|") "/")
                        kubed-tramp-method ":" (kubed--encode-context-name context)
                        "%" namespace "%" pod "%" container ":/")))
           (setq buffer (eshell t))))
        ('ansi-term
         ;; `term' accepts executable plus argv; no shell interpolation needed.
         (setq buffer (apply #'make-term name (car argv) nil (cdr argv)))
         (with-current-buffer buffer (term-mode) (term-char-mode))
         (pop-to-buffer buffer))
        (_ (user-error "Unknown terminal backend: %s" backend))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((remote-directory default-directory))
          (kubed-ext--apply-environment-to-new-buffer environment)
          (when (eq backend 'eshell) (setq-local default-directory remote-directory)))
        (setq-local kubed-ext--terminal-scope
                    (list :type "pod" :name pod :container container :context context :namespace namespace)
                    kubed-ext--terminal-environment environment)
        (puthash buffer (get-buffer-process buffer) kubed-ext--terminal-processes)
        (kubed-ext--own-buffer #'kubed-ext--terminal-stop)))
    buffer))

(defun kubed-ext--terminal-stop ()
  "Stop only the process owned by the current terminal buffer."
  (let ((p (gethash (current-buffer) kubed-ext--terminal-processes)))
    (remhash (current-buffer) kubed-ext--terminal-processes)
    (when (process-live-p p) (delete-process p))))

(defun kubed-ext-terminals-stop-all ()
  "Stop extension-created terminals and pending container prompts."
  (interactive)
  (kubed-ext--cancel-action-prompts)
  (maphash (lambda (buffer process)
             (when (process-live-p process) (delete-process process))
             (when (buffer-live-p buffer)
               (with-current-buffer buffer (setq kubed-ext--terminal-scope nil))))
           kubed-ext--terminal-processes)
  (clrhash kubed-ext--terminal-processes))

(defun kubed-ext--terminal-pod (backend pod context namespace container)
  "Open BACKEND for POD in CONTEXT/NAMESPACE, discovering absent CONTAINER."
  (kubed-ext--resolved context namespace 'default
    (kubed-ext--terminal-pod-resolved backend pod context namespace container)))
(defun kubed-ext--terminal-pod-resolved (backend pod context namespace container)
  "Launch BACKEND for POD/CONTAINER in resolved CONTEXT/NAMESPACE."
  (let ((environment (or kubed-ext--environment (kubed-ext--capture-environment))))
    (if container (kubed-ext--terminal-launch backend pod container context namespace environment)
      (kubed-ext--read-pod-container-async
       pod "Container" context namespace
       (lambda (c) (kubed-ext--terminal-launch backend pod c context namespace environment)) nil t))))

(defun kubed-ext--terminal-interactive ()
  "Return pod terminal arguments from an explicitly selected row."
  (let ((s (kubed-ext--action-selected)))
    (unless (member (plist-get s :type) '("pod" "pods")) (user-error "Select a pod"))
    (list (plist-get s :name) (plist-get s :context) (plist-get s :namespace))))

(defun kubed-ext-vterm-pod (pod &optional context namespace container)
  "Open vterm for POD in CONTEXT/NAMESPACE, discovering CONTAINER."
  (interactive (kubed-ext--terminal-interactive))
  (kubed-ext--terminal-pod 'vterm pod context namespace container))
(defun kubed-ext-eat-pod (pod &optional context namespace container)
  "Open eat for POD in CONTEXT/NAMESPACE, discovering CONTAINER."
  (interactive (kubed-ext--terminal-interactive))
  (kubed-ext--terminal-pod 'eat pod context namespace container))
(defun kubed-ext-ghostel-pod (pod &optional context namespace container)
  "Open Ghostel for POD in CONTEXT/NAMESPACE, discovering CONTAINER."
  (interactive (kubed-ext--terminal-interactive))
  (kubed-ext--terminal-pod 'ghostel pod context namespace container))
(defun kubed-ext-eshell-pod (pod &optional context namespace container)
  "Open eshell at POD/CONTAINER's TRAMP directory in CONTEXT/NAMESPACE."
  (interactive (kubed-ext--terminal-interactive))
  (kubed-ext--terminal-pod 'eshell pod context namespace container))
(defun kubed-ext-ansi-term-pod (pod &optional context namespace container)
  "Open `ansi-term' for POD in CONTEXT/NAMESPACE, discovering CONTAINER."
  (interactive (kubed-ext--terminal-interactive))
  (kubed-ext--terminal-pod 'ansi-term pod context namespace container))

(defun kubed-ext--terminal-at-event (backend event)
  "Open BACKEND for the pod selected by EVENT."
  (let ((s (kubed-ext--action-selected event)))
    (unless (member (plist-get s :type) '("pods" "pod")) (user-error "Select a pod"))
    (kubed-ext--at-scope s
      (kubed-ext--terminal-pod backend (plist-get s :name) (plist-get s :context) (plist-get s :namespace) nil))))
(defun kubed-ext-pods-vterm (click)
  "Open vterm for the pod at CLICK."
  (interactive (list last-nonmenu-event)) (kubed-ext--terminal-at-event 'vterm click))
(defun kubed-ext-pods-eat (click)
  "Open eat for the pod at CLICK."
  (interactive (list last-nonmenu-event)) (kubed-ext--terminal-at-event 'eat click))
(defun kubed-ext-pods-ghostel (click)
  "Open Ghostel for the pod at CLICK."
  (interactive (list last-nonmenu-event)) (kubed-ext--terminal-at-event 'ghostel click))
(defun kubed-ext-pods-eshell (click)
  "Open eshell for the pod at CLICK."
  (interactive (list last-nonmenu-event)) (kubed-ext--terminal-at-event 'eshell click))
(defun kubed-ext-pods-ansi-term (click)
  "Open `ansi-term' for the pod at CLICK."
  (interactive (list last-nonmenu-event)) (kubed-ext--terminal-at-event 'ansi-term click))

(defun kubed-ext-exec-pod (pod command &optional context namespace container)
  "Run non-TTY COMMAND argv in POD/CONTAINER under CONTEXT/NAMESPACE.
When CONTAINER is absent, discover it asynchronously before execution."
  (unless (and (consp command) (cl-every #'stringp command))
    (user-error "Command must be a nonempty list of argument strings"))
  (kubed-ext--resolved context namespace 'default
    (kubed-ext--exec-pod-resolved pod command context namespace container)))
(defun kubed-ext--exec-pod-resolved (pod command context namespace container)
  "Run COMMAND after resolving POD scope once."
  (let ((env (or kubed-ext--environment (kubed-ext--capture-environment))))
    (if (not container)
        (kubed-ext--read-pod-container-async
         pod "Container" context namespace
         (lambda (c) (kubed-ext-exec-pod pod command context namespace c)) nil t)
      (let ((b (kubed-ext--action-output-buffer
                (kubed-ext--terminal-name 'exec pod container context namespace))))
        (with-current-buffer b
          (special-mode)
          (kubed-ext--apply-environment-to-new-buffer env)
          (setq-local kubed-ext--action-scope
                      (list :type "pod" :name pod :container container :context context :namespace namespace
                            :environment env)
                      kubed-ext--action-environment env header-line-format "Exec pending"))
        (display-buffer b)
        (kubed-ext--with-environment env
          (kubed-ext--async-kubectl
           (kubed-ext--kubectl-exec-args pod container context namespace command)
           (lambda (text) (kubed-ext--show-action-output b text))
           (lambda (error) (kubed-ext--show-action-output b error t))))))))

(defun kubed-ext-pods-command (click)
  "Run a prompted non-TTY command in the pod at CLICK."
  (interactive (list last-nonmenu-event))
  (let ((s (kubed-ext--action-selected click)))
    (unless (member (plist-get s :type) '("pod" "pods")) (user-error "Select a pod"))
    (kubed-ext--at-scope s
      (kubed-ext-exec-pod (plist-get s :name)
                          (split-string-and-unquote (read-string "Command (quoted argv): "))
                          (plist-get s :context) (plist-get s :namespace)))))

(provide 'kubed-ext-terminals)
;;; kubed-ext-terminals.el ends here
