;;; kubed-ext.el --- Extensions for Kubed with Async Metrics -*- lexical-binding: t; -*-

;; Author: Chetan Koneru
;; Version: 0.7.0
;; URL: https://github.com/CsBigDataHub/kubed-ext
;; Keywords: tools, kubernetes
;; Package-Requires: ((emacs "29.1") (kubed "0.7.0") (transient "0.13.2"))

;;; Commentary:
;; kubed-ext adds practical Kubernetes dashboard behavior on top of kubed:
;; k9s-style status coloring, metrics buffers, port-forward management,
;; CRD auto-discovery, Strimzi actions, label filtering, batch marking,
;; circuit-breaker handling for broken cluster connections, and terminal/log
;; helpers.
;;
;; The package requires kubed and transient.  Optional integrations are used
;; when installed: vterm, eat, ghostel, kubed-tramp, and stern.  CRD metadata
;; can be cached under `kubed-ext-crd-cache-directory' so custom resources are
;; available quickly after Emacs starts.
;;
;; Loading this file installs kubed-ext keybindings, hooks, and kubed advice.
;; `kubed-ext-unload-function' removes those runtime changes when the feature
;; is unloaded.

;;; Code:

(require 'kubed)
(require 'transient)
(require 'kubed-transient)
(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'ansi-color)
(require 'parse-time)

(defvar tramp-connection-properties)
(defvar kubed-nodes-mode-map)
(defvar kubed-node-prefix-map)
;; Suppress byte-compiler warning for built-in special variable used in cl-letf.
(defvar read-eval)

(defconst kubed-ext--display-buffer-rules
  '(("\\`\\*Kubed [a-z][a-z0-9.-]*[@[]" (display-buffer-same-window))
    ("\\`\\*kubed-top-" (display-buffer-same-window)))
  "Display rules installed by kubed-ext.")

(defconst kubed-ext--supported-kubed-version "0.7.0"
  "Kubed version whose internal `kubed-update' shape kubed-ext overrides.")

(defvar kubed-ext--original-list-mode-line-format nil
  "Original value of `kubed-list-mode-line-format' before kubed-ext changes it.")

(defvar kubed-ext--original-list-mode-line-format-saved nil
  "Non-nil after kubed-ext saves `kubed-list-mode-line-format'.")

(defvar kubed-ext--original-kubed-columns
  (and (boundp 'kubed--columns) (copy-tree kubed--columns t))
  "Original value of upstream `kubed--columns' before kubed-ext changes it.")

(defvar kubed-ext--original-column-variables nil
  "Original generated `kubed-*-columns' variables changed by kubed-ext.")

(defun kubed-ext--snapshot-column-variable (symbol)
  "Remember SYMBOL's current value so unload can restore it."
  (when (and (boundp symbol)
             (not (assq symbol kubed-ext--original-column-variables)))
    (push (cons symbol (copy-tree (symbol-value symbol) t))
          kubed-ext--original-column-variables)))

(defun kubed-ext--snapshot-column-state ()
  "Remember upstream kubed column state before runtime mutations."
  (when (boundp 'kubed--columns)
    (dolist (entry kubed--columns)
      (kubed-ext--snapshot-column-variable
       (intern (format "kubed-%s-columns" (car entry)))))))

(kubed-ext--snapshot-column-state)

(declare-function vterm "ext:vterm" (&optional buffer-name))
(declare-function eat "ext:eat" (&optional program new-buffer-name))
(declare-function ghostel-exec "ghostel" (buffer program &optional args))
(declare-function kubed-tramp-assert-support "kubed-tramp" ())
(declare-function kubed-list-configmaps "kubed" (&optional context namespace))
(declare-function kubed-list-persistentvolumeclaims "kubed"
                  (&optional context namespace))

(defgroup kubed-ext nil
  "Extensions for Kubed."
  :group 'kubed)

;;; ═══════════════════════════════════════════════════════════════
;;; § 0.  Autoload Fixes + Full-Screen List Buffers
;;; ═══════════════════════════════════════════════════════════════

(defalias 'kubed-ext-list-ingresss      #'kubed-list-ingresses)
(defalias 'kubed-ext-list-ingressclasss #'kubed-list-ingressclasses)

(dolist (rule kubed-ext--display-buffer-rules)
  (add-to-list 'display-buffer-alist rule))

;;; ═══════════════════════════════════════════════════════════════
;;; § 0a.  Utility Functions
;;; ═══════════════════════════════════════════════════════════════

(defvar kubed-ext--tracked-timers nil
  "Short-lived timers started by kubed-ext and cancelled on unload.")

(defun kubed-ext--run-at-time-tracked (time repeat function &rest args)
  "Run FUNCTION with ARGS at TIME and REPEAT, tracking timer for unload."
  (let (timer)
    (setq timer
          (run-at-time
           time repeat
           (lambda ()
             (setq kubed-ext--tracked-timers
                   (delq timer kubed-ext--tracked-timers))
             (apply function args))))
    (push timer kubed-ext--tracked-timers)
    timer))

(defun kubed-ext--cancel-tracked-timers ()
  "Cancel all short-lived timers tracked by kubed-ext."
  (dolist (timer kubed-ext--tracked-timers)
    (when (timerp timer)
      (cancel-timer timer)))
  (setq kubed-ext--tracked-timers nil))

(defsubst kubed-ext-none-p (s)
  "Return non-nil if S is nil, empty, or a kubectl placeholder value."
  (or (null s)
      (not (stringp s))
      (string-empty-p s)
      (string= s "<none>")
      (string= s "nil")
      (string-match-p "\\`[ \t]*\'" s)))

(defun kubed-ext--resource-at-event (event)
  "Return the tabulated-list resource ID at EVENT.
Works for mouse clicks, keyboard invocations, and context-menu events.
For mouse events the window and buffer-position are read from the event
itself so point is never moved as a side-effect.  For keyboard events
the resource at the current point position is returned.
Returns nil when no resource is found."
  (if (mouse-event-p event)
      (let* ((start  (event-start event))
             (window (posn-window start))
             (pt     (posn-point  start)))
        (when (and (windowp window) (numberp pt))
          (with-current-buffer (window-buffer window)
            (tabulated-list-get-id pt))))
    ;; Keyboard / non-mouse path — use current point.
    (tabulated-list-get-id)))

;;; ═══════════════════════════════════════════════════════════════
;;; § 0b.  Defcustoms: Status Faces, Output Format, Shell
;;; ═══════════════════════════════════════════════════════════════

(defcustom kubed-ext-status-faces
  '(("Running"                      . success)
    ("Succeeded"                    . shadow)
    ("Completed"                    . (:foreground "yellow"))
    ("Pending"                      . warning)
    ("ContainerCreating"            . warning)
    ("PodInitializing"              . warning)
    ("Unknown"                      . warning)
    ("Terminating"                  . (:foreground "blue"))
    ("Failed"                       . error)
    ("Error"                        . error)
    ("OOMKilled"                    . error)
    ("CrashLoopBackOff"             . error)
    ("ImagePullBackOff"             . error)
    ("ErrImagePull"                 . error)
    ("CreateContainerConfigError"   . error)
    ("InvalidImageName"             . error)
    ("Evicted"                      . error)
    ("RunContainerError"            . error)
    ("StartError"                   . error)
    ("Ready"                        . success)
    ("NotReady"                     . error)
    ("Active"                       . success)
    ("Bound"                        . success)
    ("Released"                     . shadow)
    ("Healthy"                      . success))
  "Association list mapping pod/resource status strings to faces.
Customize this to change colors for different Kubernetes statuses."
  :type '(alist :key-type string :value-type (choice face sexp))
  :group 'kubed-ext)

(defcustom kubed-ext-output-format "yaml"
  "Default output format for resource display buffers (yaml or json)."
  :type '(choice (const "yaml") (const "json"))
  :group 'kubed-ext)

(defcustom kubed-ext-pod-shell "/bin/sh"
  "Shell to run when opening a terminal in a Kubernetes pod."
  :type 'string
  :group 'kubed-ext)

(defcustom kubed-ext-stern-program "stern"
  "Name of the `stern' executable used for Kubernetes logs."
  :type 'string
  :group 'kubed-ext)

(defcustom kubed-ext-production-context-regexp "\\bprod\\b"
  "Regexp matched against a kubectl context name to flag it as production.
When matched the mode line shows the context in red with a ☢ prefix and
suffix.  The default pattern matches the word `prod' but not `preprod'.
Set to nil to disable production highlighting entirely."
  :type '(choice (regexp :tag "Regexp")
                 (const  :tag "Disabled" nil))
  :group 'kubed-ext)

(defcustom kubed-ext-command-log-enabled t
  "Non-nil means log kubectl process invocations to `*kubed-command-log*'."
  :type 'boolean
  :group 'kubed-ext)

(defcustom kubed-ext-force-unsupported-kubed-update-override nil
  "Non-nil means override `kubed-update' on unsupported kubed versions.
The override uses kubed private internals and is validated by default only
against `kubed-ext--supported-kubed-version'.  Set this only after auditing
kubed's internals for the installed version."
  :type 'boolean
  :group 'kubed-ext)

(defcustom kubed-ext-auto-discover-crds t
  "Non-nil means discover cluster CRDs asynchronously for resource switching."
  :type 'boolean
  :group 'kubed-ext)

(defcustom kubed-ext-crd-cache-enabled t
  "Non-nil means persist discovered CRD metadata across Emacs restarts."
  :type 'boolean
  :group 'kubed-ext)

(defcustom kubed-ext-crd-cache-directory
  (expand-file-name ".cache/kubed-ext/crds/" user-emacs-directory)
  "Directory where kubed-ext stores CRD metadata cache files."
  :type 'directory
  :group 'kubed-ext)

(defcustom kubed-ext-crd-discovery-batch-size 25
  "Maximum number of CRD resources to define per idle discovery batch."
  :type 'natnum
  :group 'kubed-ext)

;;; ═══════════════════════════════════════════════════════════════
;;; § 0c.  Buffer-Local Variables
;;; ═══════════════════════════════════════════════════════════════

(defvar-local kubed-ext-resource-filter ""
  "Substring filter for highlighting matching resources in list buffers.")

(defvar-local kubed-ext--header-error nil
  "Current error message to display, or nil if no error.")

(defvar-local kubed-ext--error-overlay nil
  "Overlay used to display error message at top of buffer.")

(defvar-local kubed-ext-list-label-selector nil
  "Active label selector for server-side kubectl filtering.")

(defvar-local kubed-ext--stale-list-buffer-p nil
  "Non-nil means this hidden list buffer should re-render when shown.")

(defvar-local kubed-ext--marked-id-set nil
  "Hash table of currently marked resource IDs in this list buffer.")

(defvar-local kubed-ext--marked-id-order nil
  "Marked resource IDs in current display order.")

(defvar-local kubed-ext--marked-id-set-dirty t
  "Non-nil means the current buffer's mark cache must be rebuilt from rows.")

(defvar-local kubed-ext--filter-highlight-state nil
  "Last filter highlight state as (FILTER . BUFFER-TICK).")

(defvar kubed-ext--list-buffers nil
  "Live kubed list buffers known to kubed-ext.")

(defvar kubed-ext--list-buffer-initial-scan-done nil
  "Non-nil after kubed-ext has scanned for preexisting list buffers.")

(defvar kubed-ext--selector-cache (make-hash-table :test 'equal)
  "Cached label selectors keyed by (EENV TYPE CONTEXT NAMESPACE).")

(defvar kubed-ext--stale-list-buffer-count 0
  "Number of hidden list buffers currently marked stale.")

;; External package variable declarations (suppress byte-compiler warnings).
(defvar vterm-shell)
(defvar vterm-buffer-name)
(defvar eat-buffer-name)
(defvar eshell-buffer-name)

;;; ═══════════════════════════════════════════════════════════════
;;; § 0d.  Namespace Prefetch Cache
;;; ═══════════════════════════════════════════════════════════════

(defvar kubed-ext--namespace-prefetch-contexts (make-hash-table :test 'equal)
  "Context/eenv pairs for which namespace prefetch has been triggered.")

(defun kubed-ext--host-key (&optional directory)
  "Return the remote host identity for DIRECTORY or `default-directory'.
This is the bare `file-remote-p' host, used only where a stable host
identity is needed (e.g. disk cache keys).  For kubed data-cache and
buffer-matching keys use `kubed-ext--eenv-key' instead."
  (file-remote-p (or directory default-directory)))

(defun kubed-ext--eenv-key (&optional buffer)
  "Return kubed 0.7.0's execution-environment key for BUFFER.
This is the cache key kubed uses for `kubed--alist', `kubed-update', and
list-buffer matching: a list of (HOST KUBECTL-PROGRAM BUFFER-LOCALS
EXTRA).  Falls back to a host-only key on older kubed."
  (if (fboundp 'kubed-execution-environment-key)
      (kubed-execution-environment-key buffer)
    (kubed-ext--host-key (and buffer
                              (buffer-local-value 'default-directory buffer)))))

(defun kubed-ext--selector-cache-key (type context namespace eenv)
  "Return the selector-cache key for TYPE/CONTEXT/NAMESPACE/EENV."
  (list eenv type context namespace))

(defun kubed-ext--cache-current-selector ()
  "Store or clear this buffer's label selector in `kubed-ext--selector-cache'."
  (when (derived-mode-p 'kubed-list-mode)
    (let ((key (kubed-ext--selector-cache-key
                kubed-list-type kubed-list-context kubed-list-namespace
                (kubed-ext--eenv-key))))
      (if (and (bound-and-true-p kubed-ext-list-label-selector)
               (not (string-empty-p kubed-ext-list-label-selector)))
          (puthash key kubed-ext-list-label-selector kubed-ext--selector-cache)
        (remhash key kubed-ext--selector-cache)))))

(defun kubed-ext--unregister-current-list-buffer ()
  "Forget the current buffer from kubed-ext's list-buffer registries."
  (setq kubed-ext--list-buffers
        (delq (current-buffer) kubed-ext--list-buffers))
  (when (derived-mode-p 'kubed-list-mode)
    (remhash (kubed-ext--selector-cache-key
              kubed-list-type kubed-list-context kubed-list-namespace
              (kubed-ext--eenv-key))
             kubed-ext--selector-cache)
    (when kubed-ext--stale-list-buffer-p
      (setq kubed-ext--stale-list-buffer-p nil)
      (setq kubed-ext--stale-list-buffer-count
            (max 0 (1- kubed-ext--stale-list-buffer-count))))))

(defun kubed-ext--register-current-list-buffer ()
  "Register the current kubed list buffer for fast hot-path iteration."
  (when (derived-mode-p 'kubed-list-mode)
    (cl-pushnew (current-buffer) kubed-ext--list-buffers :test #'eq)
    (add-hook 'kill-buffer-hook
              #'kubed-ext--unregister-current-list-buffer nil t)
    (kubed-ext--cache-current-selector)))

(defun kubed-ext--scan-existing-list-buffers ()
  "Register kubed list buffers that existed before kubed-ext loaded."
  (unless kubed-ext--list-buffer-initial-scan-done
    (setq kubed-ext--list-buffer-initial-scan-done t)
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (derived-mode-p 'kubed-list-mode)
            (kubed-ext--register-current-list-buffer)))))))

(defun kubed-ext--live-list-buffers ()
  "Return live kubed list buffers and prune dead registry entries."
  (kubed-ext--scan-existing-list-buffers)
  (setq kubed-ext--list-buffers
        (cl-remove-if-not #'buffer-live-p kubed-ext--list-buffers)))

(defun kubed-ext--for-each-list-buffer (fn)
  "Call FN with each known live kubed list buffer as current buffer."
  (dolist (buf (kubed-ext--live-list-buffers))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (derived-mode-p 'kubed-list-mode)
          (funcall fn buf))))))

(defun kubed-ext--cleanup-list-buffer-state ()
  "Remove kubed-ext registry hooks and local cache state from list buffers."
  (kubed-ext--for-each-list-buffer
   (lambda (_buf)
     (remove-hook 'kill-buffer-hook
                  #'kubed-ext--unregister-current-list-buffer t)
     (setq kubed-ext--stale-list-buffer-p nil)
     (setq kubed-ext--marked-id-set nil)
     (setq kubed-ext--marked-id-order nil)
     (setq kubed-ext--marked-id-set-dirty t)
     (setq kubed-ext--filter-highlight-state nil))))

(defun kubed-ext-prefetch-namespaces (&optional context eenv)
  "Asynchronously prefetch namespace list for CONTEXT in EENV.
Does nothing if namespaces are already cached or a fetch is in progress.
This ensures `kubed-read-namespace' completions appear without delay."
  (let ((ctx (kubed-ext--usable-context
              (or context (ignore-errors (kubed-local-context)))))
        (eenv (or eenv (kubed-ext--eenv-key))))
    (when ctx
      (condition-case nil
          (when (and (not (alist-get 'resources
                                     (kubed--alist eenv "namespaces" ctx nil)))
                     (not (process-live-p
                           (alist-get 'process
                                      (kubed--alist eenv "namespaces" ctx nil)))))
            (puthash (list ctx eenv) t kubed-ext--namespace-prefetch-contexts)
            (kubed-update "namespaces" ctx nil eenv))
        (error nil)))))

(defun kubed-ext--prefetch-namespaces-hook ()
  "Prefetch namespaces when a kubed list buffer is created."
  (when (bound-and-true-p kubed-list-context)
    (kubed-ext-prefetch-namespaces kubed-list-context)
    (kubed-ext-discover-crds-async kubed-list-context)))

(add-hook 'kubed-list-mode-hook #'kubed-ext--register-current-list-buffer)
(add-hook 'kubed-list-mode-hook #'kubed-ext--prefetch-namespaces-hook)

(defun kubed-ext--prefetch-after-use-context (&rest _)
  "Prefetch namespaces after `kubed-use-context' change the default context."
  (when-let ((ctx (car-safe (kubed-default-context-and-namespace))))
    ;; Clear prefetch flag so the new context gets a fresh fetch.
    (remhash ctx kubed-ext--namespace-prefetch-contexts)
    (kubed-ext-prefetch-namespaces ctx)
    (kubed-ext-discover-crds-async ctx t)))

(advice-add 'kubed-use-context :after #'kubed-ext--prefetch-after-use-context)

;;; ═══════════════════════════════════════════════════════════════
;;; § 0e.  Auto-Refresh Visible List Buffers
;;; ═══════════════════════════════════════════════════════════════

(defcustom kubed-ext-auto-refresh-interval '(15 . 30)
  "Interval range in seconds for auto-refreshing visible kubed list buffers.

A cons cell (MIN . MAX).  Each cycle picks a random delay between MIN
and MAX seconds before the next refresh.  Set to nil to disable even
when the mode is on."
  :type '(choice (const :tag "Disabled" nil)
                 (cons :tag "Interval range (seconds)"
                   (natnum :tag "Minimum")
                   (natnum :tag "Maximum")))
  :group 'kubed-ext)

(defcustom kubed-ext-auto-refresh-large-list-threshold 1000
  "Do not auto-refresh list buffers with at least this many cached rows.
Manual refresh with `g' still works.  Set to nil to auto-refresh every
visible list regardless of size."
  :type '(choice (const :tag "Never skip large lists" nil)
                 (natnum :tag "Row threshold"))
  :group 'kubed-ext)

(defcustom kubed-ext-server-side-list-resource-regexps
  '("\\`secrets\\'" "\\.[[:alnum:].-]+\\'")
  "Resource type regexps listed with kubectl server-side table output.
This avoids fetching complete objects for large sensitive/custom resources
such as Secrets and CRDs, which can contain thousands of rows."
  :type '(repeat regexp)
  :group 'kubed-ext)

(defvar kubed-ext--auto-refresh-timer nil
  "Timer for auto-refreshing visible kubed list buffers.")

(defun kubed-ext--auto-refresh-random-delay ()
  "Return a random delay in seconds, or nil if auto-refresh is disabled."
  (when (consp kubed-ext-auto-refresh-interval)
    (let ((lo (car kubed-ext-auto-refresh-interval))
          (hi (cdr kubed-ext-auto-refresh-interval)))
      (+ lo (random (max 1 (1+ (- hi lo))))))))


(defun kubed-ext--auto-refresh-schedule ()
  "Schedule the next auto-refresh tick with a random delay."
  (kubed-ext--auto-refresh-cancel-timer)
  (when-let ((delay (kubed-ext--auto-refresh-random-delay)))
    (setq kubed-ext--auto-refresh-timer
          (run-with-timer delay nil #'kubed-ext--auto-refresh-tick))))

(defun kubed-ext--auto-refresh-cancel-timer ()
  "Cancel the pending auto-refresh timer if any."
  (when (timerp kubed-ext--auto-refresh-timer)
    (cancel-timer kubed-ext--auto-refresh-timer)
    (setq kubed-ext--auto-refresh-timer nil)))

;;;###autoload
(define-minor-mode kubed-ext-auto-refresh-mode
  "Periodically refresh visible kubed list buffers.

Refresh interval is randomized between the bounds in
`kubed-ext-auto-refresh-interval' (default 15-30 s).  Only buffers
currently shown in a window are refreshed, and a refresh is skipped
when an update is already in progress or the minibuffer is active."
  :global t
  :lighter " KRef"
  :group 'kubed-ext
  (if kubed-ext-auto-refresh-mode
      (if (consp kubed-ext-auto-refresh-interval)
          (progn
            (kubed-ext--auto-refresh-schedule)
            (message "Kubed auto-refresh enabled (%d-%ds)."
                     (car kubed-ext-auto-refresh-interval)
                     (cdr kubed-ext-auto-refresh-interval)))
        (kubed-ext--auto-refresh-cancel-timer)
        (message "Kubed auto-refresh enabled, but interval is disabled."))
    (kubed-ext--auto-refresh-cancel-timer)
    (message "Kubed auto-refresh disabled.")))

;;; ═══════════════════════════════════════════════════════════════
;;; § 1.  CRD Auto-Column Discovery
;;; ═══════════════════════════════════════════════════════════════

(defvar kubed-ext-enriched-resources (make-hash-table :test 'equal)
  "Track resource type+context pairs already enriched with CRD columns.")

(defvar kubed-ext--crd-column-processes (make-hash-table :test 'equal)
  "Track in-flight CRD column discovery processes.")

(defvar kubed-ext--crd-discovery-state (make-hash-table :test 'equal)
  "Discovery state by Kubernetes context.")

(defvar kubed-ext--crd-resource-registry (make-hash-table :test 'equal)
  "Known CRD resource metadata keyed by context, then resource type.")

(defconst kubed-ext--crd-cache-schema-version 1
  "Schema version for persisted CRD cache files.")

(defvar kubed-ext--crd-cache-loaded-contexts (make-hash-table :test 'equal)
  "Contexts whose CRD cache has been loaded in this Emacs session.")

(defvar kubed-ext--crd-warm-timer nil
  "Timer used to warm the CRD cache after loading kubed-ext.")

(defconst kubed-ext--crd-discovery-jsonpath
  (concat "jsonpath="
          "{range .items[*]}"
          "{.spec.group}{\"\\t\"}"
          "{.spec.names.kind}{\"\\t\"}"
          "{.spec.names.plural}{\"\\t\"}"
          "{.spec.scope}{\"\\t\"}"
          "{range .spec.versions[?(@.storage==true)].additionalPrinterColumns[*]}"
          "{.name}{\"|||\"}{.type}{\"|||\"}{.jsonPath}{\";;;\"}"
          "{end}{\"\\t\"}"
          "{range .spec.additionalPrinterColumns[*]}"
          "{.name}{\"|||\"}{.type}{\"|||\"}{.jsonPath}{\";;;\"}"
          "{end}{\"\\n\"}{end}")
  "Compact jsonpath for CRD discovery without large OpenAPI schemas.")

(defvar kubed-ext--domain-action-bindings nil
  "Dynamic domain action bindings installed into CRD mode maps.
Each entry is (MAP-SYMBOL KEY COMMAND OLD-BINDING) and is used for unload
cleanup.")

(defvar kubed-ext--keybinding-snapshots nil
  "Original upstream keybindings overwritten by kubed-ext.
Each entry is (MAP-SYMBOL KEY COMMAND OLD-BINDING).")

(defun kubed-ext--set-key (map-symbol key command)
  "Bind KEY in MAP-SYMBOL to COMMAND and remember the old binding.
The saved binding lets `kubed-ext-unload-function' restore upstream
kubed behavior instead of merely unsetting keys."
  (when (and (boundp map-symbol) (keymapp (symbol-value map-symbol)))
    (let* ((map (symbol-value map-symbol))
           (old-binding (keymap-lookup map key)))
      (unless (cl-find-if (lambda (entry)
                            (and (eq (nth 0 entry) map-symbol)
                                 (equal (nth 1 entry) key)
                                 (eq (nth 2 entry) command)))
                          kubed-ext--keybinding-snapshots)
        (push (list map-symbol key command old-binding)
              kubed-ext--keybinding-snapshots))
      (keymap-set map key command))))

(defun kubed-ext--crd-name-from-api-resources (resource-plural text)
  "Return CRD name for RESOURCE-PLURAL from api-resources TEXT."
  (catch 'found
    (dolist (line (split-string text "\n" t))
      (when (or (string= line resource-plural)
                (string-match-p
                 (concat "\\`" (regexp-quote resource-plural) "\\.")
                 line))
        (throw 'found (string-trim line))))))

(defun kubed-ext--async-process (name command callback &optional errback)
  "Run COMMAND asynchronously under NAME and call CALLBACK with output."
  (let ((buf (generate-new-buffer (format " *%s*" name))))
    (when (and kubed-ext-command-log-enabled
               (fboundp 'kubed-ext--log-kubectl-command)
               (consp command)
               (string-suffix-p
                "kubectl"
                (file-name-sans-extension
                 (file-name-nondirectory (car command)))))
      (kubed-ext--log-kubectl-command
       (mapconcat #'identity (seq-filter #'stringp command) " ")))
    (make-process
     :name name
     :buffer buf
     :command command
     :noquery t
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (let ((code (process-exit-status proc))
               (output ""))
           (when (buffer-live-p buf)
             (setq output (with-current-buffer buf (buffer-string)))
             (kill-buffer buf))
           (if (zerop code)
               (funcall callback output)
             (when errback
               (funcall errback
                        (string-trim
                         (format "%s exited %d%s"
                                 (car command)
                                 code
                                 (if (string-empty-p (string-trim output))
                                     ""
                                   (concat ": " output)))))))))))))

(defun kubed-ext--crd-context-key (context)
  "Return a stable cache key for CONTEXT."
  (prin1-to-string
   (list :context (or context "")
         :host (file-remote-p default-directory)
         :kubeconfig (kubed-ext--kubeconfig-identity)
         :kubectl (kubed-kubectl-program))))

(defun kubed-ext--crd-context-registry (&optional context create)
  "Return CRD registry table for CONTEXT.
When CREATE is non-nil, create the table if needed."
  (let* ((key (kubed-ext--crd-context-key context))
         (registry (gethash key kubed-ext--crd-resource-registry)))
    (when (and create (not registry))
      (setq registry (make-hash-table :test 'equal))
      (puthash key registry kubed-ext--crd-resource-registry))
    registry))

(defun kubed-ext--clear-crd-context-registry (&optional context)
  "Clear in-memory CRD registry entries for CONTEXT."
  (remhash (kubed-ext--crd-context-key context)
           kubed-ext--crd-resource-registry))

(defun kubed-ext--forget-crd-context-state (&optional context)
  "Forget cached CRD bookkeeping for CONTEXT."
  (let ((key (kubed-ext--crd-context-key context)))
    (remhash key kubed-ext--crd-cache-loaded-contexts)
    (remhash key kubed-ext--crd-discovery-state)))

(defun kubed-ext--get-crd-metadata (type &optional context)
  "Return CRD metadata for TYPE in CONTEXT, or nil."
  (when-let ((registry (kubed-ext--crd-context-registry context)))
    (gethash type registry)))

(defun kubed-ext--crd-safe-symbol (name)
  "Return a readable symbol for CRD field NAME."
  (intern
   (replace-regexp-in-string
    "-+" "-"
    (replace-regexp-in-string
     "\\`-\\|-\\'" ""
     (downcase
      (replace-regexp-in-string "[^[:alnum:]]+" "-" (or name "field")))))))

(defun kubed-ext--crd-storage-version (versions)
  "Return the storage version alist from VERSIONS, or the first version."
  (or (seq-find (lambda (version) (alist-get 'storage version)) versions)
      (car versions)))

(defun kubed-ext--crd-printer-columns (crd)
  "Return the preferred additionalPrinterColumns list for CRD."
  (let* ((spec (alist-get 'spec crd))
         (versions (alist-get 'versions spec))
         (version (and (listp versions)
                       (kubed-ext--crd-storage-version versions))))
    (kubed-ext--normalize-crd-printer-columns
     (kubed-ext--crd-type crd)
     (or (alist-get 'additionalPrinterColumns version)
         (alist-get 'additionalPrinterColumns spec)
         '()))))

(defun kubed-ext--crd-column-width (name)
  "Return a pragmatic display width for CRD printer column NAME."
  (pcase (downcase (or name ""))
    ((or "ready" "status" "phase" "state") 14)
    ("metadata state" 18)
    ("warnings" 24)
    ((or "age" "version") 12)
    ((or "conditions" "message" "reason") 36)
    (_ (max 12 (min 32 (+ 2 (length (or name ""))))))))

(defun kubed-ext--strimzi-kafka-resource-p (resource)
  "Return non-nil when RESOURCE names the Strimzi Kafka CRD."
  (member (downcase (or resource ""))
          '("kafkas" "kafkas.kafka.strimzi.io")))

(defun kubed-ext--crd-column-name (column)
  "Return printer COLUMN name."
  (and (consp column) (alist-get 'name column)))

(defun kubed-ext--crd-column-named-p (column name)
  "Return non-nil when printer COLUMN is named NAME."
  (let ((column-name (kubed-ext--crd-column-name column)))
    (and (stringp column-name)
         (string= (downcase column-name) (downcase name)))))

(defun kubed-ext--crd-has-column-p (columns name)
  "Return non-nil when COLUMNS include NAME."
  (seq-some (lambda (column)
              (kubed-ext--crd-column-named-p column name))
            columns))

(defun kubed-ext--insert-crd-column-before (columns column before-name)
  "Return COLUMNS with COLUMN inserted before BEFORE-NAME when present."
  (let (result inserted)
    (dolist (existing columns)
      (when (and (not inserted)
                 (kubed-ext--crd-column-named-p existing before-name))
        (push column result)
        (setq inserted t))
      (push existing result))
    (unless inserted
      (push column result))
    (nreverse result)))

(defun kubed-ext--normalize-crd-printer-columns (resource columns)
  "Return RESOURCE printer COLUMNS adjusted for known kubectl/k9s parity gaps."
  (let ((columns (seq-remove
                  (lambda (c) (string= (or (kubed-ext--crd-column-name c) "")
                                       "Age"))
                  (or columns '()))))
    (when (and (kubed-ext--strimzi-kafka-resource-p resource)
               (not (kubed-ext--crd-has-column-p columns "Metadata State")))
      (setq columns
            (kubed-ext--insert-crd-column-before
             columns
             '((name . "Metadata State")
               (type . "string")
               (jsonPath . ".status.kafkaMetadataState"))
             "Warnings")))
    columns))

(defun kubed-ext--crd-column-specs (columns &optional resource)
  "Convert CRD printer COLUMNS to `kubed-define-resource' property specs.
RESOURCE enables resource-specific display formatting."
  (delq
   nil
   (mapcar
    (lambda (column)
      (let ((name (alist-get 'name column))
            (json-path (alist-get 'jsonPath column)))
        (unless (or (not (stringp name))
                    (not (stringp json-path))
                    (member-ignore-case name '("name" "age")))
          (list (kubed-ext--crd-safe-symbol name)
                json-path
                (kubed-ext--crd-column-width name)
                nil
                (when (or (kubed-ext--status-column-p name)
                          (kubed-ext--ready-column-p name)
                          (kubed-ext--restart-column-p name)
                          (kubed-ext--warnings-column-p name))
                  (kubed-ext--column-value-formatter-for-resource
                   name nil resource))))))
    columns)))

(defun kubed-ext--crd-type (crd)
  "Return the fully qualified kubectl resource type for CRD."
  (let* ((spec (alist-get 'spec crd))
         (names (alist-get 'names spec))
         (plural (alist-get 'plural names))
         (group (alist-get 'group spec)))
    (when (and (stringp plural) (stringp group))
      (concat plural "." group))))

(defun kubed-ext--crd-display-name (metadata)
  "Return a completion label for CRD METADATA."
  (format "%s  %s  %s"
          (plist-get metadata :kind)
          (plist-get metadata :type)
          (if (plist-get metadata :namespaced) "Namespaced" "Cluster")))

(defun kubed-ext--crd-mode-map-symbol (type)
  "Return the generated Kubed mode-map symbol for resource TYPE."
  (intern (format "kubed-%s-mode-map" type)))

(defun kubed-ext--crd-safe-name-p (value)
  "Return non-nil when VALUE is safe CRD-derived identifier text."
  (and (stringp value)
       (< 0 (length value))
       (<= (length value) 253)
       (string-match-p "\\`[[:alnum:]][[:alnum:].-]*[[:alnum:]]\\'" value)
       (not (string-match-p "\\.\\." value))))

(defun kubed-ext--crd-kind-safe-p (value)
  "Return non-nil when VALUE is a safe Kubernetes kind string."
  (and (stringp value)
       (< 0 (length value))
       (<= (length value) 128)
       (string-match-p "\\`[[:alpha:]][[:alnum:]_]*\\'" value)))

(defun kubed-ext--crd-printer-column-safe-p (column)
  "Return non-nil when CRD printer COLUMN has safe scalar fields."
  (and (consp column)
       (let ((name (alist-get 'name column))
             (json-path (alist-get 'jsonPath column)))
         (and (stringp name)
              (<= (length name) 128)
              (stringp json-path)
              (<= (length json-path) 512)
              (string-prefix-p "." json-path)))))

(defun kubed-ext--crd-cache-entry-valid-p (entry)
  "Return non-nil when ENTRY is a safe compact CRD cache plist."
  (and (listp entry)
       (kubed-ext--crd-safe-name-p (plist-get entry :type))
       (kubed-ext--crd-kind-safe-p (plist-get entry :kind))
       (kubed-ext--crd-safe-name-p (plist-get entry :group))
       (kubed-ext--crd-safe-name-p (plist-get entry :plural))
       (plist-member entry :namespaced)
       (memq (plist-get entry :namespaced) '(nil t))
       (let ((columns (plist-get entry :printer-columns)))
         (and (listp columns)
              (seq-every-p #'kubed-ext--crd-printer-column-safe-p columns)))))

(defun kubed-ext--kubeconfig-identity ()
  "Return the kubeconfig identity used for CRD disk cache keys."
  (let ((kubeconfig (getenv "KUBECONFIG")))
    (if (and (stringp kubeconfig) (not (string-empty-p kubeconfig)))
        kubeconfig
      (expand-file-name "config" "~/.kube/"))))

(defun kubed-ext--crd-cache-key (context)
  "Return disk cache key for CONTEXT."
  (secure-hash
   'sha1
   (prin1-to-string
    (list :context (or context "")
          :host (file-remote-p default-directory)
          :kubeconfig (kubed-ext--kubeconfig-identity)
          :kubectl (kubed-kubectl-program)))))

(defun kubed-ext--crd-cache-file (context)
  "Return CRD cache file path for CONTEXT."
  (expand-file-name
   (concat (kubed-ext--crd-cache-key context) ".el")
   kubed-ext-crd-cache-directory))

(defun kubed-ext--crd-cache-entry-from-crd (crd)
  "Return compact cache entry plist for CRD."
  (when-let* ((type (kubed-ext--crd-type crd))
              (spec (alist-get 'spec crd))
              (names (alist-get 'names spec))
              (kind (alist-get 'kind names))
              (group (alist-get 'group spec))
              (plural (alist-get 'plural names)))
    (let ((entry (list :type type
                       :kind kind
                       :group group
                       :plural plural
                       :namespaced (string= (alist-get 'scope spec) "Namespaced")
                       :printer-columns (kubed-ext--crd-printer-columns crd))))
      (when (kubed-ext--crd-cache-entry-valid-p entry)
        entry))))

(defun kubed-ext--define-crd-entry (context entry)
  "Define or update a Kubed resource from cached CRD ENTRY in CONTEXT."
  (when-let* (((kubed-ext--crd-cache-entry-valid-p entry))
              (type (plist-get entry :type))
              (kind (plist-get entry :kind))
              (group (plist-get entry :group))
              (plural (plist-get entry :plural)))
    (let* ((namespaced (plist-get entry :namespaced))
           (printer-columns (kubed-ext--normalize-crd-printer-columns
                             type (plist-get entry :printer-columns)))
           (resource-symbol (intern type))
           (plural-symbol (intern type))
           (columns (kubed-ext--crd-column-specs printer-columns type))
           (metadata (list :type type
                           :kind kind
                           :group group
                           :plural plural
                           :namespaced namespaced
                           :columns columns)))
      (puthash type metadata (kubed-ext--crd-context-registry context t))
      (if (fboundp (intern (format "kubed-list-%s" type)))
          (kubed-ext-enrich-columns type nil printer-columns)
        (eval `(kubed-define-resource ,resource-symbol
                   ,columns
                 :plural ,plural-symbol
                 :namespaced ,namespaced)
              t))
      (when (fboundp 'kubed-ext--patch-type-timestamp-columns)
        (kubed-ext--patch-type-timestamp-columns type))
      (when (fboundp 'kubed-ext--install-domain-actions)
        (kubed-ext--install-domain-actions type))
      metadata)))

(defun kubed-ext--define-crd-resource (crd &optional context)
  "Define or update a Kubed resource from CRD metadata in CONTEXT."
  (when-let ((entry (kubed-ext--crd-cache-entry-from-crd crd)))
    (kubed-ext--define-crd-entry context entry)))

(defun kubed-ext--sort-crd-cache-entries (entries)
  "Return ENTRIES sorted by fully qualified resource type."
  (sort (copy-sequence entries)
        (lambda (a b)
          (string< (or (plist-get a :type) "")
                   (or (plist-get b :type) "")))))

(defun kubed-ext--crd-cache-fingerprint (entries)
  "Return stable fingerprint for CRD cache ENTRIES."
  (secure-hash 'sha1 (prin1-to-string entries)))

(defun kubed-ext--crd-cache-payload-valid-p (payload context)
  "Return non-nil if PAYLOAD is a valid CRD cache for CONTEXT."
  (and (listp payload)
       (= (or (plist-get payload :schema-version) 0)
          kubed-ext--crd-cache-schema-version)
       (equal (plist-get payload :context) (or context ""))
       (listp (plist-get payload :entries))
       (seq-every-p #'kubed-ext--crd-cache-entry-valid-p
                    (plist-get payload :entries))))

(defun kubed-ext--read-crd-cache-payload (context)
  "Read CRD cache payload for CONTEXT, or nil."
  (when (and kubed-ext-crd-cache-enabled
             (file-readable-p (kubed-ext--crd-cache-file context)))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents (kubed-ext--crd-cache-file context))
          (let ((payload
                 (let ((read-eval nil))
                   (read (current-buffer)))))
            (when (kubed-ext--crd-cache-payload-valid-p payload context)
              payload)))
      (error nil))))

(defun kubed-ext--load-crd-cache (context &optional force)
  "Load persisted CRD cache for CONTEXT.
Non-nil FORCE reloads even when this context was already loaded."
  (when kubed-ext-crd-cache-enabled
    (let ((key (kubed-ext--crd-context-key context)))
      (when (or force (not (gethash key kubed-ext--crd-cache-loaded-contexts)))
        (puthash key t kubed-ext--crd-cache-loaded-contexts)
        (when-let ((payload (kubed-ext--read-crd-cache-payload context)))
          (kubed-ext--clear-crd-context-registry context)
          (dolist (entry (plist-get payload :entries))
            (kubed-ext--define-crd-entry context entry))
          (puthash key 'cached kubed-ext--crd-discovery-state)
          (length (plist-get payload :entries)))))))

(defun kubed-ext--write-crd-cache (context entries)
  "Persist CRD cache ENTRIES for CONTEXT when content changed."
  (when kubed-ext-crd-cache-enabled
    (let* ((context (or context ""))
           (entries (kubed-ext--sort-crd-cache-entries entries))
           (fingerprint (kubed-ext--crd-cache-fingerprint entries))
           (old (kubed-ext--read-crd-cache-payload context)))
      (unless (equal fingerprint (plist-get old :fingerprint))
        (make-directory kubed-ext-crd-cache-directory t)
        (let* ((file (kubed-ext--crd-cache-file context))
               (temp (make-temp-file
                      (expand-file-name "kubed-ext-crds"
                                        kubed-ext-crd-cache-directory)
                      nil ".el"))
               (payload (list :schema-version
                          kubed-ext--crd-cache-schema-version
                          :context context
                          :written-at (current-time-string)
                          :fingerprint fingerprint
                          :entries entries)))
          (unwind-protect
              (progn
                (with-temp-file temp
                  (let ((print-length nil)
                        (print-level nil))
                    (prin1 payload (current-buffer))
                    (terpri (current-buffer))))
                (rename-file temp file t))
            (when (file-exists-p temp)
              (ignore-errors (delete-file temp)))))))))

(defun kubed-ext--parse-crd-printer-columns-compact (text)
  "Parse compact printer column TEXT from CRD discovery jsonpath output."
  (delq
   nil
   (mapcar
    (lambda (chunk)
      (pcase-let ((`(,name ,type ,json-path)
                   (split-string chunk (regexp-quote "|||"))))
        (let ((column `((name . ,name)
                        (type . ,type)
                        (jsonPath . ,json-path))))
          (when (kubed-ext--crd-printer-column-safe-p column)
            column))))
    (split-string (or text "") (regexp-quote ";;;") t))))

(defun kubed-ext--parse-crd-compact-list (text)
  "Parse compact CRD discovery TEXT into cache entry plists."
  (delq
   nil
   (mapcar
    (lambda (line)
      (pcase-let ((`(,group ,kind ,plural ,scope ,storage-columns
                            ,spec-columns . ,_)
                   (split-string line "\t")))
        (let* ((type (and (not (string-empty-p (or plural "")))
                          (not (string-empty-p (or group "")))
                          (concat plural "." group)))
               (printer-columns
                (or (kubed-ext--parse-crd-printer-columns-compact
                     storage-columns)
                    (kubed-ext--parse-crd-printer-columns-compact
                     spec-columns)))
               (entry (list :type type
                            :kind kind
                            :group group
                            :plural plural
                            :namespaced (string= scope "Namespaced")
                            :printer-columns printer-columns)))
          (when (kubed-ext--crd-cache-entry-valid-p entry)
            entry))))
    (split-string text "\n" t))))

(defun kubed-ext--define-crd-entries-async
    (context entries key callback count &optional clear-registry)
  "Define CRD ENTRIES for CONTEXT in small timer batches.
KEY is the discovery-state key.  CALLBACK receives the final COUNT.
When CLEAR-REGISTRY is non-nil, clear CONTEXT registry before defining
this batch sequence."
  (when clear-registry
    (kubed-ext--clear-crd-context-registry context))
  (let ((batch-size (max 1 kubed-ext-crd-discovery-batch-size))
        (remaining entries))
    (dotimes (_ batch-size)
      (when remaining
        (when (kubed-ext--define-crd-entry context (pop remaining))
          (cl-incf count))))
    (if remaining
        (kubed-ext--run-at-time-tracked
         0 nil #'kubed-ext--define-crd-entries-async
         context remaining key callback count nil)
      (puthash key 'done kubed-ext--crd-discovery-state)
      (when callback
        (funcall callback count)))))

(defun kubed-ext--parse-crd-list (json-text)
  "Parse kubectl CRD list JSON-TEXT into CRD item alists."
  (let* ((json-object-type 'alist)
         (json-array-type 'list)
         (obj (json-read-from-string json-text)))
    (or (alist-get 'items obj) '())))

(defun kubed-ext--parse-crd (json-text)
  "Parse one kubectl CRD JSON-TEXT into an alist."
  (let ((json-object-type 'alist)
        (json-array-type 'list))
    (json-read-from-string json-text)))

(defun kubed-ext--expected-crd-discovery-error-p (err)
  "Return non-nil if ERR is an expected CRD discovery startup failure."
  (or (kubed-ext--auth-error-p err)
      (kubed-ext--context-error-string-p err)))

(defun kubed-ext-discover-crds-async (&optional context force callback)
  "Discover CRDs in CONTEXT asynchronously and define Kubed resources.
Non-nil FORCE ignores the cached discovery state.  CALLBACK receives the
number of CRD resources defined or updated."
  (when kubed-ext-auto-discover-crds
    (let ((ctx (kubed-ext--usable-context
                (or context (ignore-errors (kubed-local-context))))))
      (when force
        (kubed-ext--forget-crd-context-state ctx))
      (let* ((key (kubed-ext--crd-context-key ctx))
             ;; Manual force refresh should not synchronously re-define the
             ;; old cache before starting fresh discovery.
             (cache-count (and (not force)
                               (kubed-ext--load-crd-cache ctx)))
             (state (gethash key kubed-ext--crd-discovery-state)))
        (when (or force (not (memq state '(inflight done))))
          (puthash key 'inflight kubed-ext--crd-discovery-state)
          (kubed-ext--async-process
           "kubed-ext-crd-discovery"
           (append (list (kubed-kubectl-program)
                         "get" "customresourcedefinitions.apiextensions.k8s.io"
                         "-o" kubed-ext--crd-discovery-jsonpath)
                   (when ctx (list "--context" ctx)))
           (lambda (output)
             (condition-case err
                 (let ((entries (kubed-ext--sort-crd-cache-entries
                                 (kubed-ext--parse-crd-compact-list output))))
                   (kubed-ext--write-crd-cache ctx entries)
                   (kubed-ext--define-crd-entries-async
                    ctx entries key callback 0 t))
               (error
                (remhash key kubed-ext--crd-discovery-state)
                (message "Kubed-ext CRD discovery parse failed: %s" err))))
           (lambda (err)
             (remhash key kubed-ext--crd-discovery-state)
             (unless (or cache-count
                         (kubed-ext--expected-crd-discovery-error-p err))
               (message "Kubed-ext CRD discovery failed: %s" err)))))))))

(defun kubed-ext-crd-resource-choices (&optional context)
  "Return completion choices for discovered CRD resources in CONTEXT."
  (let (choices)
    (when-let ((registry (kubed-ext--crd-context-registry context)))
      (maphash
       (lambda (type metadata)
         (push (cons (kubed-ext--crd-display-name metadata) type) choices))
       registry))
    (sort choices (lambda (a b) (string< (car a) (car b))))))

(defconst kubed-ext-common-resources
  '(("pods" . "Pods")
    ("deployments" . "Deployments")
    ("services" . "Services")
    ("configmaps" . "ConfigMaps")
    ("secrets" . "Secrets")
    ("ingresses" . "Ingresses")
    ("jobs" . "Jobs")
    ("cronjobs" . "CronJobs")
    ("statefulsets" . "StatefulSets")
    ("daemonsets" . "DaemonSets")
    ("replicasets" . "ReplicaSets")
    ("persistentvolumeclaims" . "PVCs")
    ("persistentvolumes" . "PVs")
    ("nodes" . "Nodes")
    ("namespaces" . "Namespaces")
    ("events" . "Events")
    ("networkpolicies" . "NetworkPolicies")
    ("horizontalpodautoscalers" . "HPAs"))
  "Common resources for quick switching.")

(defconst kubed-ext-cluster-scoped-resources
  '("customresourcedefinitions" "ingressclasses" "namespaces" "nodes"
    "persistentvolumes" "storageclasses")
  "Resource types known to be cluster-scoped without calling kubectl.")

(defun kubed-ext--resource-choices (&optional context)
  "Return completion choices for built-in and discovered CRD resources.
CONTEXT selects which discovered CRDs to include."
  (sort
   (append (mapcar (lambda (resource)
                     (cons (cdr resource) (car resource)))
                   kubed-ext-common-resources)
           (kubed-ext-crd-resource-choices context))
   (lambda (a b) (string< (car a) (car b)))))

(defun kubed-ext--resource-namespaced-p (type context)
  "Return non-nil if TYPE should be listed with a namespace in CONTEXT."
  (if-let ((metadata (kubed-ext--get-crd-metadata type context)))
      (plist-get metadata :namespaced)
    (not (member type kubed-ext-cluster-scoped-resources))))

(defun kubed-ext--defer-minibuffer-callback (callback &rest args)
  "Call CALLBACK with ARGS when the minibuffer is not active."
  (if (active-minibuffer-window)
      (apply #'kubed-ext--run-at-time-tracked 0.2 nil
             #'kubed-ext--defer-minibuffer-callback callback args)
    (apply callback args)))

(defun kubed-ext--read-pod-container-async
    (pod prompt context namespace callback &optional init-container guess)
  "Asynchronously read a container from POD, then call CALLBACK.
PROMPT is passed to `completing-read'.  CONTEXT and NAMESPACE select
the pod.  Non-nil INIT-CONTAINER reads init containers.  Non-nil GUESS
uses the only returned container without prompting."
  (let ((jsonpath (if init-container
                      "jsonpath={.spec.initContainers[*].name}"
                    "jsonpath={.spec.containers[*].name}")))
    (kubed-ext--async-process
     "kubed-ext-read-container"
     (append (list (kubed-kubectl-program) "get" "pod" pod "-o" jsonpath)
             (when namespace (list "--namespace" namespace))
             (when context (list "--context" context)))
     (lambda (output)
       (let ((containers (split-string (string-trim output) " " t)))
         (cond
          ((null containers)
           (message "kubed-ext: pod `%s' has no %scontainers"
                    pod (if init-container "init " "")))
          ((and guess (null (cdr containers)))
           (funcall callback (car containers)))
          (t
           (kubed-ext--run-at-time-tracked
            0 nil
            #'kubed-ext--defer-minibuffer-callback
            (lambda ()
              (funcall callback
                       (completing-read
                        (format-prompt prompt nil) containers nil t))))))))
     (lambda (err)
       (message "kubed-ext: failed to read containers for %s: %s" pod err)))))

(defun kubed-ext-enrich-columns-async (resource-plural context buffer)
  "Asynchronously inject CRD printer columns for RESOURCE-PLURAL in CONTEXT.
Refresh BUFFER if it is still displaying RESOURCE-PLURAL."
  (let ((key (cons resource-plural (or context ""))))
    (unless (gethash key kubed-ext--crd-column-processes)
      (puthash key t kubed-ext--crd-column-processes)
      (kubed-ext--async-process
       "kubed-ext-api-resources"
       (append (list (kubed-kubectl-program)
                     "api-resources" "--no-headers" "-o" "name")
               (when context (list "--context" context)))
       (lambda (api-output)
         (if-let ((crd-name
                   (kubed-ext--crd-name-from-api-resources
                    resource-plural api-output)))
             (kubed-ext--async-process
              "kubed-ext-crd-columns"
              (append (list (kubed-kubectl-program)
                            "get" "crd" crd-name "-o" "json")
                      (when context (list "--context" context)))
              (lambda (crd-output)
                (remhash key kubed-ext--crd-column-processes)
                (condition-case err
                    (let* ((crd (kubed-ext--parse-crd crd-output))
                           (cols (kubed-ext--crd-printer-columns crd)))
                      (when cols
                        (kubed-ext-enrich-columns resource-plural context cols)
                        (kubed-ext--refresh-list-after-column-change
                         buffer resource-plural context 10)))
                  (error
                   (message "kubed-ext CRD column parse failed: %s" err))))
              (lambda (err)
                (remhash key kubed-ext--crd-column-processes)
                (message "kubed-ext CRD column discovery failed: %s" err)))
           (remhash key kubed-ext--crd-column-processes)))
       (lambda (err)
         (remhash key kubed-ext--crd-column-processes)
         (message "kubed-ext api-resources failed: %s" err))))))

(defun kubed-ext-enrich-columns (resource-plural context columns)
  "Inject CRD printer COLUMNS into kubed for RESOURCE-PLURAL in CONTEXT."
  (ignore context)
  (when-let ((cols (kubed-ext--normalize-crd-printer-columns
                    resource-plural columns)))
    (setf (alist-get resource-plural kubed--columns nil nil #'string=)
          (cons '("NAME:.metadata.name")
                (kubed-ext--colorize-fetch-columns-for-resource
                 (mapcar
                  (lambda (c)
                    (cons (format "%s:%s"
                                  (upcase (replace-regexp-in-string
                                           "[ /]" "_" (alist-get 'name c)))
                                  (alist-get 'jsonPath c))
                          nil))
                  cols)
                 resource-plural)))
    (let ((fmt-var (intern (format "kubed-%s-columns" resource-plural))))
      (kubed-ext--snapshot-column-variable fmt-var)
      (when (boundp fmt-var)
        (set fmt-var
             (mapcar (lambda (c)
                       (list (alist-get 'name c)
                             (max (+ 2 (length (alist-get 'name c))) 14)
                             t))
                     cols))))
    t))

(defun kubed-ext--apply-list-format-for-type (type)
  "Apply current kubed display columns for TYPE to the current list buffer."
  (let ((fmt-var (intern (format "kubed-%s-columns" type))))
    (when (and (boundp fmt-var) (symbol-value fmt-var))
      (setq tabulated-list-format
            (apply #'vector
                   (cons kubed-name-column (symbol-value fmt-var))))
      (tabulated-list-init-header))))

(defun kubed-ext--refresh-list-after-column-change (buffer type context attempts)
  "Refresh BUFFER after async column discovery for TYPE in CONTEXT.
ATTEMPTS bounds retries while an upstream kubed update is still running."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (derived-mode-p 'kubed-list-mode)
                 (equal kubed-list-type type)
                 (equal kubed-list-context context))
        (kubed-ext--apply-list-format-for-type type)
        (condition-case nil
            (kubed-list-update t)
          (user-error
           (when (> attempts 0)
             (kubed-ext--run-at-time-tracked
              0.5 nil
              #'kubed-ext--refresh-list-after-column-change
              buffer type context (1- attempts)))))))))

(defun kubed-ext--maybe-enrich-columns (&rest _)
  "Before-advice: lazily discover CRD columns on first list."
  (when-let ((type kubed-list-type))
    (let ((key (cons type (or kubed-list-context ""))))
      (unless (gethash key kubed-ext-enriched-resources)
        (puthash key t kubed-ext-enriched-resources)
        (let ((existing (alist-get type kubed--columns nil nil #'string=)))
          (when (or (null existing) (<= (length existing) 1))
            (kubed-ext-enrich-columns-async
             type kubed-list-context (current-buffer))))))))

(advice-add 'kubed-list-update :before #'kubed-ext--maybe-enrich-columns)

(defun kubed-ext-refresh-crd-columns ()
  "Force re-discovery of CRDs and CRD columns for the current context."
  (interactive)
  (let ((context (kubed-ext--usable-context
                  (ignore-errors (kubed-local-context)))))
    (clrhash kubed-ext-enriched-resources)
    (kubed-ext--forget-crd-context-state context)
    (kubed-ext-discover-crds-async
     context
     t
     (lambda (count)
       (message "CRD cache refreshed: %d resource%s."
                count (if (= count 1) "" "s"))))))

(defun kubed-ext-warm-crd-cache (&optional context)
  "Load cached CRDs for CONTEXT immediately, then refresh them asynchronously."
  (interactive)
  (let ((ctx (kubed-ext--usable-context
              (or context (ignore-errors (kubed-local-context))))))
    (when ctx
      (kubed-ext--load-crd-cache ctx)
      (kubed-ext-discover-crds-async ctx))))

(when (executable-find (kubed-kubectl-program))
  (setq kubed-ext--crd-warm-timer
        (run-at-time 0 nil #'kubed-ext-warm-crd-cache)))

;;; ═══════════════════════════════════════════════════════════════
;;; § 2.  Port-Forward with Auto-Complete
;;; ═══════════════════════════════════════════════════════════════

(defconst kubed-ext--dq (string 34)
  "Double-quote character as a one-char string.")

(defconst kubed-ext--jq-tab
  (concat "{" kubed-ext--dq "\\t" kubed-ext--dq "}")
  "Kubectl jsonpath tab separator token.")

(defconst kubed-ext--jq-nl
  (concat "{" kubed-ext--dq "\\n" kubed-ext--dq "}")
  "Kubectl jsonpath newline separator token.")

(defun kubed-ext--json-patch (key value)
  "Return JSON string {spec:{KEY:VALUE}} for kubectl patch."
  (let ((q kubed-ext--dq))
    (concat "{" q "spec" q ":{" q key q ":" value "}}")))

(defun kubed-ext-read-resource-port (type name prompt &optional context namespace)
  "Read a port for TYPE/NAME with PROMPT in CONTEXT/NAMESPACE.
This avoids synchronous discovery so port-forward prompts never block Emacs."
  (ignore type name context namespace)
  (read-number (format-prompt prompt nil)))

(defun kubed-ext-read-service-port (service prompt &optional context namespace)
  "Read a port for SERVICE with PROMPT in CONTEXT and NAMESPACE.
This avoids synchronous discovery so port-forward prompts never block Emacs."
  (ignore service context namespace)
  (read-number (format-prompt prompt nil)))

(defun kubed-ext-forward-port (type name local-port remote-port context namespace)
  "Forward LOCAL-PORT to REMOTE-PORT of TYPE/NAME in CONTEXT/NAMESPACE."
  (let ((res-spec (pcase type
                    ((or "pod" "pods") name)
                    ((or "service" "services") (concat "svc/" name))
                    (_ (concat (replace-regexp-in-string "s\'" "" type)
                               "/" name))))
        (desc (format "%s/%s %d:%d in %s[%s]"
                      type name local-port remote-port
                      (or namespace "default") (or context "current"))))
    (message "Forwarding localhost:%d -> %s:%d..."
             local-port res-spec remote-port)
    (push (cons desc
                (apply #'start-process
                       "*kubed-port-forward*" nil
                       (kubed-kubectl-program) "port-forward"
                       res-spec (format "%d:%d" local-port remote-port)
                       (append
                        (when namespace (list "-n" namespace))
                        (when context (list "--context" context)))))
          kubed-port-forward-process-alist)
    (kubed-ext-refresh-pf-markers-in-visible-buffers)))

(defun kubed-ext-forward-port-to-pod
    (pod local-port remote-port context namespace)
  "Forward LOCAL-PORT to REMOTE-PORT of pod POD in CONTEXT/NAMESPACE."
  (interactive
   (let* ((c (kubed-local-context))
          (c (if (equal current-prefix-arg '(16))
                 (kubed-read-context "Context" c) c))
          (n (kubed--namespace c current-prefix-arg))
          (p (kubed-read-pod "Forward port to pod" nil nil c n))
          (r (kubed-ext-read-resource-port "pods" p "Remote port" c n))
          (l (read-number (format "Local port (remote=%d): " r) r)))
     (list p l r c n)))
  (kubed-ext-forward-port "pods" pod local-port remote-port context namespace))

(defun kubed-ext-pods-forward-port (click)
  "Forward local network port to remote port of pod at CLICK position."
  (interactive (list last-nonmenu-event) kubed-pods-mode)
  (if-let ((pod (kubed-ext--resource-at-event click)))
      (let* ((r (kubed-ext-read-resource-port
                 "pods" pod "Remote port"
                 kubed-list-context kubed-list-namespace))
             (l (read-number (format "Local port (remote=%d): " r) r)))
        (kubed-ext-forward-port "pods" pod l r
                                kubed-list-context kubed-list-namespace))
    (user-error "No Kubernetes pod at point")))

(defun kubed-ext-forward-port-to-service
    (service local-port remote-port context namespace)
  "Forward LOCAL-PORT to REMOTE-PORT of service SERVICE in CONTEXT/NAMESPACE."
  (interactive
   (let* ((c (kubed-local-context))
          (c (if (equal current-prefix-arg '(16))
                 (kubed-read-context "Context" c) c))
          (n (kubed--namespace c current-prefix-arg))
          (s (kubed-read-service "Forward port to service" nil nil c n))
          (r (kubed-ext-read-service-port s "Remote port" c n))
          (l (read-number (format "Local port (remote=%d): " r) r)))
     (list s l r c n)))
  (kubed-ext-forward-port "services" service local-port remote-port
                          context namespace))

(defun kubed-ext-services-forward-port (click)
  "Forward local port to Kubernetes service at CLICK position."
  (interactive (list last-nonmenu-event) kubed-services-mode)
  (if-let ((service (kubed-ext--resource-at-event click)))
      (let* ((r (kubed-ext-read-service-port
                 service "Remote port"
                 kubed-list-context kubed-list-namespace))
             (l (read-number (format "Local port (remote=%d): " r) r)))
        (kubed-ext-forward-port "services" service l r
                                kubed-list-context kubed-list-namespace))
    (user-error "No Kubernetes service at point")))

(defun kubed-ext-forward-port-to-deployment
    (deployment local-port remote-port context namespace)
  "Forward LOCAL-PORT to REMOTE-PORT of DEPLOYMENT in CONTEXT/NAMESPACE."
  (interactive
   (let* ((c (kubed-local-context))
          (c (if (equal current-prefix-arg '(16))
                 (kubed-read-context "Context" c) c))
          (n (kubed--namespace c current-prefix-arg))
          (d (kubed-read-deployment "Forward port to deployment" nil nil c n))
          (r (kubed-ext-read-resource-port "deployments" d "Remote port" c n))
          (l (read-number (format "Local port (remote=%d): " r) r)))
     (list d l r c n)))
  (kubed-ext-forward-port "deployments" deployment local-port remote-port
                          context namespace))

(defun kubed-ext-deployments-forward-port (click)
  "Forward local port to Kubernetes deployment at CLICK position."
  (interactive (list last-nonmenu-event) kubed-deployments-mode)
  (if-let ((deployment (kubed-ext--resource-at-event click)))
      (let* ((r (kubed-ext-read-resource-port
                 "deployments" deployment "Remote port"
                 kubed-list-context kubed-list-namespace))
             (l (read-number (format "Local port (remote=%d): " r) r)))
        (kubed-ext-forward-port "deployments" deployment l r
                                kubed-list-context kubed-list-namespace))
    (user-error "No Kubernetes deployment at point")))

;;; ═══════════════════════════════════════════════════════════════
;;; § 3.  Port-Forward Markers + Port-Forward List Buffer
;;; ═══════════════════════════════════════════════════════════════

(defface kubed-ext-port-forward-face
  '((((background dark))  :background "#1a2a1a")
    (((background light)) :background "#e8f5e8"))
  "Face for resources with active port-forwards.")

(defun kubed-ext-mark-port-forwards (&rest _)
  "Highlight list rows with active port-forwards."
  (when (derived-mode-p 'kubed-list-mode)
    (remove-overlays (point-min) (point-max) 'kubed-pf t)
    (when (kubed-port-forward-process-alist)
      (let ((active (make-hash-table :test 'equal)))
        (dolist (pair (kubed-port-forward-process-alist))
          (when (process-live-p (cdr pair))
            (pcase-let ((`(,resource _ ,namespace ,context)
                         (kubed-ext-parse-pf-descriptor (car pair))))
              (pcase-let ((`(,type ,name)
                           (kubed-ext--split-resource-ref resource)))
                (when (and name
                           (kubed-ext--same-resource-type-p
                            type kubed-list-type)
                           (or (kubed-ext-none-p namespace)
                               (null kubed-list-namespace)
                               (string= namespace kubed-list-namespace))
                           (or (kubed-ext-none-p context)
                               (null kubed-list-context)
                               (string= context kubed-list-context)))
                  (puthash name t active))))))
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (when-let ((id (tabulated-list-get-id)))
              (when (gethash id active)
                (let ((ov (make-overlay (line-beginning-position)
                                        (line-end-position))))
                  (overlay-put ov 'kubed-pf t)
                  (overlay-put ov 'face 'kubed-ext-port-forward-face)
                  (overlay-put ov 'help-echo "Port-forwarding active"))))
            (forward-line)))))))

(advice-add 'kubed-list-revert :after #'kubed-ext-mark-port-forwards)

(defun kubed-ext-refresh-pf-markers-in-visible-buffers (&rest _)
  "Refresh port-forward markers in all visible kubed list buffers."
  (dolist (win (window-list))
    (with-current-buffer (window-buffer win)
      (when (derived-mode-p 'kubed-list-mode)
        (kubed-ext-mark-port-forwards)))))

(advice-add 'kubed-stop-port-forward :after
            #'kubed-ext-refresh-pf-markers-in-visible-buffers)

(defun kubed-ext-parse-pf-descriptor (desc)
  "Parse port-forward descriptor DESC into list.
DESC format: type/name local:remote in namespace[context]."
  (if (string-match
       "\\`\\([^ ]+\\) \\([0-9]+:[0-9]+\\) in \\([^[]+\\)\\[\\(.+\\)\\]\\'"
       desc)
      (list (match-string 1 desc) (match-string 2 desc)
            (match-string 3 desc) (match-string 4 desc))
    (list desc "" "" "")))

(defun kubed-ext--split-resource-ref (resource)
  "Return (TYPE NAME) parsed from Kubernetes RESOURCE reference."
  (if (and (stringp resource)
           (string-match "\\`\\([^/]+\\)/\\(.+\\)\\'" resource))
      (list (match-string 1 resource) (match-string 2 resource))
    (list nil resource)))

(defun kubed-ext--resource-type-aliases (type)
  "Return resource type aliases for TYPE."
  (pcase (or type "")
    ((or "pod" "pods") '("pod" "pods"))
    ((or "service" "services" "svc") '("service" "services" "svc"))
    ((or "deployment" "deployments" "deploy") '("deployment" "deployments" "deploy"))
    (_ (list type))))

(defun kubed-ext--same-resource-type-p (left right)
  "Return non-nil if LEFT and RIGHT name the same Kubernetes resource type."
  (and left right
       (not (null (seq-intersection (kubed-ext--resource-type-aliases left)
                                    (kubed-ext--resource-type-aliases right)
                                    #'string=)))))

(defun kubed-ext-port-forward-entries ()
  "Return entries for port-forward list buffer."
  (mapcar (lambda (pair)
            (let* ((desc (car pair))
                   (parsed (kubed-ext-parse-pf-descriptor desc))
                   (status (if (process-live-p (cdr pair))
                               (propertize "Active" 'face 'success)
                             (propertize "Dead" 'face 'error))))
              (list desc (vector (nth 0 parsed) (nth 1 parsed)
                                 (nth 2 parsed) (nth 3 parsed) status))))
          (kubed-port-forward-process-alist)))

(defun kubed-ext-port-forwards-stop (click)
  "Stop port-forward at CLICK position."
  (interactive (list last-nonmenu-event) kubed-ext-port-forwards-mode)
  (if-let ((desc (kubed-ext--resource-at-event click)))
      (when (y-or-n-p (format "Stop port-forward: %s?" desc))
        (kubed-stop-port-forward desc)
        (tabulated-list-print t))
    (user-error "No port-forward at point")))

(defun kubed-ext-port-forwards-stop-all ()
  "Stop all active port-forwards."
  (interactive nil kubed-ext-port-forwards-mode)
  (when (y-or-n-p (format "Stop all %d port-forwards?"
                          (length kubed-port-forward-process-alist)))
    (dolist (pair (kubed-port-forward-process-alist))
      (when (process-live-p (cdr pair))
        (delete-process (cdr pair))))
    (setq kubed-port-forward-process-alist nil)
    (tabulated-list-print t)
    (kubed-ext-refresh-pf-markers-in-visible-buffers)
    (message "All port-forwards stopped.")))

(defun kubed-ext-port-forwards-refresh ()
  "Refresh port-forward list."
  (interactive nil kubed-ext-port-forwards-mode)
  (kubed-port-forward-process-alist)
  (tabulated-list-print t))

(define-derived-mode kubed-ext-port-forwards-mode tabulated-list-mode
  "Kubed PF"
  "Major mode for listing active Kubernetes port-forwards."
  (setq tabulated-list-format
        [("Resource" 32 t) ("Ports" 14 t)
         ("Namespace" 20 t) ("Context" 24 t) ("Status" 8 t)]
        tabulated-list-padding 2
        tabulated-list-entries #'kubed-ext-port-forward-entries)
  (tabulated-list-init-header))

(keymap-set kubed-ext-port-forwards-mode-map "D"
            #'kubed-ext-port-forwards-stop)
(keymap-set kubed-ext-port-forwards-mode-map "K"
            #'kubed-ext-port-forwards-stop-all)
(keymap-set kubed-ext-port-forwards-mode-map "g"
            #'kubed-ext-port-forwards-refresh)
(keymap-set kubed-ext-port-forwards-mode-map "q" #'quit-window)

(defun kubed-ext-list-port-forwards ()
  "Display buffer listing all active port-forwards."
  (interactive)
  (let ((buf (get-buffer-create "*Kubed Port Forwards*")))
    (with-current-buffer buf
      (kubed-ext-port-forwards-mode)
      (tabulated-list-print t))
    (switch-to-buffer buf)))

;;; ═══════════════════════════════════════════════════════════════
;;; § 3a.  Error Overlay Header
;;; ═══════════════════════════════════════════════════════════════

(defun kubed-ext--set-header-error (msg)
  "Set error MSG as an overlay at the top of the current buffer."
  (kubed-ext--clear-header-error)
  (when (and msg (not (string-empty-p (string-trim msg))))
    (setq kubed-ext--header-error (string-trim msg))
    (when (> (buffer-size) 0)
      (let ((ov (make-overlay (point-min) (point-min))))
        (overlay-put ov 'before-string
                     (propertize
                      (format "⚠ %s  [Type `$' for process buffer]\n"
                              kubed-ext--header-error)
                      'face 'error))
        (overlay-put ov 'kubed-ext-error t)
        (overlay-put ov 'priority 100)
        (setq kubed-ext--error-overlay ov)))))

(defun kubed-ext--clear-header-error ()
  "Clear any error overlay from the current buffer."
  (when kubed-ext--error-overlay
    (delete-overlay kubed-ext--error-overlay)
    (setq kubed-ext--error-overlay nil))
  (setq kubed-ext--header-error nil))

(defun kubed-ext--reapply-overlays (&rest _)
  "Re-apply error overlay and filter highlight after buffer revert."
  (when (derived-mode-p 'kubed-list-mode)
    (when kubed-ext--header-error
      (let ((msg kubed-ext--header-error))
        (setq kubed-ext--error-overlay nil)
        (kubed-ext--set-header-error msg)))
    (kubed-ext--apply-filter-highlights)))

(advice-add 'kubed-list-revert :after #'kubed-ext--reapply-overlays)

;;; ═══════════════════════════════════════════════════════════════
;;; § 3b.  Mark / Unmark System (kubel-style)
;;; ═══════════════════════════════════════════════════════════════

(defconst kubed-ext--mark-tags '(?* ?D)
  "First-column tags treated as kubed-ext marks.")

(defun kubed-ext--make-id-set (&optional ids)
  "Return a hash-set initialized with IDS."
  (let ((set (make-hash-table :test 'equal)))
    (dolist (id ids set)
      (puthash id t set))))

(defun kubed-ext--marked-item-tags-with-tags (tags)
  "Return (ID . TAG) pairs whose first-column mark is in TAGS.
Pairs are returned in current buffer display order."
  (let (items)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((tag (char-after)))
          (when (memq tag tags)
            (when-let ((id (tabulated-list-get-id)))
              (push (cons id tag) items))))
        (forward-line)))
    (nreverse items)))

(defun kubed-ext--marked-items-with-tags (tags)
  "Return resource IDs whose first-column mark is any character in TAGS."
  (mapcar #'car (kubed-ext--marked-item-tags-with-tags tags)))

(defun kubed-ext--rebuild-mark-cache ()
  "Rebuild and return the current buffer's marked-ID hash-set."
  (setq kubed-ext--marked-id-order
        (kubed-ext--marked-items-with-tags kubed-ext--mark-tags))
  (setq kubed-ext--marked-id-set
        (kubed-ext--make-id-set kubed-ext--marked-id-order))
  (setq kubed-ext--marked-id-set-dirty nil)
  kubed-ext--marked-id-set)

(defun kubed-ext--mark-cache-items ()
  "Return marked IDs from `kubed-ext--marked-id-set' in display order."
  (seq-filter (lambda (id) (gethash id kubed-ext--marked-id-set))
              kubed-ext--marked-id-order))

(defun kubed-ext--ensure-mark-cache ()
  "Return a fresh marked-ID cache for the current buffer."
  (if (and (hash-table-p kubed-ext--marked-id-set)
           (not kubed-ext--marked-id-set-dirty))
      kubed-ext--marked-id-set
    (kubed-ext--rebuild-mark-cache)))

(defun kubed-ext--mark-cache-put (id)
  "Record ID as marked in the current buffer mark cache.
Rebuilds the cache first when it is stale (e.g. after a refresh) so marks
preserved by `kubed-list-revert' are not lost by the incremental update."
  (when id
    (kubed-ext--ensure-mark-cache)
    (unless (gethash id kubed-ext--marked-id-set)
      (setq kubed-ext--marked-id-order
            (append kubed-ext--marked-id-order (list id))))
    (puthash id t kubed-ext--marked-id-set)
    (setq kubed-ext--marked-id-set-dirty nil)))

(defun kubed-ext--mark-cache-remove (id)
  "Record ID as unmarked in the current buffer mark cache.
Rebuilds the cache first when it is stale (e.g. after a refresh) so marks
preserved by `kubed-list-revert' are not lost by the incremental update."
  (when id
    (kubed-ext--ensure-mark-cache)
    (remhash id kubed-ext--marked-id-set)
    (setq kubed-ext--marked-id-order
          (delete id kubed-ext--marked-id-order))
    (setq kubed-ext--marked-id-set-dirty nil)))

(defun kubed-ext--mark-cache-dirty (&rest _)
  "Mark the current buffer's marked-ID cache dirty."
  (when (derived-mode-p 'kubed-list-mode)
    (setq kubed-ext--marked-id-set-dirty t)))

(defun kubed-ext-marked-items ()
  "Return list of resource IDs marked with `*' or `D' in current buffer."
  (kubed-ext--ensure-mark-cache)
  (kubed-ext--mark-cache-items))

(defun kubed-ext-mark-item ()
  "Mark the resource at point with `*'."
  (interactive nil kubed-list-mode)
  (let ((id (tabulated-list-get-id)))
    (tabulated-list-put-tag
     (propertize "*" 'face 'dired-marked 'help-echo "Selected") t)
    (kubed-ext--mark-cache-put id)))

(defun kubed-ext-unmark-item ()
  "Unmark the resource at point."
  (interactive nil kubed-list-mode)
  (let ((id (tabulated-list-get-id)))
    (tabulated-list-put-tag " " t)
    (kubed-ext--mark-cache-remove id)))

(defun kubed-ext-mark-all ()
  "Mark all visible resources in the current buffer."
  (interactive nil kubed-list-mode)
  (let ((set (kubed-ext--make-id-set)))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let ((id (tabulated-list-get-id)))
          (tabulated-list-put-tag
           (propertize "*" 'face 'dired-marked))
          (puthash id t set))
        (forward-line)))
    (setq kubed-ext--marked-id-set set)
    (setq kubed-ext--marked-id-order
          (kubed-ext--marked-items-with-tags kubed-ext--mark-tags))
    (setq kubed-ext--marked-id-set-dirty nil))
  (message "Marked all items."))

(defun kubed-ext-unmark-all ()
  "Unmark all resources in the current buffer."
  (interactive nil kubed-list-mode)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (memq (char-after) kubed-ext--mark-tags)
        (tabulated-list-put-tag " "))
      (forward-line)))
  (setq kubed-ext--marked-id-set (kubed-ext--make-id-set))
  (setq kubed-ext--marked-id-order nil)
  (setq kubed-ext--marked-id-set-dirty nil)
  (message "Unmarked all items."))

;;; ── Mark Cache Invalidation Across Refreshes ───────────────────────
;;
;; kubed 0.7.0's `kubed-list-revert' preserves mark glyphs itself (it saves
;; each marked row's tag substring before reprinting and re-applies it
;; afterward).  kubed-ext therefore no longer walks every row to persist and
;; restore marks on each refresh — an O(rows) cost that hurt huge lists.
;; Instead we only invalidate the hash-based mark cache so the next batch
;; operation rebuilds it lazily from whatever glyphs survived the revert.

(defun kubed-ext--mark-cache-dirty-after-revert (&rest _)
  "Invalidate this buffer's mark cache after a list refresh.
Installed as `:after' advice on `kubed-list-revert'.  Marks themselves are
preserved by upstream `kubed-list-revert'; this only forces the kubed-ext
mark cache to rebuild lazily on next use."
  (when (derived-mode-p 'kubed-list-mode)
    (setq kubed-ext--marked-id-set-dirty t)))

(advice-add 'kubed-list-revert :after
            #'kubed-ext--mark-cache-dirty-after-revert)
(advice-add 'kubed-list-mark-for-deletion :after
            #'kubed-ext--mark-cache-dirty)
(advice-add 'kubed-list-unmark :after
            #'kubed-ext--mark-cache-dirty)

;;; ═══════════════════════════════════════════════════════════════
;;; § 3c.  Substring Filter with Highlight (kubel-style)
;;; ═══════════════════════════════════════════════════════════════

(defun kubed-ext--entry-matches-filter-p (entry filter)
  "Return non-nil if any cell in ENTRY matches FILTER."
  (and (vectorp entry)
       (cl-loop for i below (length entry)
                thereis (let ((cell (aref entry i)))
                          (and (stringp cell)
                               (string-match-p filter cell))))))

(defun kubed-ext--apply-filter-highlights ()
  "Apply filter highlighting to current buffer.
Matching rows stay normal; non-matching rows get `shadow' face.  If the
filter and buffer contents are unchanged since the last pass, do no work."
  (when (derived-mode-p 'kubed-list-mode)
    (let* ((filter (or (bound-and-true-p kubed-ext-resource-filter) ""))
           (tick (buffer-chars-modified-tick))
           (state (cons filter tick)))
      (unless (equal kubed-ext--filter-highlight-state state)
        (remove-overlays (point-min) (point-max) 'kubed-ext-filter t)
        (when (not (string-empty-p filter))
          (save-excursion
            (let ((block-start nil))
              (goto-char (point-min))
              (while (not (eobp))
                (let* ((entry (tabulated-list-get-entry))
                       (matched (or (null entry)
                                    (kubed-ext--entry-matches-filter-p
                                     entry filter))))
                  (cond
                   ((and matched block-start)
                    (kubed-ext--make-filter-overlay
                     block-start (line-beginning-position))
                    (setq block-start nil))
                   ((and (not matched) (null block-start))
                    (setq block-start (line-beginning-position)))))
                (forward-line))
              (when block-start
                (kubed-ext--make-filter-overlay block-start (point-max))))))
        (setq kubed-ext--filter-highlight-state state)))))

(defun kubed-ext--make-filter-overlay (beg end)
  "Create a filter dimming overlay from BEG to END."
  (when (< beg end)
    (let ((ov (make-overlay beg end)))
      (overlay-put ov 'kubed-ext-filter t)
      (overlay-put ov 'face 'shadow))))

(defun kubed-ext-set-filter (filter)
  "Set substring FILTER for highlighting resources.
Empty string clears the filter."
  (interactive
   (list (read-string (format-prompt "Filter" "clear")
                      kubed-ext-resource-filter))
   kubed-list-mode)
  (setq-local kubed-ext-resource-filter (or filter ""))
  (kubed-ext--apply-filter-highlights)
  (force-mode-line-update)
  (if (string-empty-p kubed-ext-resource-filter)
      (message "Filter cleared.")
    (message "Filter: %s" kubed-ext-resource-filter)))

(defun kubed-ext-jump-to-next-highlight ()
  "Jump to the next resource matching `kubed-ext-resource-filter'."
  (interactive nil kubed-list-mode)
  (unless (and kubed-ext-resource-filter
               (not (string-empty-p kubed-ext-resource-filter)))
    (user-error "No filter set (use `f' to set one)"))
  (let ((found nil) (start (point)))
    (end-of-line)
    (while (and (not found) (not (eobp)))
      (forward-line)
      (when-let ((entry (tabulated-list-get-entry)))
        (when (kubed-ext--entry-matches-filter-p
               entry kubed-ext-resource-filter)
          (setq found t))))
    (unless found
      (goto-char (point-min))
      (while (and (not found) (< (point) start))
        (when-let ((entry (tabulated-list-get-entry)))
          (when (kubed-ext--entry-matches-filter-p
                 entry kubed-ext-resource-filter)
            (setq found t)))
        (unless found (forward-line))))
    (if found
        (beginning-of-line)
      (goto-char start)
      (message "No match for `%s'" kubed-ext-resource-filter))))

(defun kubed-ext-jump-to-previous-highlight ()
  "Jump to the previous resource matching `kubed-ext-resource-filter'."
  (interactive nil kubed-list-mode)
  (unless (and kubed-ext-resource-filter
               (not (string-empty-p kubed-ext-resource-filter)))
    (user-error "No filter set (use `f' to set one)"))
  (let ((found nil) (start (point)))
    (beginning-of-line)
    (while (and (not found) (not (bobp)))
      (forward-line -1)
      (when-let ((entry (tabulated-list-get-entry)))
        (when (kubed-ext--entry-matches-filter-p
               entry kubed-ext-resource-filter)
          (setq found t))))
    (unless found
      (goto-char (point-max))
      (while (and (not found) (> (point) start))
        (forward-line -1)
        (when-let ((entry (tabulated-list-get-entry)))
          (when (kubed-ext--entry-matches-filter-p
                 entry kubed-ext-resource-filter)
            (setq found t)))))
    (if found
        (beginning-of-line)
      (goto-char start)
      (message "No match for `%s'" kubed-ext-resource-filter))))

;;; ═══════════════════════════════════════════════════════════════
;;; § 3d.  Wide View + Output Format Toggle
;;; ═══════════════════════════════════════════════════════════════

(defun kubed-ext--parse-kubectl-table (text)
  "Parse TEXT from kubectl output into (HEADERS . ROWS).
HEADERS is a list of strings, ROWS is a list of lists of strings."
  (let* ((lines (split-string text "\n" t))
         (header-line (car lines))
         (data-lines (cdr lines))
         (starts nil)
         (pos 0))
    (push 0 starts)
    (while (string-match "\\S-\\(\\s-\\{2,\\}\\)\\S-" header-line pos)
      (let ((gap-end (match-end 1)))
        (push gap-end starts)
        (setq pos gap-end)))
    (setq starts (nreverse starts))
    (let* ((parse-line
            (lambda (line)
              (let ((cols nil)
                    (offsets starts))
                (while offsets
                  (let* ((beg (car offsets))
                         (end (if (cdr offsets) (cadr offsets) (length line)))
                         (end (min end (length line)))
                         (str (if (>= beg (length line)) ""
                                (string-trim
                                 (substring line beg end)))))
                    (push str cols))
                  (setq offsets (cdr offsets)))
                (nreverse cols))))
           (headers (funcall parse-line header-line))
           (rows (delq nil
                       (mapcar (lambda (line)
                                 (when (not (string-empty-p
                                             (string-trim line)))
                                   (funcall parse-line line)))
                               data-lines))))
      (cons headers rows))))

(defvar-local kubed-ext-wide--type      nil "Resource type shown in this wide view buffer.")
(defvar-local kubed-ext-wide--context   nil "`kubectl' context used in this wide view buffer.")
(defvar-local kubed-ext-wide--namespace nil "Namespace used in this wide view buffer.")
(defvar-local kubed-ext-wide--name nil "Resource name shown in this wide view buffer, or nil for all.")
(defvar-local kubed-ext-wide--request-token 0
  "Monotonic token used to ignore stale async wide-view responses.")

(defvar-local kubed-ext-display--request-token 0
  "Monotonic token used to ignore stale async `display-buffer' responses.")

(defcustom kubed-ext-wide-deployment-detail-limit 200
  "Maximum number of extra deployment detail rows in wide view.
Set to nil to render every container, selector, and label row."
  :type '(choice (const :tag "Unlimited" nil)
                 (natnum :tag "Maximum rows"))
  :group 'kubed-ext)

(dolist (sym '(kubed-ext-wide--type kubed-ext-wide--context
                                    kubed-ext-wide--namespace kubed-ext-wide--name
                                    kubed-ext-wide--request-token))
  (put sym 'permanent-local t))

(defun kubed-ext--column-header (fetch-spec)
  "Return the display header part of FETCH-SPEC."
  (when (stringp fetch-spec)
    (car (split-string fetch-spec ":" t))))

(defun kubed-ext--status-column-p (header)
  "Return non-nil if HEADER is a Kubernetes status-like column."
  (member (upcase (string-trim (or header "")))
          '("STATUS" "PHASE" "STATE" "REASON" "CONDITION" "HEALTH")))

(defun kubed-ext--ready-column-p (header)
  "Return non-nil if HEADER is a Kubernetes readiness column."
  (string-match-p "\\bREADY\\b"
                  (upcase (replace-regexp-in-string "[-_]" " "
                                                    (or header "")))))

(defun kubed-ext--restart-column-p (header)
  "Return non-nil if HEADER is a Kubernetes restart count column."
  (member (upcase (string-trim (or header ""))) '("RESTARTS" "RESTART")))

(defun kubed-ext--warnings-column-p (header)
  "Return non-nil if HEADER is a warning summary column."
  (string= (upcase (string-trim (or header ""))) "WARNINGS"))

(defun kubed-ext--boolean-list-value-p (value)
  "Return non-nil when VALUE is only comma-separated Kubernetes booleans."
  (let ((parts (split-string (or value "") "," t "[[:space:]\n\r]+")))
    (and parts
         (seq-every-p
          (lambda (part)
            (member (downcase (string-trim part))
                    '("true" "false" "unknown")))
          parts))))

(defun kubed-ext--column-kind (header)
  "Classify column HEADER into a colorize kind symbol.
Return one of `status', `ready', `restart', `warnings', or `plain'.
The cost of header classification is paid once per column so callers can
dispatch per cell with `kubed-ext--colorize-cell-by-kind' without
re-normalizing the header for every row."
  (cond
   ((kubed-ext--status-column-p header) 'status)
   ((kubed-ext--ready-column-p header) 'ready)
   ((kubed-ext--restart-column-p header) 'restart)
   ((kubed-ext--warnings-column-p header) 'warnings)
   (t 'plain)))

(defun kubed-ext--colorize-cell-by-kind (kind value &optional resource)
  "Return VALUE colorized for a column of KIND.
KIND is a symbol from `kubed-ext--column-kind'.  This is the per-cell hot
path: it performs no header classification, only value formatting."
  (let ((value (if (stringp value) value (format "%s" value))))
    (cond
     ((kubed-ext-none-p value)
      (propertize "" 'face 'shadow))
     ((eq kind 'status)
      (let ((face (cdr (assoc value kubed-ext-status-faces))))
        (if face (propertize value 'face face) value)))
     ((eq kind 'ready)
      (cond
       ((string-match "\\`\\([0-9]+\\)/\\([0-9]+\\)\\'" value)
        (let* ((r    (string-to-number (match-string 1 value)))
               (tot  (string-to-number (match-string 2 value)))
               (face (cond ((and (= r 0) (= tot 0)) 'shadow)
                           ((= r tot)               'success)
                           ((= r 0)                 'error)
                           (t                       'warning))))
          (propertize value 'face face 'kubed-sort-value r)))
       ((member value '("True" "true" "Ready"))
        (propertize value 'face 'success))
       ((member value '("False" "false" "NotReady"))
        (propertize value 'face 'error))
       ((member value '("Unknown" "unknown"))
        (propertize value 'face 'warning))
       (t value)))
     ((eq kind 'restart)
      (let* ((n    (string-to-number value))
             (face (cond ((= n 0) nil)
                         ((< n 5) 'warning)
                         (t       'error))))
        (if face
            (propertize value 'face face 'kubed-sort-value n)
          (propertize value 'kubed-sort-value n))))
     ((eq kind 'warnings)
      (if (and (kubed-ext--strimzi-kafka-resource-p resource)
               (kubed-ext--boolean-list-value-p value))
          (propertize "" 'face 'shadow)
        (propertize value 'face 'warning)))
     (t value))))

(defun kubed-ext--colorize-column-value (header value &optional resource)
  "Return VALUE propertized with a face based on HEADER and RESOURCE."
  (kubed-ext--colorize-cell-by-kind
   (kubed-ext--column-kind header) value resource))

(defun kubed-ext--colorize-wide-cell (header value)
  "Return VALUE propertized with a face based on column HEADER name."
  (kubed-ext--colorize-column-value header value))

(defun kubed-ext--column-value-formatter (header formatter)
  "Return a formatter applying FORMATTER then colorizing HEADER value."
  (lambda (value)
    (kubed-ext--colorize-column-value
     header
     (if formatter (funcall formatter value) value))))

(defun kubed-ext--column-value-formatter-for-resource
    (header formatter resource)
  "Return a formatter applying FORMATTER then colorizing HEADER value for RESOURCE."
  (lambda (value)
    (kubed-ext--colorize-column-value
     header
     (if formatter (funcall formatter value) value)
     resource)))

(defun kubed-ext--colorize-fetch-column (column)
  "Attach generic status color formatting to COLUMN when appropriate."
  (let* ((spec (if (consp column) (car column) column))
         (header (kubed-ext--column-header spec)))
    (if (or (kubed-ext--status-column-p header)
            (kubed-ext--ready-column-p header)
            (kubed-ext--restart-column-p header)
            (kubed-ext--warnings-column-p header))
        (cons spec
              (kubed-ext--column-value-formatter
               header (and (consp column) (cdr column))))
      column)))

(defun kubed-ext--colorize-fetch-column-for-resource (column resource)
  "Attach generic status color formatting to COLUMN for RESOURCE."
  (let* ((spec (if (consp column) (car column) column))
         (header (kubed-ext--column-header spec)))
    (if (or (kubed-ext--status-column-p header)
            (kubed-ext--ready-column-p header)
            (kubed-ext--restart-column-p header)
            (kubed-ext--warnings-column-p header))
        (cons spec
              (kubed-ext--column-value-formatter-for-resource
               header (and (consp column) (cdr column)) resource))
      column)))

(defun kubed-ext--colorize-fetch-columns (columns)
  "Attach status color formatting to matching COLUMNS."
  (mapcar #'kubed-ext--colorize-fetch-column columns))

(defun kubed-ext--colorize-fetch-columns-for-resource (columns resource)
  "Attach status color formatting to matching COLUMNS for RESOURCE."
  (mapcar (lambda (column)
            (kubed-ext--colorize-fetch-column-for-resource column resource))
          columns))

;; NOTE: Callees defined BEFORE the caller to satisfy the byte-compiler.

(defun kubed-ext--wide-row-primary-p ()
  "Return non-nil if point is on a primary wide-view row.

In the multi-row deployments wide view, continuation rows use an empty
Name column.  We treat rows with a non-empty Name column as primary."
  (when-let ((ent (tabulated-list-get-entry)))
    (let ((name (aref ent 0)))
      (and (stringp name)
           (not (string-empty-p (string-trim name)))))))

(defun kubed-ext--wide-populate-kubectl-wide-from-output (type raw)
  "Populate current buffer from RAW `kubectl get -o wide' output for TYPE."
  (let* ((parsed (kubed-ext--parse-kubectl-table raw))
         (headers (car parsed))
         (rows (cdr parsed)))
    (unless headers
      (user-error "`kubectl' returned no output for %s" type))
    (let* ((content-widths
            (cl-loop for h in headers
                     for i from 0
                     collect (apply #'max
                                    (string-width h)
                                    4
                                    (mapcar (lambda (row)
                                              (string-width
                                               (or (nth i row) "")))
                                            rows))))
           (min-widths
            (cl-loop for h in headers
                     for i from 0
                     collect (max 6 (min (nth i content-widths)
                                         (max (string-width h) 6)))))
           (available
            (max 20 (- (window-body-width
                        (or (get-buffer-window (current-buffer) t)
                            (selected-window)))
                       2)))
           (padding 2)
           (total (lambda (widths)
                    (+ (apply #'+ widths)
                       (* padding (max 0 (1- (length widths)))))))
           (widths (copy-sequence content-widths)))
      (while (> (funcall total widths) available)
        (let* ((idx (cl-position (apply #'max widths) widths))
               (w (nth idx widths))
               (minw (nth idx min-widths)))
          (if (<= w minw)
              (setq widths nil)
            (setf (nth idx widths) (1- w)))))
      (unless widths
        (setq widths min-widths))
      (setq tabulated-list-format
            (apply #'vector
                   (cl-loop for h in headers
                            for w in widths
                            collect (list h w t)))))
    (setq tabulated-list-entries
          (mapcar (lambda (row)
                    (list (or (car row) "")
                          (apply #'vector
                                 (cl-loop for h in headers
                                          for i from 0
                                          collect (kubed-ext--colorize-wide-cell
                                                   h (or (nth i row) ""))))))
                  rows))
    (setq tabulated-list-padding 2)
    (tabulated-list-init-header)
    (tabulated-list-print t)))

(defun kubed-ext--wide-populate-deployments-from-json (json-str name)
  "Populate a multi-row deployment wide view from JSON-STR.
NAME is non-nil when JSON-STR is for one named deployment."
  (let* ((obj (json-parse-string json-str :object-type 'alist :array-type 'list))
         (items (if name
                    (if (alist-get 'items obj)
                        (alist-get 'items obj)
                      (list obj))
                  (or (alist-get 'items obj) '())))
         (entries nil)
         (detail-count 0)
         (detail-truncated 0)
         (seen-detail (make-hash-table :test 'equal)))
    (setq tabulated-list-padding 2)
    (setq tabulated-list-format
          (vector
           (list "Name" 34 t)
           (list "Ready" 10 t)
           (list "Up-to-date" 12 t)
           (list "Available" 12 t)
           (list "Age" 10 t)
           (list "Detail" 70 t)))
    (dolist (it items)
      (let* ((meta (alist-get 'metadata it))
             (spec (alist-get 'spec it))
             (status (alist-get 'status it))
             (dep-name (or (alist-get 'name meta) ""))
             (labels (alist-get 'labels meta))
             (selector (alist-get 'selector spec))
             (match-labels (alist-get 'matchLabels selector))
             (replicas (number-to-string (or (alist-get 'replicas spec) 0)))
             (ready (number-to-string (or (alist-get 'readyReplicas status) 0)))
             (updated (number-to-string (or (alist-get 'updatedReplicas status) 0)))
             (available (number-to-string (or (alist-get 'availableReplicas status) 0)))
             (creation (or (alist-get 'creationTimestamp meta) ""))
             (age (kubed-ext--format-age creation))
             (ready-str (kubed-ext--format-ready-total ready replicas))
             (updated-str (kubed-ext--format-deployment-count updated replicas))
             (avail-str (kubed-ext--format-deployment-count available replicas))
             (tmpl (alist-get 'template spec))
             (podspec (and tmpl (alist-get 'spec tmpl)))
             (containers (and podspec (alist-get 'containers podspec))))
        (let* ((pairs
                (mapcar (lambda (c)
                          (let ((cname (or (alist-get 'name c) ""))
                                (img (or (alist-get 'image c) "")))
                            (cons cname img)))
                        (or containers '())))
               (summary
                (if (null pairs)
                    ""
                  (mapconcat
                   (lambda (p)
                     (let ((cname (car p))
                           (img (cdr p))
                           (img-face 'font-lock-constant-face))
                       (concat
                        (propertize cname 'face 'shadow)
                        (propertize " → " 'face 'shadow)
                        (propertize img 'face img-face))))
                   pairs
                   (propertize " | " 'face 'shadow)))))
          (push (list dep-name
                      (vector dep-name ready-str updated-str avail-str age summary))
                entries))
        (cl-flet
            ((push-detail
              (detail)
              (when (and (stringp detail) (not (string-empty-p (string-trim detail))))
                (let ((k (cons dep-name detail)))
                  (unless (gethash k seen-detail)
                    (puthash k t seen-detail)
                    (if (and kubed-ext-wide-deployment-detail-limit
                             (>= detail-count
                                 kubed-ext-wide-deployment-detail-limit))
                        (cl-incf detail-truncated)
                      (cl-incf detail-count)
                      (push (list dep-name
                                  (vector "" "" "" "" ""
                                          (propertize detail 'face 'shadow)))
                            entries)))))))
          (dolist (c (or containers '()))
            (let* ((cname (or (alist-get 'name c) ""))
                   (img (or (alist-get 'image c) ""))
                   (img-face 'font-lock-constant-face))
              (push-detail
               (concat
                (propertize "container: " 'face 'shadow)
                (propertize cname 'face 'shadow)
                (propertize "  image: " 'face 'shadow)
                (propertize img 'face img-face)))))
          (when (and match-labels (listp match-labels))
            (dolist (pair match-labels)
              (push-detail (format "selector: %s=%s" (car pair) (cdr pair)))))
          (when (and labels (listp labels))
            (dolist (pair labels)
              (push-detail (format "label: %s=%s" (car pair) (cdr pair))))))))
    (when (> detail-truncated 0)
      (push (list ""
                  (vector "" "" "" "" ""
                          (propertize
                           (format "%d detail row%s hidden; customize `kubed-ext-wide-deployment-detail-limit' to show more"
                                   detail-truncated
                                   (if (= detail-truncated 1) "" "s"))
                           'face 'warning)))
            entries))
    (setq tabulated-list-entries (nreverse entries))
    (tabulated-list-init-header)
    (tabulated-list-print t)))

(defun kubed-ext--wide-populate (type ctx ns &optional name)
  "Fetch and fill current buffer with a wide-format table for TYPE in CTX/NS.

Optional argument NAME limits output to a single named resource.

For deployments, we render a more intuitive multi-row view based on JSON.

This runs `kubectl' asynchronously."
  (setq kubed-ext-wide--type      type
        kubed-ext-wide--context   ctx
        kubed-ext-wide--namespace ns
        kubed-ext-wide--name      name)
  (let ((target (current-buffer))
        (token (cl-incf kubed-ext-wide--request-token)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "Loading %s wide view...\n" type))
      (special-mode))
    (if (string= type "deployments")
        (kubed-ext--async-kubectl
         (append (list "get" "deployments")
                 (when name (list name))
                 (list "-o" "json")
                 (when ns (list "-n" ns))
                 (when ctx (list "--context" ctx)))
         (lambda (json-str)
           (when (buffer-live-p target)
             (with-current-buffer target
               (when (= token kubed-ext-wide--request-token)
                 (let ((inhibit-read-only t))
                   (kubed-ext-wide-mode)
                   (kubed-ext--wide-populate-deployments-from-json
                    json-str name))))))
         (lambda (err)
           (when (buffer-live-p target)
             (with-current-buffer target
               (when (= token kubed-ext-wide--request-token)
                 (let ((inhibit-read-only t))
                   (erase-buffer)
                   (insert (format "Failed to load %s wide view: %s\n"
                                   type err))))))))
      (kubed-ext--async-kubectl
       (append (list "get" type)
               (when name (list name))
               (list "-o" "wide")
               (when ns (list "-n" ns))
               (when ctx (list "--context" ctx)))
       (lambda (raw)
         (when (buffer-live-p target)
           (with-current-buffer target
             (when (= token kubed-ext-wide--request-token)
               (let ((inhibit-read-only t))
                 (kubed-ext-wide-mode)
                 (kubed-ext--wide-populate-kubectl-wide-from-output
                  type raw))))))
       (lambda (err)
         (when (buffer-live-p target)
           (with-current-buffer target
             (when (= token kubed-ext-wide--request-token)
               (let ((inhibit-read-only t))
                 (erase-buffer)
                 (insert (format "Failed to load %s wide view: %s\n"
                                 type err)))))))))))

(defun kubed-ext-list-wide ()
  "Display the current resource list in kubectl wide format with color coding.
When point is on a resource, show only that resource."
  (interactive nil kubed-list-mode)
  (let* ((type kubed-list-type)
         (ctx  kubed-list-context)
         (ns   kubed-list-namespace)
         (name (tabulated-list-get-id))
         (buf  (get-buffer-create
                (format "*Kubed wide %s/%s/%s%s*"
                        type (or ns "cluster") (or ctx "current")
                        (if name (concat "/" name) "")))))
    (pop-to-buffer buf)
    (kubed-ext-wide-mode)
    (let ((inhibit-read-only t))
      (kubed-ext--wide-populate type ctx ns name))))

(defun kubed-ext-wide-describe-resource ()
  "Describe the Kubernetes resource at point in the wide view buffer."
  (interactive nil kubed-ext-wide-mode)
  (unless (kubed-ext--wide-row-primary-p)
    (user-error "No primary resource row at point"))
  (if-let ((name (tabulated-list-get-id)))
      (let* ((type kubed-ext-wide--type)
             (ns   kubed-ext-wide--namespace)
             (ctx  kubed-ext-wide--context))
        (unless type
          (user-error "No resource type context in this buffer"))
        (let ((buf (get-buffer-create
                    (format "*Kubed describe %s/%s/%s/%s*"
                            type name
                            (or ns "default")
                            (or ctx "current")))))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (format "Loading describe output for %s/%s...\n"
                              type name))
              (goto-char (point-min))
              (special-mode)))
          (kubed-ext--async-kubectl
           (append (list "describe" type name)
                   (when ns (list "-n" ns))
                   (when ctx (list "--context" ctx)))
           (lambda (output)
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (let ((inhibit-read-only t))
                   (erase-buffer)
                   (insert output)
                   (goto-char (point-min))
                   (special-mode)))))
           (lambda (err)
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (let ((inhibit-read-only t))
                   (erase-buffer)
                   (insert (format "Failed to describe %s/%s: %s\n"
                                   type name err))
                   (special-mode))))))
          (display-buffer buf)))
    (user-error "No Kubernetes resource at point")))

(defun kubed-ext-wide-fit-column (n)
  "Fit width of Nth table column to its content in wide view.
If N is negative, fit all columns.  Interactively, N is the column
number at point, or the numeric prefix argument if you provide one."
  (interactive
   (list (if current-prefix-arg
             (prefix-numeric-value current-prefix-arg)
           (kubed-list-column-number-at-point)))
   kubed-ext-wide-mode)
  (kubed-list-fit-column-width-to-content n))

(define-derived-mode kubed-ext-wide-mode tabulated-list-mode "Kubed Wide"
  "Major mode for displaying kubectl wide output with color coding.
\\{kubed-ext-wide-mode-map}"
  (setq truncate-lines t)
  (setq-local word-wrap nil)
  (setq-local truncate-string-ellipsis (propertize "…" 'face 'shadow))
  (keymap-set kubed-ext-wide-mode-map "g"   #'kubed-ext-list-wide-refresh)
  (keymap-set kubed-ext-wide-mode-map "q"   #'quit-window)
  (keymap-set kubed-ext-wide-mode-map "b"   #'kubed-ext-switch-buffer)
  (keymap-set kubed-ext-wide-mode-map "<"   #'kubed-ext-wide-fit-column)
  (keymap-set kubed-ext-wide-mode-map "|"   #'kubed-ext-wide-fit-column)
  (keymap-set kubed-ext-wide-mode-map "}"   #'tabulated-list-widen-current-column)
  (keymap-set kubed-ext-wide-mode-map "{"   #'tabulated-list-narrow-current-column)
  (keymap-set kubed-ext-wide-mode-map "?"   #'kubed-ext-transient-menu)
  (keymap-set kubed-ext-wide-mode-map "d"   #'kubed-ext-wide-describe-resource)
  (keymap-set kubed-ext-wide-mode-map "RET" #'kubed-ext-wide-describe-resource))

(defun kubed-ext-list-wide-refresh ()
  "Refresh the wide view buffer by re-running kubectl."
  (interactive nil kubed-ext-wide-mode)
  (unless kubed-ext-wide--type
    (user-error "No resource context; open wide view from a resource list buffer"))
  (message "Refreshing %s wide view..." kubed-ext-wide--type)
  (let ((inhibit-read-only t))
    (kubed-ext--wide-populate
     kubed-ext-wide--type
     kubed-ext-wide--context
     kubed-ext-wide--namespace
     kubed-ext-wide--name)))

(defun kubed-ext-toggle-output-format ()
  "Toggle resource display format between YAML and JSON and revert."
  (interactive)
  (setq kubed-ext-output-format
        (if (string= kubed-ext-output-format "yaml") "json" "yaml"))
  (when (bound-and-true-p kubed-display-resource-mode)
    (revert-buffer))
  (message "Output format: %s" kubed-ext-output-format))

(defun kubed-ext-set-output-format ()
  "Prompt for and set the output format for resource display."
  (interactive)
  (setq kubed-ext-output-format
        (completing-read "Output format: " '("yaml" "json") nil t
                         nil nil kubed-ext-output-format))
  (when (bound-and-true-p kubed-display-resource-mode)
    (revert-buffer))
  (message "Output format set to %s." kubed-ext-output-format))

(defun kubed-ext--display-revert-format (orig-fn &rest args)
  "Around advice for `kubed-display-resource-revert' for configurable format.
ORIG-FN is the original function, ARGS its arguments."
  (if (string= kubed-ext-output-format "yaml")
      (apply orig-fn args)
    (seq-let (type name context namespace)
        kubed-display-resource-info
      (let ((inhibit-read-only t)
            (target (current-buffer))
            (token (cl-incf kubed-ext-display--request-token))
            (format kubed-ext-output-format)
            (info (copy-sequence kubed-display-resource-info)))
        (buffer-disable-undo)
        (erase-buffer)
        (insert (format "Loading %s/%s as %s...\n"
                        type name format))
        (set-buffer-modified-p nil)
        (kubed-ext--async-kubectl
         (append (list "get" type
                       (concat "--output=" format)
                       name)
                 (when namespace (list "-n" namespace))
                 (when context (list "--context" context)))
         (lambda (output)
           (when (buffer-live-p target)
             (with-current-buffer target
               (when (and (= token kubed-ext-display--request-token)
                          (equal info kubed-display-resource-info)
                          (string= format kubed-ext-output-format))
                 (let ((inhibit-read-only t))
                   (erase-buffer)
                   (insert output)
                   (goto-char (point-min))
                   (set-buffer-modified-p nil)
                   (buffer-enable-undo))))))
         (lambda (err)
           (when (buffer-live-p target)
             (with-current-buffer target
               (when (and (= token kubed-ext-display--request-token)
                          (equal info kubed-display-resource-info)
                          (string= format kubed-ext-output-format))
                 (let ((inhibit-read-only t))
                   (erase-buffer)
                   (insert (format "Failed to display Kubernetes resource `%s': %s\n"
                                   name err))
                   (set-buffer-modified-p nil)
                   (buffer-enable-undo)))))))))))

(advice-add 'kubed-display-resource-revert :around
            #'kubed-ext--display-revert-format)

;;; ═══════════════════════════════════════════════════════════════
;;; § 3e.  Batch Operations on Marked Items
;;; ═══════════════════════════════════════════════════════════════

(defun kubed-ext-delete-marked ()
  "Delete all resources marked with `*' or `D'."
  (interactive nil kubed-list-mode)
  (let ((items (kubed-ext-marked-items)))
    (unless items
      (user-error "No resources marked with `*' or `D'"))
    (when (y-or-n-p (format "Delete %d marked %s?"
                            (length items) kubed-list-type))
      (kubed-delete-resources kubed-list-type items
                              kubed-list-context kubed-list-namespace)
      (kubed-list-update t))))

(defun kubed-ext-logs-marked ()
  "Show logs for all pods marked with `*' or `D'."
  (interactive nil kubed-list-mode)
  (unless (member kubed-list-type '("pod" "pods"))
    (user-error "Marked logs are only supported from pod lists"))
  (let ((items (kubed-ext-marked-items)))
    (unless items
      (user-error "No resources marked with `*' or `D'"))
    (dolist (pod items)
      (kubed-logs kubed-list-type pod kubed-list-context kubed-list-namespace
                  t t nil nil nil nil nil))))

;;; ═══════════════════════════════════════════════════════════════
;;; § 4.  Strimzi Operation Helpers
;;; ═══════════════════════════════════════════════════════════════

(defconst kubed-ext-strimzi-reconciliation-types
  '("kafkas" "kafkaconnects" "kafkabridges" "kafkamirrormakers"
    "kafkamirrormaker2s")
  "Strimzi resource plural names that support reconciliation pause.")

(defun kubed-ext--resource-plural-name (type)
  "Return the plural resource name from possibly fully-qualified TYPE."
  (car (split-string type "\\." t)))

(defun kubed-ext--strimzi-type-p (type)
  "Return non-nil when TYPE is a Strimzi CRD resource type."
  (string-suffix-p ".strimzi.io" type))

(defun kubed-ext--strimzi-plural-p (type plurals)
  "Return non-nil if TYPE has a plural name in PLURALS."
  (member (kubed-ext--resource-plural-name type) plurals))

(defun kubed-ext--strimzi-resource-at-point ()
  "Return current Strimzi list context as (TYPE NAME CONTEXT NAMESPACE)."
  (unless (derived-mode-p 'kubed-list-mode)
    (user-error "Not in a Kubed list buffer"))
  (unless (kubed-ext--strimzi-type-p kubed-list-type)
    (user-error "Not a Strimzi resource: %s" kubed-list-type))
  (if-let ((name (tabulated-list-get-id)))
      (list kubed-list-type name kubed-list-context kubed-list-namespace)
    (user-error "No Strimzi resource at point")))

(defun kubed-ext--kubectl-patch-merge-async
    (type name patch context namespace success-message)
  "Patch TYPE/NAME with PATCH asynchronously in CONTEXT/NAMESPACE.
SUCCESS-MESSAGE is formatted with TYPE and NAME after success."
  (kubed-ext--async-kubectl
   (append (list "patch" type name "--type" "merge" "-p" patch)
           (when namespace (list "-n" namespace))
           (when context (list "--context" context)))
   (lambda (_)
     (message success-message type name)
     (when (derived-mode-p 'kubed-list-mode)
       (kubed-list-update)))
   (lambda (err)
     (message "Failed to patch %s/%s: %s" type name err))))

(defun kubed-ext-strimzi-pause-reconciliation
    (type name &optional context namespace)
  "Pause Strimzi reconciliation for resource NAME of TYPE in CONTEXT."
  (interactive
   (let* ((c (kubed-local-context))
          (c (if (equal current-prefix-arg '(16))
                 (kubed-read-context "Context" c) c))
          (n (kubed--namespace c current-prefix-arg))
          (_ (kubed-ext-discover-crds-async c))
          (choices (seq-filter
                    (lambda (choice)
                      (kubed-ext--strimzi-plural-p
                       (cdr choice)
                       kubed-ext-strimzi-reconciliation-types))
                    (kubed-ext-crd-resource-choices c)))
          (label (completing-read "Resource type: " choices nil t))
          (type (alist-get label choices nil nil #'string=))
          (name (kubed-read-resource-name
                 type "Pause reconciliation for" nil nil c n)))
     (list type name c n)))
  (let ((context (or context (kubed-local-context)))
        (namespace (or namespace
                       (kubed--namespace (or context
                                             (kubed-local-context))))))
    (kubed-ext--async-kubectl
     (append (list "annotate" type name
                   "strimzi.io/pause-reconciliation=true" "--overwrite")
             (when namespace (list "-n" namespace))
             (when context (list "--context" context)))
     (lambda (_)
       (message "Paused reconciliation for %s/%s." type name))
     (lambda (err)
       (message "Failed to pause reconciliation for %s/%s: %s"
                type name err)))))

(defun kubed-ext-strimzi-resume-reconciliation
    (type name &optional context namespace)
  "Resume Strimzi reconciliation for resource NAME of TYPE in CONTEXT."
  (interactive
   (let* ((c (kubed-local-context))
          (c (if (equal current-prefix-arg '(16))
                 (kubed-read-context "Context" c) c))
          (n (kubed--namespace c current-prefix-arg))
          (_ (kubed-ext-discover-crds-async c))
          (choices (seq-filter
                    (lambda (choice)
                      (kubed-ext--strimzi-plural-p
                       (cdr choice)
                       kubed-ext-strimzi-reconciliation-types))
                    (kubed-ext-crd-resource-choices c)))
          (label (completing-read "Resource type: " choices nil t))
          (type (alist-get label choices nil nil #'string=))
          (name (kubed-read-resource-name
                 type "Resume reconciliation for" nil nil c n)))
     (list type name c n)))
  (let ((context (or context (kubed-local-context)))
        (namespace (or namespace
                       (kubed--namespace (or context
                                             (kubed-local-context))))))
    (kubed-ext--async-kubectl
     (append (list "annotate" type name
                   "strimzi.io/pause-reconciliation-")
             (when namespace (list "-n" namespace))
             (when context (list "--context" context)))
     (lambda (_)
       (message "Resumed reconciliation for %s/%s." type name))
     (lambda (err)
       (message "Failed to resume reconciliation for %s/%s: %s"
                type name err)))))

(defun kubed-ext-strimzi-restart-connector
    (name &optional context namespace)
  "Trigger restart of Strimzi KafkaConnector NAME in CONTEXT/NAMESPACE."
  (interactive
   (let* ((c (kubed-local-context))
          (c (if (equal current-prefix-arg '(16))
                 (kubed-read-context "Context" c) c))
          (n (kubed--namespace c current-prefix-arg)))
     (list (kubed-read-resource-name
            "kafkaconnectors.kafka.strimzi.io"
            "Restart connector" nil nil c n)
           c n)))
  (let ((context (or context (kubed-local-context)))
        (namespace (or namespace
                       (kubed--namespace (or context
                                             (kubed-local-context))))))
    (kubed-ext--async-kubectl
     (append (list "annotate" "kafkaconnectors.kafka.strimzi.io" name
                   "strimzi.io/restart=true" "--overwrite")
             (when namespace (list "-n" namespace))
             (when context (list "--context" context)))
     (lambda (_)
       (message "Triggered restart of connector %s." name))
     (lambda (err)
       (message "Failed to restart connector %s: %s" name err)))))

(defun kubed-ext-scale-kafkaconnect
    (name replicas &optional context namespace)
  "Scale Strimzi KafkaConnect NAME to REPLICAS in CONTEXT/NAMESPACE."
  (interactive
   (let* ((c (kubed-local-context))
          (c (if (equal current-prefix-arg '(16))
                 (kubed-read-context "Context" c) c))
          (n (kubed--namespace c current-prefix-arg)))
     (list (kubed-read-resource-name
            "kafkaconnects.kafka.strimzi.io"
            "Scale KafkaConnect" nil nil c n)
           (read-number "Number of replicas: ")
           c n)))
  (let* ((context (or context (kubed-local-context)))
         (namespace (or namespace (kubed--namespace context))))
    (kubed-ext--kubectl-patch-merge-async
     "kafkaconnects.kafka.strimzi.io" name
     (kubed-ext--json-patch "replicas" (number-to-string replicas))
     context namespace
     (format "Scaled %%s/%%s to %d replicas." replicas))))

(defun kubed-ext-strimzi-pause-at-point ()
  "Pause reconciliation for the Strimzi resource at point."
  (interactive nil kubed-list-mode)
  (pcase-let ((`(,type ,name ,context ,namespace)
               (kubed-ext--strimzi-resource-at-point)))
    (unless (kubed-ext--strimzi-plural-p
             type kubed-ext-strimzi-reconciliation-types)
      (user-error "Pause reconciliation is not supported for %s" type))
    (kubed-ext-strimzi-pause-reconciliation type name context namespace)))

(defun kubed-ext-strimzi-resume-at-point ()
  "Resume reconciliation for the Strimzi resource at point."
  (interactive nil kubed-list-mode)
  (pcase-let ((`(,type ,name ,context ,namespace)
               (kubed-ext--strimzi-resource-at-point)))
    (unless (kubed-ext--strimzi-plural-p
             type kubed-ext-strimzi-reconciliation-types)
      (user-error "Resume reconciliation is not supported for %s" type))
    (kubed-ext-strimzi-resume-reconciliation type name context namespace)))

(defun kubed-ext-strimzi-restart-connector-at-point ()
  "Restart the Strimzi KafkaConnector at point."
  (interactive nil kubed-list-mode)
  (pcase-let ((`(,type ,name ,context ,namespace)
               (kubed-ext--strimzi-resource-at-point)))
    (unless (string= (kubed-ext--resource-plural-name type) "kafkaconnectors")
      (user-error "Restart is only supported for KafkaConnector resources"))
    (kubed-ext--async-kubectl
     (append (list "annotate" type name
                   "strimzi.io/restart=true" "--overwrite")
             (when namespace (list "-n" namespace))
             (when context (list "--context" context)))
     (lambda (_)
       (message "Triggered restart of %s/%s." type name)
       (kubed-list-update))
     (lambda (err)
       (message "Failed to restart %s/%s: %s" type name err)))))

(defun kubed-ext-strimzi-scale-kafkaconnect-at-point (replicas)
  "Scale the Strimzi KafkaConnect at point to REPLICAS."
  (interactive
   (list (read-number "Number of replicas: "))
   kubed-list-mode)
  (pcase-let ((`(,type ,name ,context ,namespace)
               (kubed-ext--strimzi-resource-at-point)))
    (unless (string= (kubed-ext--resource-plural-name type) "kafkaconnects")
      (user-error "Scale is only supported for KafkaConnect resources"))
    (kubed-ext--kubectl-patch-merge-async
     type name
     (kubed-ext--json-patch "replicas" (number-to-string replicas))
     context namespace
     (format "Scaled %%s/%%s to %d replicas." replicas))))

(defun kubed-ext-strimzi-set-topic-partitions-at-point (partitions)
  "Set the Strimzi KafkaTopic at point to PARTITIONS partitions."
  (interactive
   (list (read-number "Number of partitions: "))
   kubed-list-mode)
  (pcase-let ((`(,type ,name ,context ,namespace)
               (kubed-ext--strimzi-resource-at-point)))
    (unless (string= (kubed-ext--resource-plural-name type) "kafkatopics")
      (user-error "Partitions are only supported for KafkaTopic resources"))
    (kubed-ext--kubectl-patch-merge-async
     type name
     (kubed-ext--json-patch "partitions" (number-to-string partitions))
     context namespace
     (format "Set %%s/%%s to %d partitions." partitions))))

(defun kubed-ext--set-domain-action-key (map-symbol key command)
  "Bind KEY in MAP-SYMBOL to COMMAND and remember it for unload cleanup."
  (let ((map (symbol-value map-symbol))
        (old-binding (keymap-lookup (symbol-value map-symbol) key)))
    (keymap-set map key command)
    (cl-pushnew (list map-symbol key command old-binding)
                kubed-ext--domain-action-bindings
                :test (lambda (a b)
                        (and (eq (nth 0 a) (nth 0 b))
                             (equal (nth 1 a) (nth 1 b))
                             (eq (nth 2 a) (nth 2 b)))))))

(defun kubed-ext--install-strimzi-actions (type)
  "Install Strimzi action keybindings for discovered CRD TYPE."
  (when (kubed-ext--strimzi-type-p type)
    (let ((map-symbol (kubed-ext--crd-mode-map-symbol type))
          (plural (kubed-ext--resource-plural-name type)))
      (when (boundp map-symbol)
        (when (kubed-ext--strimzi-plural-p
               type kubed-ext-strimzi-reconciliation-types)
          (kubed-ext--set-domain-action-key
           map-symbol "P" #'kubed-ext-strimzi-pause-at-point)
          (kubed-ext--set-domain-action-key
           map-symbol "R" #'kubed-ext-strimzi-resume-at-point))
        (pcase plural
          ("kafkaconnects"
           (kubed-ext--set-domain-action-key
            map-symbol "$" #'kubed-ext-strimzi-scale-kafkaconnect-at-point))
          ("kafkaconnectors"
           (kubed-ext--set-domain-action-key
            map-symbol "X" #'kubed-ext-strimzi-restart-connector-at-point))
          ("kafkatopics"
           (kubed-ext--set-domain-action-key
            map-symbol "p"
            #'kubed-ext-strimzi-set-topic-partitions-at-point)))))))

(defun kubed-ext--install-domain-actions (type)
  "Install domain-specific action keybindings for discovered CRD TYPE."
  (kubed-ext--install-strimzi-actions type))

;;; ═══════════════════════════════════════════════════════════════
;;; § 5.  Column Helpers, Metrics, Pod/Deployment Formatting
;;; ═══════════════════════════════════════════════════════════════

(defun kubed-ext-append-columns (resource-plural fetch-columns display-columns)
  "Append extra FETCH-COLUMNS and DISPLAY-COLUMNS to RESOURCE-PLURAL."
  (let ((existing (alist-get resource-plural kubed--columns nil nil #'string=))
        (fmt-var (intern (format "kubed-%s-columns" resource-plural))))
    (kubed-ext--snapshot-column-variable fmt-var)
    (setf (alist-get resource-plural kubed--columns nil nil #'string=)
          (append existing (kubed-ext--colorize-fetch-columns
                            fetch-columns)))
    (when (boundp fmt-var)
      (set fmt-var (append (symbol-value fmt-var) display-columns)))))

(defun kubed-ext-set-columns (resource-plural fetch-columns display-columns)
  "Replace columns for RESOURCE-PLURAL with FETCH-COLUMNS and DISPLAY-COLUMNS."
  (let ((fmt-var (intern (format "kubed-%s-columns" resource-plural))))
    (kubed-ext--snapshot-column-variable fmt-var)
    (setf (alist-get resource-plural kubed--columns nil nil #'string=)
          (cons '("NAME:.metadata.name")
                (kubed-ext--colorize-fetch-columns fetch-columns)))
    (when (boundp fmt-var)
      (set fmt-var display-columns))))

(defun kubed-ext-parse-cpu (s)
  "Parse Kubernetes CPU quantity S to millicores (number)."
  (cond
   ((kubed-ext-none-p s) 0)
   ((string-suffix-p "m" s)  (string-to-number s))
   ((string-suffix-p "n" s)  (/ (string-to-number s) 1000000.0))
   ((string-suffix-p "u" s)  (/ (string-to-number s) 1000.0))
   (t (* 1000.0 (string-to-number s)))))

(defun kubed-ext-parse-mem (s)
  "Parse Kubernetes memory quantity S to MiB (number)."
  (cond
   ((kubed-ext-none-p s) 0)
   ((string-suffix-p "Ti" s) (* 1048576.0 (string-to-number s)))
   ((string-suffix-p "Gi" s) (* 1024.0    (string-to-number s)))
   ((string-suffix-p "Mi" s) (string-to-number s))
   ((string-suffix-p "Ki" s) (/ (string-to-number s) 1024.0))
   (t (/ (string-to-number s) 1048576.0))))

(defun kubed-ext-sum-cpu (s)
  "Sum comma-separated CPU values in S."
  (if (kubed-ext-none-p s) 0
    (apply #'+ (mapcar #'kubed-ext-parse-cpu
                       (split-string s "," t "[ \t]")))))

(defun kubed-ext-sum-mem (s)
  "Sum comma-separated memory values in S."
  (if (kubed-ext-none-p s) 0
    (apply #'+ (mapcar #'kubed-ext-parse-mem
                       (split-string s "," t "[ \t]")))))

(defun kubed-ext-format-cpu (millicores)
  "Format MILLICORES as human-readable CPU string."
  (let ((str (cond
              ((<= millicores 0) "0")
              ((>= millicores 1000) (format "%.1f" (/ millicores 1000.0)))
              (t (format "%dm" (round millicores))))))
    (propertize str 'kubed-sort-value millicores)))

(defun kubed-ext-format-mem (mib)
  "Format MIB as human-readable memory string."
  (let ((str (cond
              ((<= mib 0) "0")
              ((>= mib 1024) (format "%.1fGi" (/ mib 1024.0)))
              (t (format "%.0fMi" mib)))))
    (propertize str 'kubed-sort-value mib)))

(defun kubed-ext-format-pct (used total)
  "Format percentage USED/TOTAL with color coding."
  (if (<= total 0)
      (propertize "n/a" 'face 'shadow 'kubed-sort-value -1)
    (let* ((pct (/ (* 100.0 used) total))
           (str (format "%.0f%%" pct)))
      (propertize str
                  'face (cond ((>= pct 90) 'error)
                              ((>= pct 70) 'warning)
                              (t 'success))
                  'kubed-sort-value pct))))

(defun kubed-ext-top-numeric-sorter (col-idx)
  "Return a sort predicate comparing column COL-IDX by `kubed-sort-value'."
  (lambda (a b)
    (let ((va (get-text-property 0 'kubed-sort-value (aref (cadr a) col-idx)))
          (vb (get-text-property 0 'kubed-sort-value (aref (cadr b) col-idx))))
      (< (or va 0) (or vb 0)))))

;; ── Kubernetes Timestamp ──

(defun kubed-ext--parse-k8s-timestamp (s)
  "Parse Kubernetes RFC3339/ISO8601 timestamp S to an Emacs time value.

Supports the common Kubernetes forms:
- YYYY-MM-DDTHH:MM:SSZ
- YYYY-MM-DDTHH:MM:SS.sssZ
- YYYY-MM-DDTHH:MM:SS±HH:MM

Return nil if S cannot be parsed."
  (when (and (stringp s)
             (not (string-empty-p s))
             ;; Quick guard: must start with YYYY-MM-DDTHH:MM:SS
             (string-match-p
              "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}"
              s))
    (condition-case nil
        (parse-iso8601-time-string s)
      (error nil))))

(defun kubed-ext--format-age (timestamp)
  "Convert TIMESTAMP to human-readable relative age like kubectl.

If TIMESTAMP already carries a `kubed-sort-value' text property (i.e. it
was already formatted by a previous call), return it unchanged.
Returns `n/a' for nil, empty, or placeholder values like `<none>'."
  (cond
   ;; Nil, empty, or kubectl placeholder.
   ((kubed-ext-none-p timestamp)
    (propertize "n/a" 'face 'shadow 'kubed-sort-value 0))
   ;; Already formatted — return as-is (idempotency guard).
   ((and (stringp timestamp)
         (get-text-property 0 'kubed-sort-value timestamp))
    timestamp)
   ;; Try to parse as ISO 8601 timestamp.
   (t
    (let ((parsed (kubed-ext--parse-k8s-timestamp timestamp)))
      (if (null parsed)
          ;; Not a recognizable timestamp — pass through.
          (propertize (if (stringp timestamp) timestamp
                        (format "%s" timestamp))
                      'kubed-sort-value 0)
        (let* ((secs (max 0 (floor (float-time (time-subtract nil parsed)))))
               (total-minutes (/ secs 60))
               (hours (/ secs 3600))
               (days (/ secs 86400))
               (years (/ days 365))
               (str (cond
                     ((< secs 120)
                      (format "%ds" secs))
                     ((< total-minutes 10)
                      (let ((s (mod secs 60)))
                        (if (= s 0) (format "%dm" total-minutes)
                          (format "%dm%ds" total-minutes s))))
                     ((< hours 3)
                      (format "%dm" total-minutes))
                     ((< hours 8)
                      (let ((m (mod total-minutes 60)))
                        (if (= m 0) (format "%dh" hours)
                          (format "%dh%dm" hours m))))
                     ((< hours 48)
                      (format "%dh" hours))
                     ((< days 8)
                      (let ((h (mod hours 24)))
                        (if (= h 0) (format "%dd" days)
                          (format "%dd%dh" days h))))
                     ((< years 2)
                      (format "%dd" days))
                     ((< years 8)
                      (format "%dy%dd" years (mod days 365)))
                     (t (format "%dy" years)))))
          (propertize str 'kubed-sort-value (float secs))))))))

;; ── Pod Status Computation (kubel-style) ──

(defun kubed-ext--compute-pod-status (phase waiting terminated deletion)
  "Compute kubel-style pod status from PHASE, WAITING, TERMINATED, DELETION.
Returns a propertized string with face from `kubed-ext-status-faces'.
Deletion is detected by positive timestamp match."
  (let* ((waiting-reason (unless (kubed-ext-none-p waiting)
                           (car (split-string waiting ","))))
         (terminated-reason (unless (kubed-ext-none-p terminated)
                              (car (split-string terminated ","))))
         (deleting (and (stringp deletion)
                        (string-match-p
                         "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T"
                         deletion)))
         (status (cond
                  (deleting "Terminating")
                  (waiting-reason waiting-reason)
                  (terminated-reason terminated-reason)
                  (t (or phase "Unknown"))))
         (face (cdr (assoc status kubed-ext-status-faces))))
    (if face (propertize status 'face face) status)))

(defun kubed-ext--format-pod-ready (ready-count total-count)
  "Format READY-COUNT/TOTAL-COUNT as kubel-style `x/y' with color."
  (let* ((r (string-to-number (or ready-count "0")))
         (tot (string-to-number (or total-count "0")))
         (str (format "%d/%d" r tot))
         (face (cond
                ((and (= r 0) (= tot 0)) 'shadow)
                ((= r tot)                'success)
                ((> r 0)                  'warning)
                (t                        'error))))
    (propertize str 'face face 'kubed-sort-value r)))

(defun kubed-ext--format-pod-restarts (count)
  "Format pod restart COUNT with color coding."
  (let* ((str (number-to-string count))
         (face (cond
                ((= count 0) nil)
                ((< count 5) 'warning)
                (t 'error))))
    (if face
        (propertize str 'face face 'kubed-sort-value count)
      (propertize str 'kubed-sort-value count))))

;; ── Deployment Formatting ──

(defun kubed-ext--format-ready-total (ready replicas)
  "Format READY/REPLICAS as ready/total with color coding."
  (let* ((r (string-to-number (or ready "0")))
         (tot (string-to-number (or replicas "0")))
         (str (format "%d/%d" r tot))
         (face (cond
                ((and (= r 0) (= tot 0)) 'shadow)
                ((= r tot)                'success)
                ((> r 0)                  'warning)
                (t                        'error))))
    (propertize str 'face face 'kubed-sort-value r)))

(defun kubed-ext--format-deployment-count (count replicas)
  "Format deployment COUNT relative to REPLICAS with color coding."
  (let* ((c (string-to-number (or count "0")))
         (tot (string-to-number (or replicas "0")))
         (str (number-to-string c))
         (face (cond
                ((and (= c 0) (= tot 0)) 'shadow)
                ((>= c tot)               'success)
                ((> c 0)                  'warning)
                (t                        'error))))
    (propertize str 'face face 'kubed-sort-value c)))

;;; ═══════════════════════════════════════════════════════════════
;;; § 6.  Enhance Upstream Resource Columns + Mode Hooks
;;; ═══════════════════════════════════════════════════════════════

(kubed-ext-set-columns
 "pods"
 (list
  '("PHASE:.status.phase")
  '("READY_NAMES:.status.containerStatuses[?(@.ready==true)].name")
  '("ALL_NAMES:.status.containerStatuses[*].name")
  '("STARTTIME:.status.startTime")
  '("RESTARTS:.status.containerStatuses[*].restartCount")
  '("IP:.status.podIP")
  '("NODE:.spec.nodeName")
  '("WAITING:.status.containerStatuses[*].state.waiting.reason")
  '("TERMINATED:.status.containerStatuses[*].state.terminated.reason")
  '("DELETION:.metadata.deletionTimestamp"))
 nil)

(defun kubed-ext--count-csv-items (s)
  "Count comma-separated items in S.  Empty/none=0, single item=1."
  (cond
   ((kubed-ext-none-p s)             0)
   ((string-empty-p (string-trim s)) 0)
   (t (1+ (seq-count (lambda (c) (= c ?,)) s)))))

(defun kubed-ext--pod-entries ()
  "Custom entries for pods with kubel-style status and color coding.
Reads 11-element fetch vectors and produces 7-column display vectors:
Name, Ready(x/y), Status, Restarts, Age, IP, Node."
  (let* ((raw  (alist-get 'resources
                          (kubed--alist (kubed-ext--eenv-key)
                                        kubed-list-type
                                        kubed-list-context
                                        kubed-list-namespace)))
         (pred (kubed-list-interpret-filter))
         (result nil))
    (dolist (entry raw)
      (let* ((vec (cadr entry))
             (new-entry
              (if (and (vectorp vec) (>= (length vec) 11))
                  (let* ((ready-names  (aref vec 2))
                         (all-names    (aref vec 3))
                         (restart-raw  (aref vec 5))
                         (ready-count  (kubed-ext--count-csv-items ready-names))
                         (total-count  (kubed-ext--count-csv-items all-names))
                         (restart-n    (if (kubed-ext-none-p restart-raw) 0
                                         (apply #'+
                                                (mapcar #'string-to-number
                                                        (split-string
                                                         restart-raw "," t " "))))))
                    (list (car entry)
                          (vector
                           (aref vec 0)
                           (kubed-ext--format-pod-ready
                            (number-to-string ready-count)
                            (number-to-string total-count))
                           (kubed-ext--compute-pod-status
                            (aref vec 1) (aref vec 8) (aref vec 9) (aref vec 10))
                           (kubed-ext--format-pod-restarts restart-n)
                           (kubed-ext--format-age (aref vec 4))
                           (if (kubed-ext-none-p (aref vec 6)) "" (aref vec 6))
                           (if (kubed-ext-none-p (aref vec 7)) "" (aref vec 7)))))
                entry)))
        (when (funcall pred new-entry)
          (push new-entry result))))
    (nreverse result)))

(defun kubed-ext--pod-mode-setup ()
  "Set up pod list with kubel-style Ready, Status, Restarts, Age columns."
  (setq-local tabulated-list-format
              (vector
               kubed-name-column
               (list "Ready" 7 (kubed-ext-top-numeric-sorter 1))
               (list "Status" 22 t)
               (list "Restarts" 9
                     (kubed-ext-top-numeric-sorter 3) :right-align t)
               (list "Age" 10 (kubed-ext-top-numeric-sorter 4))
               (list "IP" 16 t)
               (list "Node" 32 t)))
  (setq-local tabulated-list-entries #'kubed-ext--pod-entries)
  (tabulated-list-init-header))

(add-hook 'kubed-pods-mode-hook #'kubed-ext--pod-mode-setup)

(kubed-ext-append-columns
 "services"
 (list
  (cons "SELECTOR:.spec.selector"
        (lambda (s)
          (cond
           ((kubed-ext-none-p s) "")
           ((string-prefix-p "map[" s) (substring s 4 -1))
           (t s)))))
 (list (list "Selector" 40 t)))

(kubed-ext-set-columns
 "ingresses"
 (list
  '("CLASS:.spec.ingressClassName")
  '("HOSTS:.spec.rules[*].host")
  (cons "ADDRESS:.status.loadBalancer.ingress[0]"
        (lambda (s)
          (cond
           ((kubed-ext-none-p s) "")
           ((string-match "ip:\\([^] ,]+\\)" s) (match-string 1 s))
           ((string-match "hostname:\\([^] ,]+\\)" s) (match-string 1 s))
           (t s))))
  (cons "PORTS:.spec.tls[0].hosts"
        (lambda (s)
          (if (kubed-ext-none-p s) "80" "80, 443")))
  (cons "AGE:.metadata.creationTimestamp" #'kubed-ext--format-age))
 (list
  (list "Class" 20 t)
  (list "Hosts" 40 t)
  (list "Address" 40 t)
  (list "Ports" 10 t)
  (list "Age" 20 t)))

(kubed-ext-set-columns
 "persistentvolumes"
 (list
  '("CAPACITY:.spec.capacity.storage")
  '("ACCESS-MODES:.spec.accessModes[*]")
  '("RECLAIM:.spec.persistentVolumeReclaimPolicy")
  (cons "STATUS:.status.phase"
        (lambda (ph)
          (let ((face (cdr (assoc ph kubed-ext-status-faces))))
            (if face (propertize ph 'face face) ph))))
  '("CLAIM-NS:.spec.claimRef.namespace")
  '("CLAIM-NAME:.spec.claimRef.name")
  '("STORAGECLASS:.spec.storageClassName"))
 (list
  (list "Capacity" 10 t)
  (list "Access-modes" 18 t)
  (list "Reclaim" 14 t)
  (list "Status" 10 t)
  (list "Claim-ns" 16 t)
  (list "Claim-name" 28 t)
  (list "Storageclass" 20 t)))

(defun kubed-ext--deployment-entries ()
  "Custom entries for deployments with kubel-style Ready and Age."
  (let* ((raw (alist-get 'resources
                         (kubed--alist (kubed-ext--eenv-key)
                                       kubed-list-type
                                       kubed-list-context
                                       kubed-list-namespace)))
         (pred (kubed-list-interpret-filter))
         (result nil))
    (dolist (entry raw)
      (let* ((vec (cadr entry))
             (new-entry
              (if (and (vectorp vec) (>= (length vec) 6))
                  (list (car entry)
                        (vector
                         (aref vec 0)
                         (kubed-ext--format-ready-total
                          (let ((v (aref vec 1)))
                            (if (kubed-ext-none-p v) "0" v))
                          (let ((v (aref vec 4)))
                            (if (kubed-ext-none-p v) "0" v)))
                         (kubed-ext--format-deployment-count
                          (let ((v (aref vec 2)))
                            (if (kubed-ext-none-p v) "0" v))
                          (let ((v (aref vec 4)))
                            (if (kubed-ext-none-p v) "0" v)))
                         (kubed-ext--format-deployment-count
                          (let ((v (aref vec 3)))
                            (if (kubed-ext-none-p v) "0" v))
                          (let ((v (aref vec 4)))
                            (if (kubed-ext-none-p v) "0" v)))
                         (kubed-ext--format-age (aref vec 5))))
                entry)))
        (when (funcall pred new-entry)
          (push new-entry result))))
    (nreverse result)))

(defun kubed-ext--deployment-mode-setup ()
  "Set up deployment list with kubel-style Ready and Age columns."
  (setq-local tabulated-list-format
              (vector
               kubed-name-column
               (list "Ready" 10 (kubed-ext-top-numeric-sorter 1))
               (list "Up-to-date" 12
                     (kubed-ext-top-numeric-sorter 2) :right-align t)
               (list "Available" 12
                     (kubed-ext-top-numeric-sorter 3) :right-align t)
               (list "Age" 12 (kubed-ext-top-numeric-sorter 4))))
  (setq-local tabulated-list-entries #'kubed-ext--deployment-entries)
  (tabulated-list-init-header))

(add-hook 'kubed-deployments-mode-hook #'kubed-ext--deployment-mode-setup)

;;; ═══════════════════════════════════════════════════════════════
;;; § 7.  New Built-in Resources
;;; ═══════════════════════════════════════════════════════════════

(eval '(kubed-define-resource node
           ((status ".status.conditions[-1:].status" 10
                    nil
                    (lambda (s)
                      (cond
                       ((string= s "True")    (propertize "Ready"    'face 'success))
                       ((string= s "False")   (propertize "NotReady" 'face 'error))
                       ((string= s "Unknown") (propertize "Unknown"  'face 'warning))
                       (t s))))
            (roles ".metadata.labels" 16
                   nil
                   (lambda (s)
                     (let ((roles nil) (pos 0))
                       (while (string-match
                               "node-role\\.kubernetes\\.io/\\([^:=, ]+\\)"
                               s pos)
                         (push (match-string 1 s) roles)
                         (setq pos (match-end 0)))
                       (if roles (string-join (nreverse roles) ",") ""))))
            (taints ".spec.taints[*].key" 7
                    (lambda (l r)
                      (< (string-to-number l) (string-to-number r)))
                    (lambda (s)
                      (if (or (string-empty-p s) (string= s "")) "0"
                        (number-to-string
                         (1+ (seq-count (lambda (c) (= c ?,)) s)))))
                    :right-align t)
            (version ".status.nodeInfo.kubeletVersion" 14)
            (cpu-alloc ".status.allocatable.cpu" 6
                       (lambda (l r)
                         (< (string-to-number l) (string-to-number r)))
                       nil :right-align t)
            (mem-alloc ".status.allocatable.memory" 8
                       nil
                       (lambda (s)
                         (let ((mib (kubed-ext-parse-mem s)))
                           (if (>= mib 1024)
                               (format "%.1fGi" (/ mib 1024.0))
                             (format "%.0fMi" mib))))
                       :right-align t)
            (internal-ip ".status.addresses[0].address" 16)
            (creationtimestamp ".metadata.creationTimestamp" 20))
         :namespaced nil
         (top "T" "Show resource metrics for"
              (let* ((ctx kubed-list-context)
                     (buf (get-buffer-create
                           (format "*kubed-top node/%s[%s]*" node ctx))))
                (with-current-buffer buf
                  (let ((inhibit-read-only t))
                    (erase-buffer)
                    (insert (format "Loading node metrics for %s...\n" node))
                    (goto-char (point-min))
                    (special-mode)))
                (kubed-ext--async-kubectl-2
                 (list "top" "node" node "--no-headers" "--context" ctx)
                 (list "get" "node" node "--no-headers"
                       "--context" ctx "-o"
                       "custom-columns=CPU:.status.allocatable.cpu,MEM:.status.allocatable.memory")
                 (lambda (outputs)
                   (let* ((top-f (split-string
                                  (string-trim (car outputs)) nil t))
                          (cpu-used (kubed-ext-parse-cpu (nth 1 top-f)))
                          (mem-used (kubed-ext-parse-mem (nth 3 top-f)))
                          (alloc-f (split-string
                                    (string-trim (cadr outputs)) nil t))
                          (cpu-a (kubed-ext-parse-cpu (nth 0 alloc-f)))
                          (mem-a (kubed-ext-parse-mem (nth 1 alloc-f))))
                     (when (buffer-live-p buf)
                       (with-current-buffer buf
                         (let ((inhibit-read-only t))
                           (erase-buffer)
                           (insert (propertize
                                    (format "Node Metrics: %s\n" node)
                                    'face 'bold))
                           (insert (make-string 60 ?-) "\n\n")
                           (insert
                            (format "  CPU:  %s used  /  %s allocatable  (%s)\n"
                                    (kubed-ext-format-cpu cpu-used)
                                    (kubed-ext-format-cpu cpu-a)
                                    (kubed-ext-format-pct cpu-used cpu-a)))
                           (insert
                            (format "  MEM:  %s used  /  %s allocatable  (%s)\n"
                                    (kubed-ext-format-mem mem-used)
                                    (kubed-ext-format-mem mem-a)
                                    (kubed-ext-format-pct mem-used mem-a)))
                           (goto-char (point-min))
                           (special-mode))))))
                 (lambda (err)
                   (when (buffer-live-p buf)
                     (with-current-buffer buf
                       (let ((inhibit-read-only t))
                         (erase-buffer)
                         (insert (format "Failed to load node metrics: %s\n"
                                         err))
                         (special-mode))))))
                (display-buffer buf))))
      t)

(eval '(kubed-define-resource persistentvolumeclaim
           ((status ".status.phase" 10 nil
                    (lambda (ph)
                      (propertize ph 'face
                                  (pcase ph
                                    ("Bound"   'success)
                                    ("Pending" 'warning)
                                    ("Lost"    'error)
                                    (_         'default)))))
            (volume ".spec.volumeName" 36)
            (capacity ".status.capacity.storage" 10)
            (access-modes ".spec.accessModes[*]" 18)
            (storageclass ".spec.storageClassName" 20)))
      t)

(eval '(kubed-define-resource configmap
           ((creationtimestamp ".metadata.creationTimestamp" 20)))
      t)

(eval '(kubed-define-resource event
           ((type ".type" 8 nil
                  (lambda (s)
                    (propertize s 'face
                                (pcase s
                                  ("Normal"  'success)
                                  ("Warning" 'warning)
                                  (_         'default)))))
            (reason ".reason" 20)
            (object-kind ".involvedObject.kind" 12)
            (object-name ".involvedObject.name" 28)
            (count ".count" 6
                   (lambda (l r)
                     (< (string-to-number l) (string-to-number r)))
                   nil :right-align t)
            (message ".message" 50)
            (last-seen ".lastTimestamp" 20)))
      t)

(eval '(kubed-define-resource networkpolicy
           ((pod-selector ".spec.podSelector.matchLabels" 50 nil
                          (lambda (s)
                            (cond
                             ((or (string-empty-p s) (string= s ""))
                              "(all pods)")
                             ((string-prefix-p "map[" s) (substring s 4 -1))
                             (t s))))
            (policy-types ".spec.policyTypes[*]" 18)
            (creationtimestamp ".metadata.creationTimestamp" 20))
         :plural networkpolicies)
      t)

(eval '(kubed-define-resource horizontalpodautoscaler
           ((reference ".spec.scaleTargetRef.name" 28)
            (ref-kind ".spec.scaleTargetRef.kind" 14)
            (min-pods ".spec.minReplicas" 8
                      (lambda (l r)
                        (< (string-to-number l) (string-to-number r)))
                      nil :right-align t)
            (max-pods ".spec.maxReplicas" 8
                      (lambda (l r)
                        (< (string-to-number l) (string-to-number r)))
                      nil :right-align t)
            (replicas ".status.currentReplicas" 8
                      (lambda (l r)
                        (< (string-to-number l) (string-to-number r)))
                      nil :right-align t)))
      t)

(eval '(kubed-define-resource poddisruptionbudget
           ((min-available ".spec.minAvailable" 14)
            (max-unavailable ".spec.maxUnavailable" 16)
            (allowed-disruptions ".status.disruptionsAllowed" 20
                                 (lambda (l r)
                                   (< (string-to-number l)
                                      (string-to-number r)))
                                 nil :right-align t)
            (current-healthy ".status.currentHealthy" 16
                             (lambda (l r)
                               (< (string-to-number l)
                                  (string-to-number r)))
                             nil :right-align t)
            (desired-healthy ".status.desiredHealthy" 16
                             (lambda (l r)
                               (< (string-to-number l)
                                  (string-to-number r)))
                             nil :right-align t)))
      t)

(eval '(kubed-define-resource customresourcedefinition
           ((group ".spec.group" 34)
            (kind ".spec.names.kind" 24)
            (scope ".spec.scope" 12 nil
                   (lambda (s)
                     (propertize s 'face
                                 (pcase s
                                   ("Namespaced" 'success)
                                   ("Cluster"    'warning)
                                   (_            'default)))))
            (versions ".spec.versions[*].name" 16)
            (creationtimestamp ".metadata.creationTimestamp" 20))
         :namespaced nil
         (list-instances "I" "List instances of"
                         (let* ((plural (car (split-string
                                              customresourcedefinition "\\.")))
                                (entry  (tabulated-list-get-entry))
                                (scope  (and entry
                                             (substring-no-properties
                                              (aref entry 3))))
                                (ns-p   (string= scope "Namespaced"))
                                (ctx    kubed-list-context)
                                (buf    (get-buffer-create
                                         (format "*kubed %s[%s]*" plural ctx))))
                           (with-current-buffer buf
                             (let ((inhibit-read-only t))
                               (erase-buffer)
                               (insert (format "Loading %s instances...\n"
                                               plural))
                               (goto-char (point-min))
                               (special-mode)))
                           (kubed-ext--async-kubectl
                            (append (list "get" plural "-o" "wide")
                                    (when ctx (list "--context" ctx))
                                    (when ns-p
                                      (list "--all-namespaces")))
                            (lambda (output)
                              (when (buffer-live-p buf)
                                (with-current-buffer buf
                                  (let ((inhibit-read-only t))
                                    (erase-buffer)
                                    (insert output)
                                    (goto-char (point-min))
                                    (special-mode)))))
                            (lambda (err)
                              (when (buffer-live-p buf)
                                (with-current-buffer buf
                                  (let ((inhibit-read-only t))
                                    (erase-buffer)
                                    (insert (format
                                             "Failed to list %s instances: %s\n"
                                             plural err))
                                    (special-mode))))))
                           (display-buffer buf))))
      t)

;;; ═══════════════════════════════════════════════════════════════
;;; § 8–9.  Plain Resources
;;; ═══════════════════════════════════════════════════════════════

(dolist (spec '((endpoint) (limitrange) (podtemplate) (replicationcontroller)
                (resourcequota) (serviceaccount) (controllerrevision) (lease)
                (endpointslice) (rolebinding) (role)
                (azureapplicationgatewayrewrite) (podmonitor) (servicemonitor)))
  (eval (cons 'kubed-define-resource spec) t))

;;; ═══════════════════════════════════════════════════════════════
;;; § 9a.  Human-Readable Age for All Timestamp Columns
;;; ═══════════════════════════════════════════════════════════════

(defvar kubed-ext--timestamp-patched-types
  (make-hash-table :test 'equal)
  "Resource types whose timestamp columns carry a formatter.")

(defun kubed-ext--timestamp-column-spec-p (spec)
  "Non-nil if SPEC names a Kubernetes timestamp column.
SPEC is a fetch-column string such as
`CREATIONTIMESTAMP:.metadata.creationTimestamp'."
  (and
   (stringp spec)
   (or (string-match-p "\\.creationTimestamp" spec)
       (string-match-p "\\.startTime" spec)
       (string-match-p "\\.lastTimestamp" spec)
       (string-match-p "\\.lastScheduleTime" spec)
       (string-match-p "\\.lastSuccessfulTime" spec)
       (string-match-p "\\.deletionTimestamp" spec)
       (string-match-p "\\`AGE:" spec)
       (string-match-p "\\`LAST.SEEN:" spec)
       (string-match-p "\\`LASTSCHEDULE:" spec)
       (string-match-p "\\`LASTSUCCESS:" spec)
       (string-match-p "\\`STARTTIME:" spec)
       (string-match-p "TIMESTAMP:" spec))))

(defun kubed-ext--patch-type-timestamp-columns (type)
  "Attach age formatter to timestamp columns for TYPE.
Pods and deployments are skipped because they format
ages inside their custom entries functions."
  (unless (or (gethash type kubed-ext--timestamp-patched-types)
              (member type '("pods" "deployments")))
    (puthash type t kubed-ext--timestamp-patched-types)
    (let ((columns (alist-get type kubed--columns
                              nil nil #'string=)))
      (when columns
        (setf (alist-get type kubed--columns
                         nil nil #'string=)
              (mapcar
               (lambda (col)
                 (let ((spec (if (consp col)
                                 (car col)
                               col)))
                   (if (and (kubed-ext--timestamp-column-spec-p
                             spec)
                            (not (and (consp col) (cdr col))))
                       (cons spec #'kubed-ext--format-age)
                     col)))
               columns))))))

(defun kubed-ext--patch-all-timestamp-columns ()
  "Patch timestamp columns for all known resource types."
  (dolist (entry kubed--columns)
    (kubed-ext--patch-type-timestamp-columns (car entry))))

(defun kubed-ext--fix-timestamp-display-columns ()
  "Rename timestamp display columns to short labels.
Install numeric sort via the `kubed-sort-value' text
property that `kubed-ext--format-age' attaches."
  (dolist (type-name '("services" "secrets" "jobs"
                       "replicasets" "daemonsets"
                       "statefulsets" "ingressclasses"
                       "namespaces" "nodes" "events"
                       "cronjobs"))
    (let ((fmt-var (intern
                    (format "kubed-%s-columns"
                            type-name))))
      (when (and (boundp fmt-var)
                 (symbol-value fmt-var))
        (kubed-ext--snapshot-column-variable fmt-var)
        (let ((idx 0))
          (set fmt-var
               (mapcar
                (lambda (col)
                  (cl-incf idx)
                  (let ((nm (downcase (car col))))
                    (cond
                     ((or (string= nm "creationtimestamp")
                          (string= nm "starttime"))
                      (append
                       (list "Age" 10
                             (kubed-ext-top-numeric-sorter
                              idx))
                       (nthcdr 3 col)))
                     ((string= nm "last-seen")
                      (append
                       (list "Last Seen" 10
                             (kubed-ext-top-numeric-sorter
                              idx))
                       (nthcdr 3 col)))
                     ((string= nm "lastschedule")
                      (append
                       (list "Last Sched" 10
                             (kubed-ext-top-numeric-sorter
                              idx))
                       (nthcdr 3 col)))
                     ((string= nm "lastsuccess")
                      (append
                       (list "Last OK" 10
                             (kubed-ext-top-numeric-sorter
                              idx))
                       (nthcdr 3 col)))
                     (t col))))
                (symbol-value fmt-var))))))))

;; Apply patches now -- all kubed-define-resource forms
;; have already been evaluated.
(kubed-ext--patch-all-timestamp-columns)
(kubed-ext--fix-timestamp-display-columns)

;;; ═══════════════════════════════════════════════════════════════
;;; § 10.  Async Helpers
;;; ═══════════════════════════════════════════════════════════════

(defun kubed-ext--async-kubectl (args callback &optional errback)
  "Run kubectl with ARGS asynchronously.
Call CALLBACK with output string on success.
Call ERRBACK with error string on failure."
  (let* ((output-buf (generate-new-buffer " *kubed-ext-async*"))
         (default-directory temporary-file-directory))
    (when (and kubed-ext-command-log-enabled
               (fboundp 'kubed-ext--log-kubectl-command))
      (kubed-ext--log-kubectl-command
       (mapconcat #'identity
                  (cons (kubed-kubectl-program)
                        (seq-filter #'stringp args))
                  " ")))
    (make-process
     :name "kubed-ext-kubectl"
     :buffer output-buf
     :command (cons (kubed-kubectl-program) args)
     :noquery t
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (let ((exit-code (process-exit-status proc))
               (output ""))
           (when (buffer-live-p output-buf)
             (setq output (with-current-buffer output-buf (buffer-string)))
             (kill-buffer output-buf))
           (if (zerop exit-code)
               (funcall callback output)
             (when errback
               (funcall errback
                        (format "kubectl exited %d" exit-code))))))))))

(defun kubed-ext--async-kubectl-2 (args1 args2 callback &optional errback)
  "Run two kubectl commands concurrently.
ARGS1 and ARGS2 are arg lists.
CALLBACK is called with (list out1 out2) on success.
ERRBACK is called with error message on failure."
  (let ((results (vector nil nil))
        (done (vector nil nil))
        (failed nil)
        (called nil))
    (cl-labels ((maybe-finish ()
                              (when (and (not failed)
                                         (not called)
                                         (aref done 0)
                                         (aref done 1))
                                (setq called t)
                                (funcall callback
                                         (list (aref results 0) (aref results 1)))))
                (fail (err)
                      (unless failed
                        (setq failed t)
                        (when errback
                          (funcall errback err)))))
      (kubed-ext--async-kubectl
       args1
       (lambda (out)
         (unless failed
           (aset results 0 out)
           (aset done 0 t)
           (maybe-finish)))
       #'fail)
      (kubed-ext--async-kubectl
       args2
       (lambda (out)
         (unless failed
           (aset results 1 out)
           (aset done 1 t)
           (maybe-finish)))
       #'fail))))

;;; ═══════════════════════════════════════════════════════════════
;;; § 10b.  Top Commands
;;; ═══════════════════════════════════════════════════════════════

(defvar-local kubed-ext-top-context nil "Context for top buffer.")
(defvar-local kubed-ext-top-namespace nil "Namespace for top buffer.")
(defvar-local kubed-ext-top--fetching nil "Non-nil when async fetch in progress.")

(defun kubed-ext-top--set-status (message &optional face)
  "Set top-buffer header status MESSAGE with optional FACE."
  (setq-local header-line-format
              (when message
                (propertize (concat " " message)
                            'face (or face 'shadow)))))

(defun kubed-ext-top-refresh ()
  "Refresh the current top buffer asynchronously."
  (interactive)
  (if kubed-ext-top--fetching
      (kubed-ext-top--set-status "Metrics fetch already in progress." 'shadow)
    (setq kubed-ext-top--fetching t)
    (kubed-ext-top--set-status "Fetching metrics..." 'shadow)
    (force-mode-line-update)
    (cond
     ((derived-mode-p 'kubed-ext-top-nodes-mode) (kubed-ext--top-nodes-fetch))
     ((derived-mode-p 'kubed-ext-top-pods-mode)  (kubed-ext--top-pods-fetch)))))

(defun kubed-ext--fields-at-least (fields count)
  "Return non-nil when FIELDS has at least COUNT elements."
  (>= (length fields) count))

(defun kubed-ext--top-nodes-fetch ()
  "Fetch node metrics asynchronously and populate the table."
  (let ((ctx kubed-ext-top-context) (buf (current-buffer)))
    (kubed-ext--async-kubectl-2
     (list "top" "nodes" "--no-headers" "--context" ctx)
     (list "get" "nodes" "--no-headers" "--context" ctx "-o"
           (concat "custom-columns=NAME:.metadata.name,"
                   "CPU:.status.allocatable.cpu,MEM:.status.allocatable.memory"))
     (lambda (results)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (setq kubed-ext-top--fetching nil)
           (let ((alloc-map (make-hash-table :test 'equal)) (entries nil))
             (dolist (line (split-string (nth 1 results) "\n" t))
               (let ((f (split-string line nil t)))
                 (when (kubed-ext--fields-at-least f 3)
                   (puthash (nth 0 f) (list (kubed-ext-parse-cpu (nth 1 f))
                                            (kubed-ext-parse-mem (nth 2 f)))
                            alloc-map))))
             (dolist (line (split-string (nth 0 results) "\n" t))
               (let ((f (split-string line nil t)))
                 (when (kubed-ext--fields-at-least f 4)
                   (let* ((name (nth 0 f))
                          (cpu-u (kubed-ext-parse-cpu (nth 1 f)))
                          (mem-u (kubed-ext-parse-mem (nth 3 f)))
                          (alloc (gethash name alloc-map '(0 0)))
                          (cpu-a (nth 0 alloc)) (mem-a (nth 1 alloc)))
                     (push (list name (vector name
                                              (kubed-ext-format-cpu cpu-u)
                                              (kubed-ext-format-cpu cpu-a)
                                              (kubed-ext-format-pct cpu-u cpu-a)
                                              (kubed-ext-format-mem mem-u)
                                              (kubed-ext-format-mem mem-a)
                                              (kubed-ext-format-pct mem-u mem-a)))
                           entries)))))
             (if entries
                 (progn
                   (setq tabulated-list-entries (nreverse entries))
                   (kubed-ext-top--set-status nil)
                   (tabulated-list-print t))
               (kubed-ext-top--set-status "No metrics rows parsed." 'warning)
               (message "Node metrics returned no parseable rows."))))))
     (lambda (err)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (setq kubed-ext-top--fetching nil)
           (kubed-ext-top--set-status (format "Metrics failed: %s" err) 'error)
           (message "Node metrics failed: %s" err)))))))

(defun kubed-ext--top-pods-fetch ()
  "Fetch pod metrics asynchronously and populate the table."
  (let ((ctx kubed-ext-top-context) (ns kubed-ext-top-namespace)
        (buf (current-buffer)))
    (kubed-ext--async-kubectl-2
     (append (list "top" "pods" "--no-headers" "--context" ctx)
             (when ns (list "-n" ns)))
     (append (list "get" "pods" "--no-headers" "--context" ctx "-o"
                   (concat "custom-columns=NAME:.metadata.name,"
                           "CR:.spec.containers[*].resources.requests.cpu,"
                           "CL:.spec.containers[*].resources.limits.cpu,"
                           "MR:.spec.containers[*].resources.requests.memory,"
                           "ML:.spec.containers[*].resources.limits.memory"))
             (when ns (list "-n" ns)))
     (lambda (results)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (setq kubed-ext-top--fetching nil)
           (let ((spec-map (make-hash-table :test 'equal)) (entries nil))
             (dolist (line (split-string (nth 1 results) "\n" t))
               (let ((f (split-string line nil t)))
                 (when (kubed-ext--fields-at-least f 5)
                   (puthash (nth 0 f) (list (kubed-ext-sum-cpu (nth 1 f))
                                            (kubed-ext-sum-cpu (nth 2 f))
                                            (kubed-ext-sum-mem (nth 3 f))
                                            (kubed-ext-sum-mem (nth 4 f)))
                            spec-map))))
             (dolist (line (split-string (nth 0 results) "\n" t))
               (let ((f (split-string line nil t)))
                 (when (kubed-ext--fields-at-least f 3)
                   (let* ((name (nth 0 f))
                          (cpu-u (kubed-ext-parse-cpu (nth 1 f)))
                          (mem-u (kubed-ext-parse-mem (nth 2 f)))
                          (spec (gethash name spec-map '(0 0 0 0))))
                     (push (list name
                                 (vector name
                                         (kubed-ext-format-cpu cpu-u)
                                         (kubed-ext-format-mem mem-u)
                                         (kubed-ext-format-cpu (nth 0 spec))
                                         (kubed-ext-format-cpu (nth 1 spec))
                                         (kubed-ext-format-pct cpu-u (nth 0 spec))
                                         (kubed-ext-format-pct cpu-u (nth 1 spec))
                                         (kubed-ext-format-mem (nth 2 spec))
                                         (kubed-ext-format-mem (nth 3 spec))
                                         (kubed-ext-format-pct mem-u (nth 2 spec))
                                         (kubed-ext-format-pct mem-u (nth 3 spec))))
                           entries)))))
             (if entries
                 (progn
                   (setq tabulated-list-entries (nreverse entries))
                   (kubed-ext-top--set-status nil)
                   (tabulated-list-print t))
               (kubed-ext-top--set-status "No metrics rows parsed." 'warning)
               (message "Pod metrics returned no parseable rows."))))))
     (lambda (err)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (setq kubed-ext-top--fetching nil)
           (kubed-ext-top--set-status (format "Metrics failed: %s" err) 'error)
           (message "Pod metrics failed: %s" err)))))))

(define-derived-mode kubed-ext-top-nodes-mode tabulated-list-mode
  "Kubed Top Nodes" "Major mode for displaying node resource usage."
  (setq tabulated-list-format
        (vector (list "Name" 40 t)
                (list "CPU"   8 (kubed-ext-top-numeric-sorter 1) :right-align t)
                (list "CPU/A" 8 (kubed-ext-top-numeric-sorter 2) :right-align t)
                (list "%CPU"  6 (kubed-ext-top-numeric-sorter 3) :right-align t)
                (list "MEM"   8 (kubed-ext-top-numeric-sorter 4) :right-align t)
                (list "MEM/A" 8 (kubed-ext-top-numeric-sorter 5) :right-align t)
                (list "%MEM"  6 (kubed-ext-top-numeric-sorter 6) :right-align t))
        tabulated-list-padding 2
        tabulated-list-entries nil)
  (tabulated-list-init-header))

(define-derived-mode kubed-ext-top-pods-mode tabulated-list-mode
  "Kubed Top Pods" "Major mode for displaying pod resource usage."
  (setq tabulated-list-format
        (vector (list "Name"   48 t)
                (list "CPU"     7 (kubed-ext-top-numeric-sorter 1)  :right-align t)
                (list "MEM"     8 (kubed-ext-top-numeric-sorter 2)  :right-align t)
                (list "CPU/R"   7 (kubed-ext-top-numeric-sorter 3)  :right-align t)
                (list "CPU/L"   7 (kubed-ext-top-numeric-sorter 4)  :right-align t)
                (list "%CPU/R"  7 (kubed-ext-top-numeric-sorter 5)  :right-align t)
                (list "%CPU/L"  7 (kubed-ext-top-numeric-sorter 6)  :right-align t)
                (list "MEM/R"   8 (kubed-ext-top-numeric-sorter 7)  :right-align t)
                (list "MEM/L"   8 (kubed-ext-top-numeric-sorter 8)  :right-align t)
                (list "%MEM/R"  7 (kubed-ext-top-numeric-sorter 9)  :right-align t)
                (list "%MEM/L"  7 (kubed-ext-top-numeric-sorter 10) :right-align t))
        tabulated-list-padding 2
        tabulated-list-entries nil)
  (tabulated-list-init-header))

(keymap-set kubed-ext-top-nodes-mode-map "g" #'kubed-ext-top-refresh)
(keymap-set kubed-ext-top-nodes-mode-map "q" #'quit-window)
(keymap-set kubed-ext-top-nodes-mode-map "b" #'kubed-ext-switch-buffer)
(keymap-set kubed-ext-top-pods-mode-map  "g" #'kubed-ext-top-refresh)
(keymap-set kubed-ext-top-pods-mode-map  "q" #'quit-window)
(keymap-set kubed-ext-top-pods-mode-map  "b" #'kubed-ext-switch-buffer)

(defun kubed-ext-top-nodes (&optional context)
  "Display node resource usage in CONTEXT."
  (interactive
   (list (if current-prefix-arg
             (kubed-read-context "Context" (kubed-local-context))
           (kubed-local-context))))
  (let* ((context (or context (kubed-local-context)))
         (buf (get-buffer-create (format "*kubed-top-nodes[%s]*" context))))
    (with-current-buffer buf
      (kubed-ext-top-nodes-mode)
      (setq-local kubed-ext-top-context context)
      (kubed-ext-top-refresh))
    (display-buffer buf)))

(defun kubed-ext-top-pods (&optional context namespace)
  "Display pod resource usage in CONTEXT and NAMESPACE."
  (interactive
   (let* ((c (kubed-local-context))
          (c (if (equal current-prefix-arg '(16))
                 (kubed-read-context "Context" c) c))
          (n (kubed--namespace c current-prefix-arg)))
     (list c n)))
  (let* ((context (or context (kubed-local-context)))
         (namespace (or namespace (kubed--namespace context)))
         (buf (get-buffer-create
               (format "*kubed-top-pods@%s[%s]*" namespace context))))
    (with-current-buffer buf
      (kubed-ext-top-pods-mode)
      (setq-local kubed-ext-top-context context)
      (setq-local kubed-ext-top-namespace namespace)
      (kubed-ext-top-refresh))
    (display-buffer buf)))

;;; ═══════════════════════════════════════════════════════════════
;;; § 11.  Terminal Support (vterm, eat, ghostel, eshell, ansi-term)
;;; ═══════════════════════════════════════════════════════════════

(defvar kubed-ext--tramp-optimized nil
  "Non-nil when TRAMP has already been optimized for kubed.")

(defvar kubed-ext--original-tramp-connection-properties nil
  "Original `tramp-connection-properties' before kubed-ext changes it.")

(defvar kubed-ext--original-tramp-completion-reread-directory-timeout nil
  "Original `tramp-completion-reread-directory-timeout' before kubed-ext changes it.")

(defvar kubed-ext--original-tramp-state-saved nil
  "Non-nil after kubed-ext snapshots global Tramp settings.")

(defun kubed-ext--snapshot-tramp-state ()
  "Remember global Tramp settings before applying kubed-ext optimizations."
  (unless kubed-ext--original-tramp-state-saved
    (setq kubed-ext--original-tramp-connection-properties
          (and (boundp 'tramp-connection-properties)
               (copy-tree tramp-connection-properties t))
          kubed-ext--original-tramp-completion-reread-directory-timeout
          (and (boundp 'tramp-completion-reread-directory-timeout)
               tramp-completion-reread-directory-timeout)
          kubed-ext--original-tramp-state-saved t)))

(defun kubed-ext--restore-tramp-state ()
  "Restore global Tramp settings changed by kubed-ext."
  (when kubed-ext--original-tramp-state-saved
    (when (boundp 'tramp-connection-properties)
      (setq tramp-connection-properties
            (copy-tree kubed-ext--original-tramp-connection-properties t)))
    (when (boundp 'tramp-completion-reread-directory-timeout)
      (setq tramp-completion-reread-directory-timeout
            kubed-ext--original-tramp-completion-reread-directory-timeout))
    (setq kubed-ext--tramp-optimized nil)))

(defun kubed-ext-optimize-tramp ()
  "Configure TRAMP to cache connections, improving Eshell/Dired speed."
  (unless kubed-ext--tramp-optimized
    (setq kubed-ext--tramp-optimized t)
    (require 'tramp)
    (kubed-ext--snapshot-tramp-state)
    (when (require 'kubed-tramp nil t)
      (when (boundp 'kubed-tramp-method)
        (add-to-list 'tramp-connection-properties
                     (list (regexp-quote (format "/%s:" kubed-tramp-method))
                           "direct-async-process" t))))
    (when (boundp 'tramp-completion-reread-directory-timeout)
      (setq tramp-completion-reread-directory-timeout nil))))

(add-hook 'kubed-list-mode-hook #'kubed-ext-optimize-tramp)

(defun kubed-ext--kubectl-exec-command (pod container context namespace shell)
  "Build kubectl exec command for POD/CONTAINER/CONTEXT/NAMESPACE/SHELL."
  (mapconcat #'shell-quote-argument
             (append (list (kubed-kubectl-program) "exec" "-it" pod
                           "-c" container)
                     (when namespace (list "-n" namespace))
                     (when context   (list "--context" context))
                     (list "--" shell))
             " "))

(defun kubed-ext--kubectl-exec-args (pod container context namespace shell)
  "Build kubectl exec argument list for POD/CONTAINER/CONTEXT/NAMESPACE/SHELL."
  (append (list "exec" "-it" pod "-c" container)
          (when namespace (list "-n" namespace))
          (when context (list "--context" context))
          (list "--" shell)))

(defun kubed-ext-pods-vterm (click)
  "Open vterm in Kubernetes pod at CLICK position."
  (interactive (list last-nonmenu-event) kubed-pods-mode)
  (unless (require 'vterm nil t)
    (user-error "This command requires the `vterm' package"))
  (if-let ((pod (kubed-ext--resource-at-event click)))
      (let* ((container (kubed-read-container pod "Container" t
                                              kubed-list-context kubed-list-namespace))
             (vterm-shell (kubed-ext--kubectl-exec-command
                           pod container kubed-list-context kubed-list-namespace
                           kubed-ext-pod-shell))
             (vterm-buffer-name
              (format "*Kubed vterm %s*"
                      (kubed-display-resource-short-description
                       "pods" pod kubed-list-context kubed-list-namespace))))
        (vterm vterm-buffer-name))
    (user-error "No Kubernetes pod at point")))

(defun kubed-ext-vterm-pod (pod &optional context namespace)
  "Open vterm in Kubernetes POD in CONTEXT/NAMESPACE."
  (interactive
   (let* ((c (kubed-local-context))
          (c (if (equal current-prefix-arg '(16))
                 (kubed-read-context "Context" c) c))
          (n (kubed--namespace c current-prefix-arg)))
     (list (kubed-read-pod "Open vterm in pod" nil nil c n) c n)))
  (unless (require 'vterm nil t)
    (user-error "This command requires the `vterm' package"))
  (let* ((context (or context (kubed-local-context)))
         (namespace (or namespace (kubed--namespace context)))
         (container (kubed-read-container pod "Container" t context namespace))
         (vterm-shell (kubed-ext--kubectl-exec-command
                       pod container context namespace kubed-ext-pod-shell))
         (vterm-buffer-name
          (format "*Kubed vterm %s*"
                  (kubed-display-resource-short-description
                   "pods" pod context namespace))))
    (vterm vterm-buffer-name)))

(defun kubed-ext-pods-eat (click)
  "Open eat terminal in Kubernetes pod at CLICK position."
  (interactive (list last-nonmenu-event) kubed-pods-mode)
  (unless (require 'eat nil t)
    (user-error "This command requires the `eat' package"))
  (if-let ((pod (kubed-ext--resource-at-event click)))
      (let* ((container (kubed-read-container pod "Container" t
                                              kubed-list-context kubed-list-namespace))
             (cmd (kubed-ext--kubectl-exec-command
                   pod container kubed-list-context kubed-list-namespace
                   kubed-ext-pod-shell))
             (eat-buffer-name
              (format "*Kubed eat %s*"
                      (kubed-display-resource-short-description
                       "pods" pod kubed-list-context kubed-list-namespace))))
        (eat cmd t))
    (user-error "No Kubernetes pod at point")))

(defun kubed-ext-eat-pod (pod &optional context namespace)
  "Open eat terminal in Kubernetes POD in CONTEXT/NAMESPACE."
  (interactive
   (let* ((c (kubed-local-context))
          (c (if (equal current-prefix-arg '(16))
                 (kubed-read-context "Context" c) c))
          (n (kubed--namespace c current-prefix-arg)))
     (list (kubed-read-pod "Open eat in pod" nil nil c n) c n)))
  (unless (require 'eat nil t)
    (user-error "This command requires the `eat' package"))
  (let* ((context (or context (kubed-local-context)))
         (namespace (or namespace (kubed--namespace context)))
         (container (kubed-read-container pod "Container" t context namespace))
         (cmd (kubed-ext--kubectl-exec-command
               pod container context namespace kubed-ext-pod-shell))
         (eat-buffer-name
          (format "*Kubed eat %s*"
                  (kubed-display-resource-short-description
                   "pods" pod context namespace))))
    (eat cmd t)))

(defun kubed-ext--ghostel-buffer-name (pod context namespace)
  "Return a Ghostel buffer name for POD in CONTEXT/NAMESPACE."
  (format "*Kubed ghostel %s*"
          (kubed-display-resource-short-description
           "pods" pod context namespace)))

(defun kubed-ext--ghostel-pod (pod context namespace)
  "Open Ghostel in Kubernetes POD in CONTEXT/NAMESPACE."
  (unless (require 'ghostel nil t)
    (user-error "This command requires the `ghostel' package"))
  (kubed-ext--read-pod-container-async
   pod "Container" context namespace
   (lambda (container)
     (let ((buf (generate-new-buffer
                 (kubed-ext--ghostel-buffer-name pod context namespace))))
       (display-buffer buf)
       (ghostel-exec
        buf (kubed-kubectl-program)
        (kubed-ext--kubectl-exec-args
         pod container context namespace kubed-ext-pod-shell))))
   nil t))

(defun kubed-ext-pods-ghostel (click)
  "Open Ghostel in Kubernetes pod at CLICK position."
  (interactive (list last-nonmenu-event) kubed-pods-mode)
  (if-let ((pod (kubed-ext--resource-at-event click)))
      (kubed-ext--ghostel-pod pod kubed-list-context kubed-list-namespace)
    (user-error "No Kubernetes pod at point")))

(defun kubed-ext-ghostel-pod (pod &optional context namespace)
  "Open Ghostel in Kubernetes POD in CONTEXT/NAMESPACE."
  (interactive
   (let* ((c (kubed-local-context))
          (c (if (equal current-prefix-arg '(16))
                 (kubed-read-context "Context" c) c))
          (n (kubed--namespace c current-prefix-arg)))
     (list (kubed-read-pod "Open ghostel in pod" nil nil c n) c n)))
  (let ((ctx (or context (kubed-local-context))))
    (kubed-ext--ghostel-pod pod ctx (or namespace (kubed--namespace ctx)))))

(defun kubed-ext-pods-eshell (click)
  "Open eshell in Kubernetes pod at CLICK position."
  (interactive (list last-nonmenu-event) kubed-pods-mode)
  (require 'kubed-tramp) (kubed-tramp-assert-support)
  (if-let ((pod (kubed-ext--resource-at-event click)))
      (let* ((default-directory
               (kubed-remote-file-name
                kubed-list-context kubed-list-namespace pod))
             (eshell-buffer-name
              (format "*Kubed eshell %s*"
                      (kubed-display-resource-short-description
                       "pods" pod kubed-list-context kubed-list-namespace))))
        (eshell t))
    (user-error "No Kubernetes pod at point")))

(defun kubed-ext-eshell-pod (pod &optional context namespace)
  "Open eshell in Kubernetes POD in CONTEXT/NAMESPACE."
  (interactive
   (let* ((c (kubed-local-context))
          (c (if (equal current-prefix-arg '(16))
                 (kubed-read-context "Context" c) c))
          (n (kubed--namespace c current-prefix-arg)))
     (list (kubed-read-pod "Open eshell in pod" nil nil c n) c n)))
  (require 'kubed-tramp) (kubed-tramp-assert-support)
  (let* ((context (or context (kubed-local-context)))
         (namespace (or namespace (kubed--namespace context)))
         (default-directory (kubed-remote-file-name context namespace pod))
         (eshell-buffer-name
          (format "*Kubed eshell %s*"
                  (kubed-display-resource-short-description
                   "pods" pod context namespace))))
    (eshell t)))

(defun kubed-ext-pods-ansi-term (click)
  "Open `ansi-term' in Kubernetes pod at CLICK position."
  (interactive (list last-nonmenu-event) kubed-pods-mode)
  (if-let ((pod (kubed-ext--resource-at-event click)))
      (let* ((container (kubed-read-container pod "Container" t
                                              kubed-list-context kubed-list-namespace))
             (cmd (kubed-ext--kubectl-exec-command
                   pod container kubed-list-context kubed-list-namespace
                   kubed-ext-pod-shell))
             (buf-name (format "Kubed term %s"
                               (kubed-display-resource-short-description
                                "pods" pod kubed-list-context kubed-list-namespace))))
        (with-current-buffer (ansi-term "/bin/bash" buf-name)
          (process-send-string (get-buffer-process (current-buffer))
                               (concat cmd "\n"))))
    (user-error "No Kubernetes pod at point")))

(defun kubed-ext-pods-command (click)
  "Run `kubectl exec' for a command in pod at CLICK position."
  (interactive (list last-nonmenu-event) kubed-pods-mode)
  (if-let ((pod (kubed-ext--resource-at-event click)))
      (let* ((container (kubed-read-container pod "Container" t
                                              kubed-list-context kubed-list-namespace))
             (command (read-shell-command "Command in pod: " kubed-ext-pod-shell))
             (buf (get-buffer-create
                   (format "*Kubed exec %s*"
                           (kubed-display-resource-short-description
                            "pods" pod kubed-list-context kubed-list-namespace))))
             (args (append
                    (kubed-ext--kubectl-exec-args
                     pod container kubed-list-context kubed-list-namespace
                     "sh")
                    (list "-lc" command))))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (special-mode)))
        (apply #'start-process "*kubed-exec-command*" buf
               (kubed-kubectl-program) args)
        (display-buffer buf))
    (user-error "No Kubernetes pod at point")))

(define-obsolete-function-alias
  'kubed-ext-pods-shell-command #'kubed-ext-pods-command "0.4.1")

;;; ═══════════════════════════════════════════════════════════════
;;; § 12.  Describe Resource
;;; ═══════════════════════════════════════════════════════════════

(defun kubed-ext-list-describe-resource (click)
  "Describe Kubernetes resource at CLICK position using kubectl describe."
  (interactive (list last-nonmenu-event) kubed-list-mode)
  (if-let ((name (kubed-ext--resource-at-event click)))
      (let* ((type kubed-list-type)
             (namespace kubed-list-namespace)
             (context kubed-list-context)
             (buf (get-buffer-create
                   (format "*Kubed describe %s/%s@%s[%s]*"
                           type name
                           (or namespace "default")
                           (or context "current")))))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert "Loading describe output...\n")
            (special-mode)))
        (kubed-ext--async-kubectl
         (append (list "describe" type name)
                 (when namespace (list "-n" namespace))
                 (when context (list "--context" context)))
         (lambda (output)
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (let ((inhibit-read-only t))
                 (erase-buffer)
                 (insert output)
                 (goto-char (point-min))))))
         (lambda (err)
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (let ((inhibit-read-only t))
                 (goto-char (point-max))
                 (insert "\n" err "\n"))))))
        (display-buffer buf))
    (user-error "No Kubernetes resource at point")))

(defun kubed-ext-describe-resource (type name &optional context namespace)
  "Describe resource NAME of TYPE in CONTEXT and NAMESPACE."
  (interactive
   (let* ((c (kubed-local-context))
          (c (if (equal current-prefix-arg '(16))
                 (kubed-read-context "Context" c) c))
          (_ (kubed-ext-discover-crds-async c))
          (choices (kubed-ext--resource-choices c))
          (label (completing-read "Type to describe: " choices nil t))
          (type (alist-get label choices nil nil #'string=))
          (ns (when (kubed-ext--resource-namespaced-p type c)
                (kubed--namespace c current-prefix-arg)))
          (name (kubed-read-resource-name type "Describe" nil nil c ns)))
     (list type name c ns)))
  (let* ((ctx (or context (kubed-local-context)))
         (ns (or namespace
                 (when (kubed-ext--resource-namespaced-p type ctx)
                   (kubed--namespace ctx))))
         (buf (get-buffer-create
               (format "*Kubed describe %s/%s@%s[%s]*"
                       type name (or ns "cluster") ctx))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Loading describe output...\n")
        (special-mode)))
    (kubed-ext--async-kubectl
     (append (list "describe" type name)
             (when ns (list "-n" ns))
             (when ctx (list "--context" ctx)))
     (lambda (output)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert output)
             (goto-char (point-min))))))
     (lambda (err)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (let ((inhibit-read-only t))
             (goto-char (point-max))
             (insert "\n" err "\n"))))))
    (display-buffer buf)))

;;; ═══════════════════════════════════════════════════════════════
;;; § 12a.  initContainer Logs
;;; ═══════════════════════════════════════════════════════════════

(defun kubed-ext-logs-init-container (&optional context namespace)
  "Show logs for an init container of a pod in CONTEXT/NAMESPACE."
  (interactive
   (let* ((c (kubed-local-context))
          (c (if (equal current-prefix-arg '(16))
                 (kubed-read-context "Context" c) c))
          (n (kubed--namespace c current-prefix-arg)))
     (list c n)))
  (let* ((ctx (or context (kubed-local-context)))
         (ns (or namespace (kubed--namespace ctx)))
         (pod (kubed-read-pod "Pod" nil nil ctx ns)))
    (kubed-ext--read-pod-container-async
     pod "Init container" ctx ns
     (lambda (container)
       (kubed-logs "pods" pod ctx ns container nil nil nil nil nil nil))
     t t)))

(defun kubed-ext-pods-logs-init-container (click)
  "Show init container logs for pod at CLICK position."
  (interactive (list last-nonmenu-event) kubed-pods-mode)
  (if-let ((pod (kubed-ext--resource-at-event click)))
      (kubed-ext--read-pod-container-async
       pod "Init container" kubed-list-context kubed-list-namespace
       (lambda (container)
         (kubed-logs "pods" pod kubed-list-context kubed-list-namespace
                     container nil nil nil nil nil nil))
       t t)
    (user-error "No Kubernetes pod at point")))

;;; ═══════════════════════════════════════════════════════════════
;;; § 12b.  Logs by Label Selector
;;; ═══════════════════════════════════════════════════════════════

(defun kubed-ext-logs-by-label (&optional context namespace)
  "Show logs for pods matching a label selector in CONTEXT/NAMESPACE."
  (interactive
   (let* ((c (kubed-local-context))
          (c (if (equal current-prefix-arg '(16))
                 (kubed-read-context "Context" c) c))
          (n (kubed--namespace c current-prefix-arg)))
     (list c n)))
  (let* ((ctx (or context (kubed-local-context)))
         (ns (or namespace (kubed--namespace ctx)))
         (label (read-string "Label selector: "
                             nil 'kubed-ext-label-selector-history))
         (tail (read-number "Tail lines: " 100))
         (buf (generate-new-buffer
               (format "*kubed-logs -l %s@%s[%s]*" label ns ctx))))
    (with-current-buffer buf (run-hooks 'kubed-logs-setup-hook))
    (kubed-ext--launch-log-process
     buf
     (list "logs" "-l" label "--tail" (number-to-string tail)
           "--no-follow" "-n" ns "--context" ctx)
     nil)
    (display-buffer buf)))

;;; ═══════════════════════════════════════════════════════════════
;;; § 13.  Rollout Management
;;; ═══════════════════════════════════════════════════════════════

(defvar-local kubed-ext-rollout-type nil)
(defvar-local kubed-ext-rollout-name nil)
(defvar-local kubed-ext-rollout-context nil)
(defvar-local kubed-ext-rollout-namespace nil)

(defun kubed-ext-rollout-history (type name &optional context namespace)
  "Show rollout history for TYPE/NAME in CONTEXT/NAMESPACE."
  (interactive
   (let* ((c (kubed-local-context))
          (c (if (equal current-prefix-arg '(16))
                 (kubed-read-context "Context" c) c))
          (n (kubed--namespace c current-prefix-arg))
          (type (completing-read "Resource type: "
                                 '("deployment" "statefulset" "daemonset") nil t))
          (name (kubed-read-resource-name
                 (concat type "s") "Rollout history for" nil nil c n)))
     (list type name c n)))
  (let* ((ctx (or context (kubed-local-context)))
         (ns (or namespace (kubed--namespace ctx)))
         (typename (format "%s/%s" type name))
         (buf (get-buffer-create
               (format "*kubed rollout %s@%s[%s]*"
                       typename (or ns "default") ctx))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Rollout History: %s\n" typename)
                            'face 'bold))
        (insert (format "Namespace: %s  Context: %s\n" (or ns "default") ctx))
        (insert (make-string 60 ?─) "\n\nLoading...\n")
        (insert (propertize
                 "\nKeys: r = view revision  u = undo  g = refresh  q = quit\n"
                 'face 'shadow))
        (goto-char (point-min))
        (setq-local kubed-ext-rollout-type type
                    kubed-ext-rollout-name name
                    kubed-ext-rollout-context ctx
                    kubed-ext-rollout-namespace ns)
        (kubed-ext-rollout-history-mode)))
    (kubed-ext--async-kubectl
     (append (list "rollout" "history" typename)
             (when ns (list "-n" ns))
             (when ctx (list "--context" ctx)))
     (lambda (output)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert (propertize (format "Rollout History: %s\n" typename)
                                 'face 'bold))
             (insert (format "Namespace: %s  Context: %s\n"
                             (or ns "default") ctx))
             (insert (make-string 60 ?-) "\n\n")
             (insert output)
             (insert "\n" (make-string 60 ?-) "\n")
             (insert (propertize
                      "\nKeys: r = view revision  u = undo  g = refresh  q = quit\n"
                      'face 'shadow))
             (goto-char (point-min))))))
     (lambda (err)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (let ((inhibit-read-only t))
             (goto-char (point-max))
             (insert "\n" err "\n"))))))
    (display-buffer buf)))

(defun kubed-ext-rollout-show-revision ()
  "Show a specific revision from the rollout history buffer."
  (interactive nil kubed-ext-rollout-history-mode)
  (let* ((typename (format "%s/%s" kubed-ext-rollout-type
                           kubed-ext-rollout-name))
         (ctx kubed-ext-rollout-context)
         (ns kubed-ext-rollout-namespace)
         (rev (read-number "Revision: "))
         (buf (get-buffer-create
               (format "*kubed rollout %s rev %d*" typename rev))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Revision %d: %s\n" rev typename)
                            'face 'bold))
        (insert (make-string 60 ?-) "\n\nLoading...\n")
        (special-mode)))
    (kubed-ext--async-kubectl
     (append (list "rollout" "history" typename (format "--revision=%d" rev))
             (when ns (list "-n" ns))
             (when ctx (list "--context" ctx)))
     (lambda (output)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert (propertize (format "Revision %d: %s\n" rev typename)
                                 'face 'bold))
             (insert (make-string 60 ?-) "\n\n")
             (insert output)
             (goto-char (point-min))))))
     (lambda (err)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (let ((inhibit-read-only t))
             (goto-char (point-max))
             (insert "\n" err "\n"))))))
    (display-buffer buf)))

(defun kubed-ext-rollout-undo (type name &optional revision context namespace)
  "Undo rollout of TYPE/NAME to REVISION in CONTEXT/NAMESPACE."
  (interactive
   (let* ((c (kubed-local-context))
          (c (if (equal current-prefix-arg '(16))
                 (kubed-read-context "Context" c) c))
          (n (kubed--namespace c current-prefix-arg))
          (type (completing-read "Resource type: "
                                 '("deployment" "statefulset" "daemonset") nil t))
          (name (kubed-read-resource-name
                 (concat type "s") "Undo rollout for" nil nil c n))
          (rev (when (y-or-n-p "Specify target revision? ")
                 (read-number "To revision: "))))
     (list type name rev c n)))
  (let* ((ctx (or context (kubed-local-context)))
         (ns (or namespace (kubed--namespace ctx)))
         (typename (format "%s/%s" type name)))
    (message "Rolling back %s..." typename)
    (kubed-ext--async-kubectl
     (append (list "rollout" "undo" typename)
             (when revision
               (list (format "--to-revision=%d" revision)))
             (when ns (list "-n" ns))
             (when ctx (list "--context" ctx)))
     (lambda (_output)
       (message "Rolled back %s%s." typename
                (if revision (format " to revision %d" revision) "")))
     (lambda (err)
       (message "Failed to undo rollout for %s: %s" typename err)))))

(defun kubed-ext-rollout-undo-from-history ()
  "Undo rollout from history buffer."
  (interactive nil kubed-ext-rollout-history-mode)
  (let* ((typename (format "%s/%s" kubed-ext-rollout-type
                           kubed-ext-rollout-name))
         (rev (when (y-or-n-p "Specify revision? ")
                (read-number "To revision: "))))
    (when (y-or-n-p (format "Undo rollout for %s%s? "
                            typename
                            (if rev (format " to rev %d" rev) "")))
      (kubed-ext-rollout-undo kubed-ext-rollout-type kubed-ext-rollout-name
                              rev kubed-ext-rollout-context
                              kubed-ext-rollout-namespace))))

(defun kubed-ext-rollout-refresh ()
  "Refresh rollout history buffer."
  (interactive nil kubed-ext-rollout-history-mode)
  (kubed-ext-rollout-history kubed-ext-rollout-type kubed-ext-rollout-name
                             kubed-ext-rollout-context
                             kubed-ext-rollout-namespace))

(define-derived-mode kubed-ext-rollout-history-mode special-mode "Kubed Rollout"
  "Mode for viewing Kubernetes rollout history."
  :interactive nil)

(keymap-set kubed-ext-rollout-history-mode-map "r"
            #'kubed-ext-rollout-show-revision)
(keymap-set kubed-ext-rollout-history-mode-map "u"
            #'kubed-ext-rollout-undo-from-history)
(keymap-set kubed-ext-rollout-history-mode-map "g"
            #'kubed-ext-rollout-refresh)
(keymap-set kubed-ext-rollout-history-mode-map "q" #'quit-window)

(defun kubed-ext-deployments-rollout-history (click)
  "Show rollout history for deployment at CLICK position."
  (interactive (list last-nonmenu-event) kubed-deployments-mode)
  (if-let ((dep (kubed-ext--resource-at-event click)))
      (kubed-ext-rollout-history "deployment" dep
                                 kubed-list-context kubed-list-namespace)
    (user-error "No Kubernetes deployment at point")))

(defun kubed-ext-deployments-rollout-undo (click)
  "Undo rollout for deployment at CLICK position."
  (interactive (list last-nonmenu-event) kubed-deployments-mode)
  (if-let ((dep (kubed-ext--resource-at-event click)))
      (let ((rev (when (y-or-n-p "Specify target revision? ")
                   (read-number "To revision: "))))
        (when (y-or-n-p (format "Undo rollout for %s?" dep))
          (kubed-ext-rollout-undo "deployment" dep rev
                                  kubed-list-context kubed-list-namespace)))
    (user-error "No Kubernetes deployment at point")))

;;; ═══════════════════════════════════════════════════════════════
;;; § 14.  Jab / Bounce Deployment
;;; ═══════════════════════════════════════════════════════════════

(defun kubed-ext-jab-deployment (deployment &optional context namespace)
  "Force rolling update of DEPLOYMENT in CONTEXT/NAMESPACE."
  (interactive
   (let* ((c (kubed-local-context))
          (c (if (equal current-prefix-arg '(16))
                 (kubed-read-context "Context" c) c))
          (n (kubed--namespace c current-prefix-arg)))
     (list (kubed-read-deployment "Jab (bounce) deployment" nil nil c n) c n)))
  (let* ((ctx (or context (kubed-local-context)))
         (ns (or namespace (kubed--namespace ctx)))
         (q kubed-ext--dq)
         (ts (number-to-string (floor (float-time))))
         (patch (concat "{" q "spec" q ":{" q "template" q ":{" q "metadata" q
                        ":{" q "labels" q ":{" q "date" q ":" q ts q "}}}}}")))
    (message "Jabbing deployment %s..." deployment)
    (kubed-ext--async-kubectl
     (append (list "patch" "deployment" deployment "-p" patch)
             (when ns (list "-n" ns))
             (when ctx (list "--context" ctx)))
     (lambda (_output)
       (message "Jabbed deployment %s (timestamp %s)." deployment ts))
     (lambda (err)
       (message "Failed to jab deployment %s: %s" deployment err)))))

(defun kubed-ext-deployments-jab (click)
  "Jab deployment at CLICK position to force a rolling update."
  (interactive (list last-nonmenu-event) kubed-deployments-mode)
  (if-let ((dep (kubed-ext--resource-at-event click)))
      (progn (kubed-ext-jab-deployment dep kubed-list-context
                                       kubed-list-namespace)
             (kubed-list-update t))
    (user-error "No Kubernetes deployment at point")))

;;; ═══════════════════════════════════════════════════════════════
;;; § 15.  Copy / Clipboard Operations
;;; ═══════════════════════════════════════════════════════════════

(defvar kubed-ext--last-kubectl-command nil
  "Last kubectl command tracked by kubed-ext.")

(defvar kubed-ext--command-log-lines 0
  "Approximate number of lines in `*kubed-command-log*'.")

(defun kubed-ext-list-copy-log-command (click)
  "Copy `stern' command for resource at CLICK position."
  (interactive (list last-nonmenu-event) kubed-list-mode)
  (if-let ((name (kubed-ext--resource-at-event click)))
      (let* ((args (append
                    (list kubed-ext-stern-program
                          (regexp-quote name)
                          "--tail" "100")
                    (when kubed-list-namespace
                      (list "-n" kubed-list-namespace))
                    (when kubed-list-context
                      (list "--context" kubed-list-context))))
             (cmd (mapconcat #'shell-quote-argument args " ")))
        (unless (member kubed-list-type '("pod" "pods"))
          (user-error "Copy stern command is non-blocking only for pods, not %s/%s"
                      kubed-list-type name))
        (kill-new cmd)
        (message "Copied: %s" cmd))
    (user-error "No Kubernetes resource at point")))

(defun kubed-ext-list-copy-kubectl-prefix ()
  "Copy the current kubectl command prefix."
  (interactive nil kubed-list-mode)
  (let ((prefix (format "%s%s%s"
                        (kubed-kubectl-program)
                        (if kubed-list-namespace
                            (format " -n %s" kubed-list-namespace) "")
                        (if kubed-list-context
                            (format " --context %s" kubed-list-context) ""))))
    (kill-new prefix)
    (message "Copied: %s" prefix)))

(defun kubed-ext-list-copy-last-command ()
  "Copy the last tracked kubectl command."
  (interactive nil kubed-list-mode)
  (if kubed-ext--last-kubectl-command
      (progn (kill-new kubed-ext--last-kubectl-command)
             (message "Copied: %s" kubed-ext--last-kubectl-command))
    (message "No kubectl command tracked yet.")))

(defun kubed-ext-list-copy-resource-as-yaml (click)
  "Copy YAML of resource at CLICK position."
  (interactive (list last-nonmenu-event) kubed-list-mode)
  (if-let ((name (kubed-ext--resource-at-event click)))
      (let* ((type kubed-list-type)
             (namespace kubed-list-namespace)
             (context kubed-list-context))
        (message "Fetching YAML for %s/%s..." type name)
        (kubed-ext--async-kubectl
         (append (list "get" type name "-o" "yaml")
                 (when namespace (list "-n" namespace))
                 (when context (list "--context" context)))
         (lambda (yaml)
           (kill-new yaml)
           (message "YAML for %s/%s copied (%d bytes)."
                    type name (length yaml)))
         (lambda (err)
           (message "Failed to copy YAML for %s/%s: %s" type name err))))
    (user-error "No Kubernetes resource at point")))

(transient-define-prefix kubed-ext-copy-popup ()
  "Kubed Copy Menu."
  ["Copy to kill ring"
   ("w" "Resource name"  kubed-list-copy-as-kill)
   ("l" "Log command"    kubed-ext-list-copy-log-command)
   ("p" "kubectl prefix" kubed-ext-list-copy-kubectl-prefix)
   ("C" "Last command"   kubed-ext-list-copy-last-command)
   ("y" "Resource YAML"  kubed-ext-list-copy-resource-as-yaml)])

;;; ═══════════════════════════════════════════════════════════════
;;; § 16.  Label Selector
;;; ═══════════════════════════════════════════════════════════════

(defvar kubed-ext-label-selector-history nil
  "History list for label selectors.")

(defun kubed-ext--find-active-selector (type context namespace &optional eenv)
  "Find the label selector for TYPE/CONTEXT/NAMESPACE in EENV.
Selectors are cached by list identity to keep the update hot path from
walking every Emacs buffer on each refresh.  A one-pass list-buffer scan
is retained for cache misses and preexisting buffers."
  (let* ((key (kubed-ext--selector-cache-key type context namespace eenv))
         (cached (gethash key kubed-ext--selector-cache)))
    (or cached
        (catch 'found
          (kubed-ext--for-each-list-buffer
           (lambda (_buf)
             (when (and (bound-and-true-p kubed-ext-list-label-selector)
                        (equal kubed-list-type type)
                        (equal kubed-list-context context)
                        (equal kubed-list-namespace namespace)
                        (equal (kubed-ext--eenv-key) eenv))
               (puthash key kubed-ext-list-label-selector
                        kubed-ext--selector-cache)
               (throw 'found kubed-ext-list-label-selector))))))))

(defun kubed-ext-list-set-label-selector (selector)
  "Set label SELECTOR for server-side filtering.  Empty clears it."
  (interactive
   (list (read-string
          (format-prompt "Label selector" "clear")
          nil 'kubed-ext-label-selector-history))
   kubed-list-mode)
  (setq-local kubed-ext-list-label-selector
              (if (string-empty-p selector) nil selector))
  (kubed-ext--cache-current-selector)
  (kubed-list-update))

;; replaced in section § 21
;; (setq kubed-list-mode-line-format
;;       '(:eval
;;         (if (process-live-p
;;              (alist-get 'process (kubed--alist kubed-list-type
;;                                                kubed-list-context
;;                                                kubed-list-namespace)))
;;             (propertize " [...]" 'help-echo "Updating...")
;;           (concat
;;            (when kubed-list-filter
;;              (propertize
;;               (concat " [" (mapconcat #'prin1-to-string
;;                                       kubed-list-filter " ") "]")
;;               'help-echo "Current filter"))
;;            (when kubed-ext-list-label-selector
;;              (propertize
;;               (concat " {" kubed-ext-list-label-selector "}")
;;               'help-echo "Label selector" 'face 'warning))
;;            (when (and (bound-and-true-p kubed-ext-resource-filter)
;;                       (not (string-empty-p kubed-ext-resource-filter)))
;;              (propertize
;;               (concat " /" kubed-ext-resource-filter "/")
;;               'help-echo "Substring filter" 'face 'italic))))))

;;; ═══════════════════════════════════════════════════════════════
;;; § 17.  Buffer Switching + Command Log
;;; ═══════════════════════════════════════════════════════════════

(defun kubed-ext-switch-buffer ()
  "Switch to another Kubed buffer.

Includes list buffers, wide-view buffers, and top/metrics buffers."
  (interactive)
  (let (bufs)
    (dolist (buf (buffer-list))
      (when (with-current-buffer buf
              (or (derived-mode-p 'kubed-list-mode)
                  (derived-mode-p 'kubed-ext-wide-mode)
                  (derived-mode-p 'kubed-ext-top-nodes-mode)
                  (derived-mode-p 'kubed-ext-top-pods-mode)
                  (derived-mode-p 'kubed-ext-port-forwards-mode)))
        (push (cons (kubed-ext--buffer-switch-candidate buf) buf) bufs)))
    (setq bufs (nreverse bufs))
    (if bufs
        (let* ((sel (completing-read "Switch to kubed buffer: " bufs nil t))
               (buf (alist-get sel bufs nil nil #'string=)))
          (switch-to-buffer buf))
      (user-error "No kubed buffers found"))))

(defun kubed-ext--buffer-switch-candidate (buf)
  "Return a human-readable completion candidate string for BUF."
  (with-current-buffer buf
    (cond
     ((derived-mode-p 'kubed-list-mode)
      (format "%-14s  %s"
              (or (bound-and-true-p kubed-list-type) "")
              (buffer-name buf)))
     ((derived-mode-p 'kubed-ext-wide-mode)
      (format "%-14s  %s"
              (format "wide:%s" (or kubed-ext-wide--type ""))
              (buffer-name buf)))
     ((derived-mode-p 'kubed-ext-top-nodes-mode)
      (format "%-14s  %s"
              (format "top:nodes@%s" (or kubed-ext-top-context ""))
              (buffer-name buf)))
     ((derived-mode-p 'kubed-ext-top-pods-mode)
      (format "%-14s  %s"
              (format "top:pods@%s" (or kubed-ext-top-namespace ""))
              (buffer-name buf)))
     ((derived-mode-p 'kubed-ext-port-forwards-mode)
      (format "%-14s  %s" "portfwds" (buffer-name buf)))
     (t (buffer-name buf)))))

(defun kubed-ext--log-kubectl-command (cmd-str)
  "Log CMD-STR to the kubectl command log buffer.
Creates the buffer if it does not exist."
  (setq kubed-ext--last-kubectl-command cmd-str)
  (let ((buf (get-buffer-create "*kubed-command-log*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'special-mode)
        (special-mode))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format "[%s] %s\n"
                        (format-time-string "%H:%M:%S") cmd-str))
        (cl-incf kubed-ext--command-log-lines)
        (when (> kubed-ext--command-log-lines 500)
          (goto-char (point-min))
          (forward-line 100)
          (delete-region (point-min) (point))
          (cl-decf kubed-ext--command-log-lines 100))))))

(defun kubed-ext-show-command-log ()
  "Show the kubed command log buffer."
  (interactive)
  (let ((buf (get-buffer-create "*kubed-command-log*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'special-mode) (special-mode)))
    (display-buffer buf)))

;;; ─── Process buffer access ────────────────────────────────────

(defun kubed-ext-show-process-buffer ()
  "Show the kubectl process/stderr buffer for the current list resource."
  (interactive nil kubed-list-mode)
  (let* ((type kubed-list-type)
         (buf  (or (get-buffer (format " *kubed-get-%s-stderr*" type))
                   (get-buffer (format "*kubed-get-%s*"         type))
                   (get-buffer (format " *kubed-get-%s*"        type)))))
    (if (and buf (buffer-live-p buf))
        (display-buffer buf)
      (user-error "No process buffer found for %s" type))))

;;; ═══════════════════════════════════════════════════════════════
;;; § 18.  Keybindings
;;; ═══════════════════════════════════════════════════════════════

;; ── Pods ──────────────────────────────────────────────────────────
(kubed-ext--set-key 'kubed-pods-mode-map  "T" #'kubed-ext-top-pods)
(kubed-ext--set-key 'kubed-pods-mode-map  "M" #'kubed-ext-top-pods)
(kubed-ext--set-key 'kubed-pods-mode-map  "v" #'kubed-ext-pods-vterm)
(kubed-ext--set-key 'kubed-pods-mode-map  "t" #'kubed-ext-pods-eat)
(kubed-ext--set-key 'kubed-pods-mode-map  "G" #'kubed-ext-pods-ghostel)
(kubed-ext--set-key 'kubed-pods-mode-map  "i" #'kubed-ext-pods-logs-init-container)
(kubed-ext--set-key 'kubed-pod-prefix-map "T" #'kubed-ext-top-pods)
(kubed-ext--set-key 'kubed-pod-prefix-map "v" #'kubed-ext-vterm-pod)
(kubed-ext--set-key 'kubed-pod-prefix-map "t" #'kubed-ext-eat-pod)
(kubed-ext--set-key 'kubed-pod-prefix-map "G" #'kubed-ext-ghostel-pod)
(kubed-ext--set-key 'kubed-pod-prefix-map "S" #'kubed-ext-eshell-pod)
(kubed-ext--set-key 'kubed-pod-prefix-map "i" #'kubed-ext-logs-init-container)

;; ── Nodes ─────────────────────────────────────────────────────────
(kubed-ext--set-key 'kubed-nodes-mode-map  "M" #'kubed-ext-top-nodes)
(kubed-ext--set-key 'kubed-node-prefix-map "T" #'kubed-ext-top-nodes)

;; ── Services ──────────────────────────────────────────────────────
(kubed-ext--set-key 'kubed-services-mode-map  "F" #'kubed-ext-services-forward-port)
(kubed-ext--set-key 'kubed-service-prefix-map "F" #'kubed-ext-forward-port-to-service)

;; ── Deployments ───────────────────────────────────────────────────
(kubed-ext--set-key 'kubed-deployments-mode-map "F" #'kubed-ext-deployments-forward-port)
(kubed-ext--set-key 'kubed-deployments-mode-map "r" #'kubed-ext-deployments-rollout-history)
(kubed-ext--set-key 'kubed-deployments-mode-map "j" #'kubed-ext-deployments-jab)
(kubed-ext--set-key 'kubed-deployments-mode-map "U" #'kubed-ext-deployments-rollout-undo)
(kubed-ext--set-key 'kubed-deployments-mode-map "M-u" #'kubed-ext-unmark-all)
(kubed-ext--set-key 'kubed-deployment-prefix-map "F" #'kubed-ext-forward-port-to-deployment)
(kubed-ext--set-key 'kubed-deployment-prefix-map "r" #'kubed-ext-rollout-history)
(kubed-ext--set-key 'kubed-deployment-prefix-map "j" #'kubed-ext-jab-deployment)
(kubed-ext--set-key 'kubed-deployment-prefix-map "U" #'kubed-ext-rollout-undo)

;; ── kubed-list-mode (parent of all list modes) ────────────────────
;;   "M-d" describes the resource at point; "d" is kubed's native mark-for-deletion.
(kubed-ext--set-key 'kubed-list-mode-map "M-d" #'kubed-ext-list-describe-resource)
(kubed-ext--set-key 'kubed-list-mode-map "S"   #'kubed-ext-list-set-label-selector)
(kubed-ext--set-key 'kubed-list-mode-map "c"   #'kubed-ext-copy-popup)
(kubed-ext--set-key 'kubed-list-mode-map "b"   #'kubed-ext-switch-buffer)
(kubed-ext--set-key 'kubed-list-mode-map "f"   #'kubed-ext-set-filter)
(kubed-ext--set-key 'kubed-list-mode-map "V"   #'kubed-ext-list-wide)
(kubed-ext--set-key 'kubed-list-mode-map "m"   #'kubed-ext-mark-item)
(kubed-ext--set-key 'kubed-list-mode-map "M-m" #'kubed-ext-mark-all)
(kubed-ext--set-key 'kubed-list-mode-map "M-u" #'kubed-ext-unmark-all)
(kubed-ext--set-key 'kubed-list-mode-map "M-n" #'kubed-ext-jump-to-next-highlight)
(kubed-ext--set-key 'kubed-list-mode-map "M-p" #'kubed-ext-jump-to-previous-highlight)
(kubed-ext--set-key 'kubed-list-mode-map "X"   #'kubed-ext-delete-marked)
(kubed-ext--set-key 'kubed-list-mode-map "A"   #'kubed-ext-auto-refresh-mode)
(kubed-ext--set-key 'kubed-list-mode-map "r"   #'kubed-ext-switch-resource)
(kubed-ext--set-key 'kubed-list-mode-map "$"   #'kubed-ext-show-process-buffer)

;; ── kubed-prefix-map ──────────────────────────────────────────────
;;   "d" belongs to deployments; use "k" (kubectl describe mnemonic).
(kubed-ext--set-key 'kubed-prefix-map "D" #'kubed-ext-describe-resource)
(kubed-ext--set-key 'kubed-prefix-map "F" #'kubed-ext-list-port-forwards)
(kubed-ext--set-key 'kubed-prefix-map "b" #'kubed-ext-switch-buffer)
(kubed-ext--set-key 'kubed-prefix-map "#" #'kubed-ext-show-command-log)
(kubed-ext--set-key 'kubed-prefix-map "l L" #'kubed-ext-logs-by-label)

(with-eval-after-load 'kubed
  (when (boundp 'kubed-deployments-mode-map)
    (kubed-ext--set-key 'kubed-deployments-mode-map "M-u" #'kubed-ext-unmark-all)))

;;; ═══════════════════════════════════════════════════════════════
;;; § 19.  Transient Navigation + Resource Actions
;;; ═══════════════════════════════════════════════════════════════

(defvar kubed-ext-extra-transient-suffixes
  '(("pods"        . [("T" "Top (metrics)"     kubed-ext-top-pods)
                      ("v" "Vterm"              kubed-ext-pods-vterm)
                      ("t" "Eat terminal"       kubed-ext-pods-eat)
                      ("G" "Ghostel"            kubed-ext-pods-ghostel)
                      ("E" "Eshell"             kubed-ext-pods-eshell)
                      ("a" "Ansi-term"          kubed-ext-pods-ansi-term)
                      ("&" "Exec command"       kubed-ext-pods-command)
                      ("i" "Init container logs" kubed-ext-pods-logs-init-container)])
    ("nodes"       . [("T" "Top (metrics)"  kubed-ext-top-nodes)])
    ("services"    . [("F" "Forward Port"   kubed-ext-services-forward-port)])
    ("deployments" . [("F" "Forward Port"       kubed-ext-deployments-forward-port)
                      ("H" "Rollout History"    kubed-ext-deployments-rollout-history)
                      ("j" "Jab (bounce)"       kubed-ext-deployments-jab)
                      ("u" "Rollout Undo"       kubed-ext-deployments-rollout-undo)]))
  "Additional per-resource-type transient suffixes.")

(defun kubed-ext-switch-context ()
  "Switch kubectl context and refresh current resource list."
  (interactive)
  (when (derived-mode-p 'kubed-list-mode)
    (let* ((type kubed-list-type)
           (ns kubed-list-namespace)
           (ctx (kubed-read-context "Switch to context")))
      ;; Immediately start prefetching namespaces for the new context.
      (kubed-ext-prefetch-namespaces ctx)
      (when type
        (let ((fn (intern (format "kubed-list-%s" type))))
          (when (fboundp fn)
            (cond
             ;; Non-namespaced resource type.
             ((not (kubed-ext--resource-namespaced-p type ctx))
              (funcall fn ctx))
             ;; Reuse current namespace.
             (ns (funcall fn ctx ns))
             ;; Let kubed resolve the namespace without blocking this command.
             (t
              (funcall fn ctx (kubed--namespace ctx))))))))))

(defun kubed-ext-switch-namespace ()
  "Switch namespace and refresh current resource list."
  (interactive)
  (when (derived-mode-p 'kubed-list-mode)
    (kubed-ext-prefetch-namespaces kubed-list-context)
    (let ((type kubed-list-type)
          (ctx kubed-list-context)
          (ns (kubed-read-namespace "Switch to namespace" nil nil
                                    kubed-list-context)))
      (when type
        (let ((fn (intern (format "kubed-list-%s" type))))
          (when (fboundp fn)
            (if (kubed-ext--resource-namespaced-p type ctx)
                (funcall fn ctx ns)
              (funcall fn ctx))))))))

(defun kubed-ext-switch-resource ()
  "Switch to a different resource type in the same context/namespace."
  (interactive)
  (when (derived-mode-p 'kubed-list-mode)
    (let* ((ctx kubed-list-context)
           (ns kubed-list-namespace)
           ;; Ensure the CRD cache is loaded for this context before building
           ;; the choices list.  This is a no-op when already loaded, but it
           ;; matters for freshly-opened cluster buffers where async discovery
           ;; may not have completed yet.
           (_ (kubed-ext--load-crd-cache ctx))
           (_ (kubed-ext-discover-crds-async ctx))
           (choices (kubed-ext--resource-choices ctx))
           (sel (completing-read "Switch to resource: " choices nil t))
           (type (alist-get sel choices nil nil #'string=)))
      (when type
        (let ((fn (intern (format "kubed-list-%s" type))))
          (if (fboundp fn)
              (if (kubed-ext--resource-namespaced-p type ctx)
                  (funcall fn ctx (or ns (kubed--namespace ctx)))
                (funcall fn ctx))
            ;; CRD function not yet defined — cache may still be loading.
            ;; Trigger a forced refresh so subsequent attempts succeed.
            (message "kubed-ext: resource `%s' not ready; refreshing CRD cache..." type)
            (kubed-ext-discover-crds-async
             ctx t
             (lambda (count)
               (message "kubed-ext: CRD cache refreshed (%d resource%s); try switching again."
                        count (if (= count 1) "" "s"))))))))))

(defun kubed-ext-jump-pods ()
  "Jump to Pods."
  (interactive)
  (kubed-list-pods kubed-list-context kubed-list-namespace))

(defun kubed-ext-jump-deployments ()
  "Jump to Deployments."
  (interactive)
  (kubed-list-deployments kubed-list-context kubed-list-namespace))

(defun kubed-ext-jump-services ()
  "Jump to Services."
  (interactive)
  (kubed-list-services kubed-list-context kubed-list-namespace))

(defun kubed-ext-jump-jobs ()
  "Jump to Jobs."
  (interactive)
  (kubed-list-jobs kubed-list-context kubed-list-namespace))

(defun kubed-ext-jump-configmaps ()
  "Jump to ConfigMaps."
  (interactive)
  (kubed-list-configmaps kubed-list-context kubed-list-namespace))

(defun kubed-ext-jump-secrets ()
  "Jump to Secrets."
  (interactive)
  (kubed-list-secrets kubed-list-context kubed-list-namespace))

(defun kubed-ext-jump-ingresses ()
  "Jump to Ingresses."
  (interactive)
  (kubed-list-ingresses kubed-list-context kubed-list-namespace))

(defun kubed-ext-jump-pvcs ()
  "Jump to PVCs."
  (interactive)
  (kubed-list-persistentvolumeclaims kubed-list-context kubed-list-namespace))

(defun kubed-ext--collect-action-groups ()
  "Build transient group vectors for the current resource type."
  (let* ((type (or kubed-list-type ""))
         (type-label (capitalize type))
         (upstream (bound-and-true-p kubed-list-transient-extra-suffixes))
         (custom (alist-get type kubed-ext-extra-transient-suffixes
                            nil nil #'string=))
         (all (append (when upstream upstream) (when custom (list custom))))
         (result nil)
         (gidx 0))
    (dolist (gv all)
      (cl-incf gidx)
      (let* ((entries (if (vectorp gv) (append gv nil) gv))
             (kw-args nil)
             (suffixes nil)
             (rest entries))
        (while rest
          (cond
           ((keywordp (car rest))
            (push (pop rest) kw-args)
            (when rest (push (pop rest) kw-args)))
           ((and (listp (car rest))
                 (>= (length (car rest)) 3)
                 (stringp (nth 0 (car rest)))
                 (stringp (nth 1 (car rest))))
            (push (pop rest) suffixes))
           (t (pop rest))))
        (setq kw-args (nreverse kw-args)
              suffixes (nreverse suffixes))
        (when suffixes
          (push (apply #'vector
                       (format "%s%s" type-label
                               (if (> (length all) 1)
                                   (format " (%d)" gidx) ""))
                       (append kw-args suffixes))
                result))))
    (nreverse result)))

(defvar kubed-ext--dynamic-menu-cache-key nil
  "Last dynamic transient shape used for `kubed-ext--dynamic-menu'.")

(declare-function kubed-ext--dynamic-menu "kubed-ext")

(defun kubed-ext-transient-menu ()
  "Show transient combining navigation, actions, and utilities."
  (interactive)
  (require 'transient)
  (require 'kubed-transient nil t)
  (let* ((in-list (derived-mode-p 'kubed-list-mode))
         (type (and in-list kubed-list-type))
         (agroups (and in-list (kubed-ext--collect-action-groups)))
         (cache-key (list in-list type agroups)))
    (condition-case err
        (progn
          (unless (equal cache-key kubed-ext--dynamic-menu-cache-key)
            (eval
             `(transient-define-prefix kubed-ext--dynamic-menu ()
                ,(format "Kubed %s" (or type "Navigation"))
                ,@(if in-list
                      '([["Scope"
                          ("c" "Context"       kubed-ext-switch-context)
                          ("n" "Namespace"     kubed-ext-switch-namespace)
                          ("r" "Resource type" kubed-ext-switch-resource)]
                         ["Workloads"
                          ("1" "Pods"        kubed-ext-jump-pods)
                          ("2" "Deployments" kubed-ext-jump-deployments)
                          ("3" "Services"    kubed-ext-jump-services)
                          ("4" "Jobs"        kubed-ext-jump-jobs)]
                         ["Config & Network"
                          ("5" "ConfigMaps" kubed-ext-jump-configmaps)
                          ("6" "Secrets"    kubed-ext-jump-secrets)
                          ("7" "Ingresses"  kubed-ext-jump-ingresses)
                          ("8" "PVCs"       kubed-ext-jump-pvcs)]])
                    '([["Open Resource List"
                        ("1" "Pods"        kubed-list-pods)
                        ("2" "Deployments" kubed-list-deployments)
                        ("3" "Services"    kubed-list-services)
                        ("4" "Jobs"        kubed-list-jobs)
                        ("5" "ConfigMaps"  kubed-list-configmaps)
                        ("6" "Secrets"     kubed-list-secrets)]]))
                ,@(cond
                   ((null agroups) nil)
                   ((= (length agroups) 1) agroups)
                   ((<= (length agroups) 3)
                    (list (apply #'vector agroups)))
                   (t (let ((rows nil) (rest agroups))
                        (while rest
                          (let ((chunk (seq-take rest 3)))
                            (push (if (= (length chunk) 1) (car chunk)
                                    (apply #'vector chunk))
                                  rows)
                            (setq rest (seq-drop rest 3))))
                        (nreverse rows))))
                ,@(when in-list
                    '([["View"
                        ("g" "Refresh"        kubed-list-update :transient t)
                        ("M-d" "Describe"       kubed-ext-list-describe-resource)
                        ("V" "Wide view"      kubed-ext-list-wide)
                        ("A" "Auto-refresh"   kubed-ext-auto-refresh-mode)
                        ("q" "Quit window"    quit-window)]
                       ["Filter"
                        ("/" "Column filter"  kubed-list-set-filter)
                        ("f" "Text highlight" kubed-ext-set-filter)
                        ("S" "Label selector" kubed-ext-list-set-label-selector)]
                       ["Copy & Mark"
                        ("b" "Switch buffer"  kubed-ext-switch-buffer)
                        ("w" "Copy row"       kubed-list-copy-as-kill)
                        ("y" "Copy menu"      kubed-ext-copy-popup)
                        ("#" "Command log"    kubed-ext-show-command-log)
                        ("m" "Mark item"      kubed-ext-mark-item)
                        ("X" "Delete marked"  kubed-ext-delete-marked)]])))
             t)
            (setq kubed-ext--dynamic-menu-cache-key cache-key))
          (funcall-interactively #'kubed-ext--dynamic-menu))
      (error
       (message "Dynamic transient error: %s" (error-message-string err))
       (when (fboundp 'kubed-list-transient)
         (call-interactively #'kubed-list-transient))))))

(kubed-ext--set-key 'kubed-list-mode-map "M-?" #'kubed-ext-transient-menu)
(kubed-ext--set-key 'kubed-list-mode-map "C" #'kubed-ext-switch-context)
(kubed-ext--set-key 'kubed-list-mode-map "N" #'kubed-ext-switch-namespace)
(kubed-ext--set-key 'kubed-list-mode-map "R" #'kubed-ext-switch-resource)

;;; ═══════════════════════════════════════════════════════════════
;;; § 20.  Kill All Kubed Buffers
;;; ═══════════════════════════════════════════════════════════════

(defun kubed-ext-kill-all-buffers (&optional no-confirm)
  "Kill all Kubed-related buffers.

Buffers are matched either by their major/minor mode (e.g.
`kubed-list-mode', `kubed-ext-wide-mode', etc.) or by their name
starting with \"*Kubed\" or \"*kubed\" (case-insensitive), including
internal space-prefixed scratch buffers such as \" *kubed-get-*\".

With a prefix argument NO-CONFIRM, skip the confirmation prompt and
kill immediately.

Also stops any active port-forward processes and cancels the
auto-refresh timer if it is running."
  (interactive "P")
  (let ((kubed-bufs
         (seq-filter
          (lambda (buf)
            (or
             ;; ── Detect by major / minor mode ──────────────────
             (with-current-buffer buf
               (or (derived-mode-p 'kubed-list-mode)
                   (derived-mode-p 'kubed-ext-wide-mode)
                   (derived-mode-p 'kubed-ext-top-nodes-mode)
                   (derived-mode-p 'kubed-ext-top-pods-mode)
                   (derived-mode-p 'kubed-ext-port-forwards-mode)
                   (derived-mode-p 'kubed-ext-rollout-history-mode)
                   (bound-and-true-p kubed-display-resource-mode)))
             ;; ── Detect by buffer-name pattern ─────────────────
             ;; Matches: *Kubed ...  *kubed-...  *kubed rollout...
             ;;          " *kubed-get-*" (internal scratch buffers)
             (string-match-p
              "\\`[[:space:]]*\\*[Kk]ubed"
              (buffer-name buf))))
          (buffer-list))))
    (if (null kubed-bufs)
        (message "No Kubed buffers to kill.")
      (when (or no-confirm
                (yes-or-no-p
                 (format "Kill %d Kubed buffer%s? "
                         (length kubed-bufs)
                         (if (= (length kubed-bufs) 1) "" "s"))))
        ;; ── Cancel auto-refresh timer ──────────────────────────
        (when (fboundp 'kubed-ext--auto-refresh-cancel-timer)
          (kubed-ext--auto-refresh-cancel-timer))
        ;; ── Kill buffers ───────────────────────────────────────
        (let ((killed 0))
          (dolist (buf kubed-bufs)
            (when (buffer-live-p buf)
              (kill-buffer buf)
              (cl-incf killed)))
          (message "Killed %d Kubed buffer%s."
                   killed
                   (if (= killed 1) "" "s")))))))

(kubed-ext--set-key 'kubed-prefix-map "Q" #'kubed-ext-kill-all-buffers)
(kubed-ext--set-key 'kubed-list-mode-map "C-c C-k" #'kubed-ext-kill-all-buffers)

;;; ═══════════════════════════════════════════════════════════════
;;; § 21.  Circuit Breaker — Intelligent VPN / Network Recovery
;;; ═══════════════════════════════════════════════════════════════
;;
;; Each kubectl context has an independent circuit in one of two states:
;;
;;   closed  — normal; auto-refresh runs freely.
;;   network — cluster unreachable (VPN, DNS, TLS, timeout).
;;             Trips after `kubed-ext-cb-threshold' consecutive network
;;             errors.  Recovery probes with exponential back-off are
;;             scheduled automatically; circuit closes on probe success.
;;
;; Network outages suppress auto-refresh.  Authentication failures are shown
;; as transient headers only; each refresh runs a new kubectl process, so exec
;; credential plugins such as kubelogin/Azure CLI get a fresh chance to renew.
;; Mode-line indicator:
;;   ⚡~Xs     network outage  (X = seconds until next probe)

(defgroup kubed-ext-circuit-breaker nil
  "Per-context circuit breaker for kubed-ext auto-refresh."
  :group 'kubed-ext)

(defcustom kubed-ext-cb-threshold 3
  "Consecutive network failures before the circuit opens for a context."
  :type 'natnum
  :group 'kubed-ext-circuit-breaker)

(defcustom kubed-ext-cb-initial-backoff 30
  "Seconds before the first network-recovery probe after the circuit opens."
  :type 'natnum
  :group 'kubed-ext-circuit-breaker)

(defcustom kubed-ext-cb-backoff-factor 2.0
  "Multiplier applied to the probe interval after each failed probe."
  :type 'number
  :group 'kubed-ext-circuit-breaker)

(defcustom kubed-ext-cb-max-backoff 300
  "Maximum probe interval in seconds (default: 5 minutes)."
  :type 'natnum
  :group 'kubed-ext-circuit-breaker)

;; ── Per-context state (all tables keyed by context string) ───────

(defvar kubed-ext--cb-open
  (make-hash-table :test 'equal)
  "Circuit state per context: nil (closed) or `network'.")

(defvar kubed-ext--cb-net-failures
  (make-hash-table :test 'equal)
  "Consecutive network-error count per context.")

(defvar kubed-ext--cb-backoff
  (make-hash-table :test 'equal)
  "Current probe back-off interval in seconds per context.")

(defvar kubed-ext--cb-probe-timers
  (make-hash-table :test 'equal)
  "Pending network-recovery probe timer per context.")

;; ── Error pattern matching ────────────────────────────────────────

(defconst kubed-ext--network-error-patterns
  '("EOF" "unexpected EOF" "connection reset by peer" "connection refused"
    "network is unreachable" "no route to host" "broken pipe"
    "no such host" "Name or service not known"
    "Temporary failure in name resolution"
    "i/o timeout" "TLS handshake timeout" "context deadline exceeded"
    "request timeout" "Unable to connect to the server"
    "transport: Error while dialing" "dial tcp" "proxy: "
    "azmk8s.io" "googleapis.com" "eks.amazonaws.com"
    "certificate has expired")
  "Kubectl error substrings indicating transient network or VPN failures.")

(defconst kubed-ext--auth-error-patterns
  '("AzureCLICredential" "kubelogin failed" "AADSTS"
    "failed to get token" "getting credentials: exec"
    "exec: executable kubelogin" "TokenRequestError"
    "Run the command below" "authenticate interactively"
    "additional arguments may be added"
    "invalid_client" "Unauthorized" "unauthorized"
    "az login" "az logout")
  "Kubectl error substrings indicating expired or invalid credentials.")

(defconst kubed-ext--context-error-patterns
  '("Error:" "failed to get token" "AzureCLICredential" "AADSTS"
    "kubelogin failed" "Run the command below"
    "authenticate interactively" "additional arguments may be added"
    "context \"" "does not exist")
  "Substrings that indicate a value is an error, not a context name.")

(defconst kubed-ext--context-error-regexp
  (regexp-opt kubed-ext--context-error-patterns)
  "Regexp matching error strings that are not valid context names.")

(defun kubed-ext--network-error-p (msg)
  "Return non-nil if MSG indicates a transient network or VPN failure."
  (and (stringp msg)
       (seq-some (lambda (pat)
                   (string-match-p (regexp-quote pat) msg))
                 kubed-ext--network-error-patterns)))

(defun kubed-ext--auth-error-p (msg)
  "Return non-nil if MSG indicates an expired or invalid credential."
  (and (stringp msg)
       (seq-some (lambda (pat)
                   (string-match-p (regexp-quote pat) msg))
                 kubed-ext--auth-error-patterns)))

(defun kubed-ext--context-error-string-p (context)
  "Return non-nil if CONTEXT is an error string."
  (and (stringp context)
       (string-match-p kubed-ext--context-error-regexp context)))

(defun kubed-ext--current-context-safe ()
  "Return a cached kubectl context, ignoring any auth noise in output.
This function deliberately avoids invoking kubectl synchronously so list
refresh and recovery paths never block the Emacs UI on credential, DNS,
VPN, or API-server delays.  If no safe cached context is available,
return nil instead of guessing a different cluster context."
  (let* ((eenv (kubed-ext--eenv-key))
         (cached (and (boundp 'kubed--context-and-namespace-alist)
                      (alist-get eenv kubed--context-and-namespace-alist
                                 nil nil #'equal)))
         (candidates (delq
                      nil
                      (list
                       (and (local-variable-p 'kubed-list-context)
                            kubed-list-context)
                       (and (local-variable-p 'kubed-display-resource-info)
                            (nth 2 kubed-display-resource-info))
                       (car-safe cached)))))
    (seq-find
     (lambda (context)
       (and (stringp context)
            (not (string-empty-p (string-trim context)))
            (not (kubed-ext--context-error-string-p context))))
     candidates)))

(defun kubed-ext--usable-context (context)
  "Return a context safe to pass to kubectl.
If CONTEXT has been poisoned with an error string, fall back to the
current kubectl context.  If no safe context can be found, return nil so
kubectl can use its configured current context without `--context'."
  (if (kubed-ext--context-error-string-p context)
      (kubed-ext--current-context-safe)
    context))

(defun kubed-ext--repair-current-list-context ()
  "Repair current list buffer if its context has an error string."
  (when (and (derived-mode-p 'kubed-list-mode)
             (kubed-ext--context-error-string-p kubed-list-context))
    (setq kubed-list-context (kubed-ext--usable-context kubed-list-context))))

(defvar kubed-ext--context-namespace-poisoned nil
  "Non-nil when a poisoned context/namespace cache entry has been detected.
Set by advice around the function `kubed-default-context-and-namespace'
after a failed resolution so the next call knows to re-sanitize.  Reset
to nil once sanitization runs successfully with nothing found.")

(defun kubed-ext--sanitize-cached-context-namespace ()
  "Clear poisoned entries from `kubed--context-and-namespace-alist'.
When kubectl fails (e.g. expired Azure token during `az login'), kubed
caches the error string as the context and/or namespace.  Each entry is
\(EENV CONTEXT . NAMESPACE); this function removes entries whose context
or namespace is actually a kubectl error string so kubed re-resolves on
the next call.  Return non-nil when any entries were cleared."
  (when (boundp 'kubed--context-and-namespace-alist)
    (let ((poisoned nil))
      (dolist (entry kubed--context-and-namespace-alist)
        ;; ENTRY is (EENV CONTEXT . NAMESPACE): car=eenv, cadr=context,
        ;; cddr=namespace.
        (when (or (kubed-ext--context-error-string-p (cadr entry))
                  (kubed-ext--context-error-string-p (cddr entry)))
          (push entry poisoned)))
      (cond
       (poisoned
        (dolist (bad poisoned)
          (setf (alist-get (car bad)
                           kubed--context-and-namespace-alist
                           nil 'remove #'equal)
                nil))
        (message "Kubed-ext: cleared %d poisoned context/namespace cache %s"
                 (length poisoned)
                 (if (= (length poisoned) 1) "entry" "entries"))
        t)
       (t
        (setq kubed-ext--context-namespace-poisoned nil)
        nil)))))

(defun kubed-ext--around-default-context-and-namespace (orig-fn &rest args)
  "Advice around the function `kubed-default-context-and-namespace'.
Before calling ORIG-FN with ARGS, clear any cached entries where the
context or namespace is actually a kubectl error string.  If the fresh
result is still poisoned (kubectl failed again), clear it and return nil
so the caller can retry later.

Sanitization is skipped when no poisoning has been detected, keeping the
hot path (cache hit with valid data) free of extra work."
  (when kubed-ext--context-namespace-poisoned
    (kubed-ext--sanitize-cached-context-namespace))
  (let ((result (apply orig-fn args)))
    (if (and (consp result)
             (or (kubed-ext--context-error-string-p (car result))
                 (kubed-ext--context-error-string-p (cdr result))))
        ;; Result is poisoned — flag it so the next call re-sanitizes,
        ;; and clear the cached entry now.
        (progn
          (setq kubed-ext--context-namespace-poisoned t)
          (kubed-ext--sanitize-cached-context-namespace)
          nil)
      result)))

(advice-add 'kubed-default-context-and-namespace
            :around #'kubed-ext--around-default-context-and-namespace)

;; Arm the flag at load time so the first call checks for a poisoned cache
;; inherited from a previous Emacs session or an earlier failed kubectl call.
(setq kubed-ext--context-namespace-poisoned t)

;; ── State accessors ───────────────────────────────────────────────

(defun kubed-ext--cb-open-p (context)
  "Return open reason for CONTEXT, or nil if closed."
  (gethash (or context "") kubed-ext--cb-open))

(defun kubed-ext--cb-backoff-for (context)
  "Return the current probe back-off interval in seconds for CONTEXT."
  (gethash (or context "") kubed-ext--cb-backoff
           kubed-ext-cb-initial-backoff))

;; ── Stderr content discovery ──────────────────────────────────────

(defun kubed-ext--cb-read-stderr (resource-type)
  "Return stderr content for a `kubectl get RESOURCE-TYPE' process, or nil.
Tries several buffer-naming conventions that kubed may use."
  (catch 'found
    (dolist (name (list (format " *kubed-get-%s-stderr*" resource-type)
                        (format "*kubed-get-%s*"         resource-type)
                        (format " *kubed-get-%s*"        resource-type)))
      (let ((buf (get-buffer name)))
        (when (and buf (buffer-live-p buf))
          (let ((content (with-current-buffer buf
                           (string-trim (buffer-string)))))
            (unless (string-empty-p content)
              (throw 'found content))))))))

;; ── List-buffer helpers ───────────────────────────────────────────

(defvar kubed-ext--known-large-lists (make-hash-table :test 'equal)
  "Resource list keys previously observed at or above the large-list threshold.")

(defun kubed-ext--large-list-key (type context namespace &optional eenv)
  "Return the auto-refresh cache key for TYPE/CONTEXT/NAMESPACE/EENV."
  (list (or eenv "") (or type "") (or context "") (or namespace "")))

(defun kubed-ext--cached-resource-count (type context namespace &optional eenv)
  "Return cached row count for TYPE/CONTEXT/NAMESPACE in EENV."
  (length (alist-get 'resources (kubed--alist eenv type context namespace))))

(defun kubed-ext--remember-large-list-if-needed
    (type context namespace eenv count)
  "Remember TYPE/CONTEXT/NAMESPACE/EENV as large when COUNT reaches threshold."
  (when (and kubed-ext-auto-refresh-large-list-threshold
             (>= count kubed-ext-auto-refresh-large-list-threshold))
    (puthash (kubed-ext--large-list-key type context namespace eenv)
             t kubed-ext--known-large-lists)))

(defun kubed-ext--large-list-auto-refresh-skip-p (type context namespace
                                                       &optional eenv)
  "Return non-nil when auto-refresh should skip TYPE/CONTEXT/NAMESPACE/EENV.
A list is skipped when its current cache is large or when it was
previously observed as large in this Emacs session."
  (and kubed-ext-auto-refresh-large-list-threshold
       (or (>= (kubed-ext--cached-resource-count type context namespace eenv)
               kubed-ext-auto-refresh-large-list-threshold)
           (gethash (kubed-ext--large-list-key type context namespace eenv)
                    kubed-ext--known-large-lists))))

(defun kubed-ext--server-side-list-resource-p (type context)
  "Return non-nil when TYPE in CONTEXT should use server-side table output."
  (or (kubed-ext--get-crd-metadata type context)
      (seq-some (lambda (regexp)
                  (string-match-p regexp type))
                kubed-ext-server-side-list-resource-regexps)))

(defun kubed-ext--list-command
    (type context namespace columns server-side-table selector)
  "Return kubectl get command for listing TYPE.
CONTEXT and NAMESPACE are optional.  COLUMNS is the custom-column spec
used when SERVER-SIDE-TABLE is nil.  SELECTOR is an optional label
selector."
  (append
   (list (kubed-kubectl-program) "get" type)
   (when context (list "--context" context))
   (if server-side-table
       (list "--server-print=true")
     (list (format "--output=custom-columns=%s"
                   (mapconcat #'car columns ","))))
   (when namespace (list "--namespace" namespace))
   (when selector (list "--selector" selector))))

(defun kubed-ext--server-table-column-widths (headers rows)
  "Return a list of display widths for HEADERS over ROWS in one pass.
Each width is clamped to the 6..48 range.  Computing all column widths
together keeps cost at O(rows*columns) instead of O(rows*columns^2)."
  (let ((widths (mapcar #'string-width headers)))
    (dolist (row rows)
      (let ((ws widths)
            (cells row))
        (while (and ws cells)
          (let ((w (string-width (or (car cells) ""))))
            (when (> w (car ws))
              (setcar ws w)))
          (setq ws (cdr ws)
                cells (cdr cells)))))
    (mapcar (lambda (w) (max 6 (min 48 w))) widths)))

(defun kubed-ext--server-table-format (headers rows)
  "Return `tabulated-list-format' vector for server table HEADERS and ROWS."
  (apply #'vector
         (cl-loop for header in headers
                  for width in (kubed-ext--server-table-column-widths
                                headers rows)
                  collect (list (capitalize header) width t))))

(defun kubed-ext--server-table-entries (headers rows type)
  "Return tabulated entries from server table HEADERS/ROWS for TYPE.
Column colorize kinds are classified once per HEADERS and reused for
every row, and ROWS are consumed in lockstep with the precomputed kinds
so per-cell cost stays O(columns) instead of O(columns squared)."
  (let ((kinds (mapcar #'kubed-ext--column-kind headers)))
    (mapcar (lambda (row)
              (list (or (car row) "")
                    (apply #'vector
                           (cl-loop for cell in row
                                    for kind in kinds
                                    collect (kubed-ext--colorize-cell-by-kind
                                             kind (or cell "") type)))))
            rows)))

(defun kubed-ext--matching-list-buffer-p (buf type context namespace eenv)
  "Return non-nil when BUF displays TYPE/CONTEXT/NAMESPACE in EENV.
Buffer matching mirrors kubed 0.7.0's `kubed--list-buffers', comparing
the full execution-environment key rather than only the remote host."
  (and (buffer-live-p buf)
       (equal (buffer-local-value 'kubed-list-type buf) type)
       (equal (buffer-local-value 'kubed-list-context buf) context)
       (equal (buffer-local-value 'kubed-list-namespace buf) namespace)
       (equal (kubed-ext--eenv-key buf) eenv)))

(defun kubed-ext--apply-list-update-format (format)
  "Install server-side list FORMAT in the current list buffer when non-nil."
  (when format
    (setq tabulated-list-format format)
    (tabulated-list-init-header)))

(defun kubed-ext--set-stale-list-buffer (stale)
  "Set current buffer's stale-list state to STALE and maintain the count."
  (unless (eq kubed-ext--stale-list-buffer-p stale)
    (setq kubed-ext--stale-list-buffer-p stale)
    (setq kubed-ext--stale-list-buffer-count
          (max 0 (+ kubed-ext--stale-list-buffer-count (if stale 1 -1))))))

(defun kubed-ext--refresh-list-buffers-after-update
    (type context namespace eenv &optional format)
  "Revert visible list buffers for TYPE/CONTEXT/NAMESPACE in EENV.
When FORMAT is non-nil, install it as the buffer-local table format first.
Hidden matching buffers are marked stale and re-rendered lazily when shown."
  (let ((bufs nil))
    (kubed-ext--for-each-list-buffer
     (lambda (buf)
       (when (kubed-ext--matching-list-buffer-p buf type context namespace eenv)
         (if-let ((win (get-buffer-window buf t)))
             (progn
               (kubed-ext--apply-list-update-format format)
               (kubed-ext--set-stale-list-buffer nil)
               (revert-buffer)
               (kubed-ext--clear-header-error)
               (set-window-point win (point))
               (push buf bufs))
           (when format
             (setq tabulated-list-format format))
           (kubed-ext--set-stale-list-buffer t)))))
    (walk-windows
     (lambda (win)
       (let ((buf (window-buffer win)))
         (when (memq buf bufs)
           (set-window-point
            win (with-current-buffer buf (point)))))))))

(defvar kubed-ext--stale-refresh-timer nil
  "Idle timer used to debounce stale list buffer re-rendering.
The `window-configuration-change-hook' fires very often (resize, split,
scroll); this timer coalesces those events into a single idle callback.")

(defun kubed-ext--refresh-stale-visible-list-buffers ()
  "Re-render visible list buffers that were marked stale while hidden."
  (when (> kubed-ext--stale-list-buffer-count 0)
    (dolist (win (window-list))
      (let ((buf (window-buffer win)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (when (and kubed-ext--stale-list-buffer-p
                       (derived-mode-p 'kubed-list-mode))
              (kubed-ext--set-stale-list-buffer nil)
              (tabulated-list-init-header)
              (revert-buffer)
              (kubed-ext--clear-header-error))))))))

(defun kubed-ext--schedule-stale-refresh ()
  "Schedule a debounced idle callback to re-render stale list buffers.
Runs after 0.3 s of idle time so rapid window-configuration changes
\(resize drag, frame split) coalesce into a single pass."
  (when (> kubed-ext--stale-list-buffer-count 0)
    (when (timerp kubed-ext--stale-refresh-timer)
      (cancel-timer kubed-ext--stale-refresh-timer))
    (setq kubed-ext--stale-refresh-timer
          (run-with-idle-timer 0.3 nil
                               #'kubed-ext--refresh-stale-visible-list-buffers))))

(add-hook 'window-configuration-change-hook
          #'kubed-ext--schedule-stale-refresh)

(defun kubed-ext--cb-in-context-bufs (context fn)
  "Call FN with no args in every live kubed list buffer using CONTEXT."
  (let ((ctx (or context "")))
    (kubed-ext--for-each-list-buffer
     (lambda (_buf)
       (when (equal kubed-list-context ctx)
         (funcall fn))))))

(defun kubed-ext--cb-set-header (context message)
  "Display MESSAGE as the error header in every list buffer for CONTEXT."
  (kubed-ext--cb-in-context-bufs
   context (lambda () (kubed-ext--set-header-error message))))

(defun kubed-ext--cb-clear-headers (context)
  "Clear circuit-breaker header overlays in every list buffer for CONTEXT."
  (kubed-ext--cb-in-context-bufs
   context #'kubed-ext--clear-header-error))

(defun kubed-ext--cb-refresh-mode-lines (context)
  "Force mode-line update in every kubed list buffer for CONTEXT."
  (kubed-ext--cb-in-context-bufs
   context #'force-mode-line-update))

(defun kubed-ext--cb-refresh-context-lists (context)
  "Refresh each visible TYPE/NAMESPACE list in CONTEXT at most once."
  (let ((ctx (or context ""))
        (seen (make-hash-table :test 'equal)))
    (kubed-ext--for-each-list-buffer
     (lambda (_buf)
       (when (equal kubed-list-context ctx)
         (let ((key (list kubed-list-type kubed-list-namespace)))
           (unless (gethash key seen)
             (puthash key t seen)
             (condition-case nil
                 (kubed-list-update t)
               (error nil)))))))))

;; ── Auth-error summary extraction ────────────────────────────────

(defun kubed-ext--cb-auth-summary (err-msg)
  "Extract a short human-readable summary from an auth error ERR-MSG."
  (let* ((lines (split-string (string-trim err-msg) "\n" t))
         (hint  (seq-find (lambda (l)
                            (string-match-p "az login\\|az logout" l))
                          lines)))
    (string-trim (or hint (car lines) "Credentials expired."))))

;; ── Core state machine ────────────────────────────────────────────

(defun kubed-ext--cb-reset-state (context)
  "Clear all circuit-breaker counters and cancel any probe timer for CONTEXT."
  (let* ((ctx   (or context ""))
         (timer (gethash ctx kubed-ext--cb-probe-timers)))
    (puthash ctx 0   kubed-ext--cb-net-failures)
    (puthash ctx nil kubed-ext--cb-open)
    (puthash ctx kubed-ext-cb-initial-backoff kubed-ext--cb-backoff)
    (when (timerp timer) (cancel-timer timer))
    (remhash ctx kubed-ext--cb-probe-timers)))

(defun kubed-ext--cb-on-success (context)
  "Record a successful kubectl call; close the circuit for CONTEXT if open."
  (let* ((ctx      (or context ""))
         (was-open (kubed-ext--cb-open-p ctx)))
    (kubed-ext--cb-reset-state ctx)
    (when was-open
      (message "Kubed ✓  [%s]: connection restored — auto-refresh resumed." ctx)
      (kubed-ext--cb-clear-headers ctx)
      (kubed-ext--cb-refresh-mode-lines ctx)
      (kubed-ext--cb-refresh-context-lists ctx))))

(defun kubed-ext--cb-trip-network (context err-msg)
  "Open the circuit with reason `network' for CONTEXT/ERR-MSG."
  (let* ((ctx     (or context ""))
         (delay   kubed-ext-cb-initial-backoff)
         (summary (truncate-string-to-width
                   (replace-regexp-in-string "\n" " " (string-trim err-msg))
                   60 nil nil "…")))
    (puthash ctx 'network kubed-ext--cb-open)
    (puthash ctx delay    kubed-ext--cb-backoff)
    (message "Kubed ⚡ [%s]: unreachable — %s  [probing in %ds; `g'/`Z' to retry]"
             ctx summary delay)
    (kubed-ext--cb-refresh-mode-lines ctx)
    (kubed-ext--cb-schedule-probe ctx delay)))

(defun kubed-ext--show-auth-error (context err-msg)
  "Show an auth error for CONTEXT ERR-MSG without opening the refresh circuit."
  (let* ((ctx     (or context ""))
         (summary (kubed-ext--cb-auth-summary err-msg)))
    (kubed-ext--cb-reset-state ctx)
    (message "Kubed 🔑 [%s]: auth error — %s  [refresh retries credentials]"
             ctx summary)
    (kubed-ext--cb-set-header
     ctx (format "🔑 Auth error — %s  [Refresh will retry credentials]"
                 summary))
    (kubed-ext--cb-refresh-mode-lines ctx)))

(defun kubed-ext--cb-on-failure (context err-msg)
  "Record a kubectl failure for CONTEXT with error message ERR-MSG.
Network errors are counted toward the circuit threshold.  Auth errors
are displayed but do not pause refresh; the next kubectl call can renew
exec credentials.  Unknown errors are silently ignored."
  (let ((ctx (or context "")))
    (cond
     ;; Auth / credential error: let the next refresh retry kubectl's
     ;; exec credential flow instead of blocking the view.
     ((kubed-ext--auth-error-p err-msg)
      (kubed-ext--show-auth-error ctx err-msg))

     ;; ── Network / transport error ─────────────────────────────
     ;; Only act while the circuit is fully closed.
     ((and (kubed-ext--network-error-p err-msg)
           (not (kubed-ext--cb-open-p ctx)))
      (let* ((n   (1+ (gethash ctx kubed-ext--cb-net-failures 0)))
             (thr kubed-ext-cb-threshold))
        (puthash ctx n kubed-ext--cb-net-failures)
        (if (>= n thr)
            (kubed-ext--cb-trip-network ctx err-msg)
          nil))))))

;; ── Network recovery probe ────────────────────────────────────────

(defun kubed-ext--cb-schedule-probe (context delay)
  "Schedule a connectivity probe for CONTEXT after DELAY seconds."
  (let ((ctx (or context "")))
    (let ((old (gethash ctx kubed-ext--cb-probe-timers)))
      (when (timerp old) (cancel-timer old)))
    (puthash ctx
             (run-with-timer delay nil #'kubed-ext--cb-run-probe ctx)
             kubed-ext--cb-probe-timers)))

(defun kubed-ext--cb-run-probe (context)
  "Run a `kubectl api-versions' connectivity probe for CONTEXT.

exit 0         → success; close circuit.
auth error     → network is up but credentials failed; close circuit and
                 let normal refresh retry the exec credential flow.
network error  → still unreachable; double back-off and reschedule.
other error    → server responded; close circuit."
  (let ((ctx (or context "")))
    (when (kubed-ext--cb-open-p ctx)
      (let ((probe-buf (generate-new-buffer " *kubed-ext-cb-probe*")))
        (make-process
         :name     "kubed-ext-cb-probe"
         :buffer   probe-buf
         :noquery  t
         :command  (list (kubed-kubectl-program)
                         "api-versions"
                         "--context"         ctx
                         "--request-timeout" "5s")
         :sentinel
         (lambda (proc _event)
           (when (memq (process-status proc) '(exit signal))
             (let* ((code   (process-exit-status proc))
                    (output (if (buffer-live-p probe-buf)
                                (prog1 (with-current-buffer probe-buf
                                         (string-trim (buffer-string)))
                                  (kill-buffer probe-buf))
                              "")))
               (remhash ctx kubed-ext--cb-probe-timers)
               (cond
                ;; Network and auth are both working.
                ((zerop code)
                 (kubed-ext--cb-on-success ctx))

                ;; Network is up but credentials failed.  Close the network
                ;; circuit and let normal refresh retry authentication.
                ((kubed-ext--auth-error-p output)
                 (kubed-ext--show-auth-error ctx output))

                ;; Cluster still unreachable — exponential back-off.
                ((kubed-ext--network-error-p output)
                 (let* ((cur  (kubed-ext--cb-backoff-for ctx))
                        (next (min kubed-ext-cb-max-backoff
                                   (round (* cur kubed-ext-cb-backoff-factor)))))
                   (puthash ctx next kubed-ext--cb-backoff)
                   (kubed-ext--cb-refresh-mode-lines ctx)
                   (kubed-ext--cb-schedule-probe ctx next)))

                ;; Unexpected non-zero exit but server responded — close circuit.
                (t
                 (kubed-ext--cb-on-success ctx)))))))))))

;; ── Wiring: feed kubed-update outcomes into the state machine ────

(defun kubed-ext--update-list-buffers-after-process
    (type context namespace eenv status err-buf-name)
  "Update list buffer headers for TYPE/CONTEXT/NAMESPACE/EENV after STATUS.
ERR-BUF-NAME is the stderr buffer name for failed updates."
  (kubed-ext--for-each-list-buffer
   (lambda (buf)
     (when (kubed-ext--matching-list-buffer-p buf type context namespace eenv)
       (if (string= status "exited abnormally with code 1\n")
           (let ((err-buf (get-buffer err-buf-name)))
             (kubed-ext--set-header-error
              (if (and err-buf (buffer-live-p err-buf))
                  (with-current-buffer err-buf
                    (string-trim (buffer-string)))
                "kubectl command failed")))
         (kubed-ext--clear-header-error))))))

(defun kubed-ext--set-list-buffer-error-after-update
    (type context namespace eenv message)
  "Display parse/update MESSAGE in list buffers matching TYPE/CONTEXT/NAMESPACE/EENV."
  (kubed-ext--for-each-list-buffer
   (lambda (buf)
     (when (kubed-ext--matching-list-buffer-p buf type context namespace eenv)
       (kubed-ext--set-header-error message)))))

(defun kubed-ext--set-list-update-flags
    (type context namespace eenv in-progress failed)
  "Set kubed update-status flags on list buffers matching the data slot.
TYPE, CONTEXT, NAMESPACE, and EENV identify the matching list buffers.
IN-PROGRESS and FAILED set the buffer-local `kubed--update-in-progress'
and `kubed--last-update-failed' for parity with upstream `kubed-update'."
  (kubed-ext--for-each-list-buffer
   (lambda (buf)
     (when (kubed-ext--matching-list-buffer-p buf type context namespace eenv)
       (setq kubed--update-in-progress in-progress
             kubed--last-update-failed failed)))))

(defun kubed-ext--kubed-update (type context &optional namespace eenv)
  "Update Kubernetes resources TYPE in CONTEXT and NAMESPACE for EENV.
This is a `kubed-update' override for kubed 0.7.0.  It intentionally
uses kubed private contracts: `kubed--alist' cache slots keyed by
EENV/TYPE/CONTEXT/NAMESPACE (the execution-environment key introduced in
kubed 0.7.0), `kubed--columns' fetch specs, and `kubed-list-mode'
buffer-local identifiers.  The override is installed only after a
compatibility check and should be re-audited for every upstream kubed
version change.

The implementation adds kubed-ext selector/status handling and avoids
noisy routine refresh messages."
  (kubed-ext--patch-type-timestamp-columns type)
  (unless eenv
    (setq eenv (kubed-ext--eenv-key)))
  (setq context (kubed-ext--usable-context context))
  (when (process-live-p
         (alist-get 'process (kubed--alist eenv type context namespace)))
    (user-error "Update in progress"))
  (let* ((out (get-buffer-create (format " *kubed-get-%s*" type)))
         (err (get-buffer-create (format " *kubed-get-%s-stderr*" type)))
         (columns (alist-get type kubed--columns
                             '(("NAME:.metadata.name")) nil #'string=))
         (selector (kubed-ext--find-active-selector type context namespace eenv))
         (server-side-table (kubed-ext--server-side-list-resource-p type context))
         (command (kubed-ext--list-command
                   type context namespace columns server-side-table selector))
         (err-buf-name (buffer-name err)))
    (with-current-buffer out (erase-buffer))
    (with-current-buffer err (erase-buffer))
    (when kubed-ext-command-log-enabled
      (kubed-ext--log-kubectl-command
       (mapconcat #'identity (seq-filter #'stringp command) " ")))
    (setf (alist-get 'process (kubed--alist eenv type context namespace))
          (make-process
           :name (format "*kubed-get-%s*" type)
           :buffer out
           :stderr err
           :file-handler t
           :command command
           :sentinel
           (lambda (_proc status)
             (cond
              ((string= status "finished\n")
               (condition-case err
                   (progn
                     (if server-side-table
                         (kubed-ext--handle-server-table-success
                          out eenv type context namespace)
                       (kubed-ext--handle-update-success
                        out eenv type context namespace columns))
                     (kubed-ext--set-list-update-flags
                      type context namespace eenv nil nil)
                     (kubed-ext--cb-on-success context))
                 (error
                  (kubed-ext--set-list-update-flags
                   type context namespace eenv nil t)
                  (kubed-ext--set-list-buffer-error-after-update
                   type context namespace eenv (error-message-string err)))))
              ((string= status "exited abnormally with code 1\n")
               (with-current-buffer err
                 (goto-char (point-max))
                 (insert "\n" status))
               (kubed-ext--set-list-update-flags
                type context namespace eenv nil t)
               (kubed-ext--update-list-buffers-after-process
                type context namespace eenv status err-buf-name)
               (kubed-ext--cb-on-failure
                context (or (kubed-ext--cb-read-stderr type)
                            "kubectl exited with code 1")))))))
    (kubed-ext--set-list-update-flags type context namespace eenv t nil)))

(defun kubed-ext--handle-update-success
    (out eenv type context namespace columns)
  "Parse successful kubectl OUT for EENV and refresh matching list buffers.
TYPE, CONTEXT, NAMESPACE, and COLUMNS identify the kubed data slot and
the custom-column parser layout."
  (let (new offsets eol)
    (with-current-buffer out
      (goto-char (point-min))
      (setq eol (pos-eol))
      (while (re-search-forward "[^ ]+" eol t)
        (push (1- (match-beginning 0)) offsets))
      (setq offsets (nreverse offsets))
      (forward-line 1)
      (while (not (eobp))
        (let ((cols nil)
              (beg (car offsets))
              (ends (append (cdr offsets)
                            (list (- (pos-eol) (point))))))
          (dolist (column columns)
            (let* ((end (or (pop ends) 0))
                   (line-end (- (pos-eol) (point)))
                   (safe-beg (max 0 (min (or beg 0) line-end)))
                   (safe-end (max safe-beg (min end line-end)))
                   (str (string-trim
                         (buffer-substring (+ (point) safe-beg)
                                           (+ (point) safe-end)))))
              (push (if-let ((f (cdr column))) (funcall f str) str)
                    cols)
              (setq beg end)))
          (push (nreverse cols) new))
        (forward-line 1)))
    (let ((resources (mapcar (lambda (c)
                               (list (car c) (apply #'vector c)))
                             new)))
      (setf (kubed--alist eenv type context namespace)
            (list (cons 'resources resources)))
      (kubed-ext--remember-large-list-if-needed
       type context namespace eenv (length resources)))
    (kubed-ext--refresh-list-buffers-after-update
     type context namespace eenv)))

(defun kubed-ext--handle-server-table-success (out eenv type context namespace)
  "Parse server-side kubectl table OUT for EENV/TYPE/CONTEXT/NAMESPACE."
  (let ((raw (with-current-buffer out (buffer-string))))
    (cond
     ((or (string-empty-p (string-trim raw))
          (string-match-p "\\`[[:space:]]*No resources found" raw))
      (setf (kubed--alist eenv type context namespace)
            (list (cons 'resources nil)))
      (kubed-ext--refresh-list-buffers-after-update
       type context namespace eenv))
     (t
      (let* ((parsed (kubed-ext--parse-kubectl-table raw))
             (headers (car parsed))
             (rows (cdr parsed)))
        (if (null headers)
            (progn
              (setf (kubed--alist eenv type context namespace)
                    (list (cons 'resources nil)))
              (kubed-ext--refresh-list-buffers-after-update
               type context namespace eenv))
          (let ((resources (kubed-ext--server-table-entries headers rows type)))
            (setf (kubed--alist eenv type context namespace)
                  (list (cons 'resources resources)))
            (kubed-ext--remember-large-list-if-needed
             type context namespace eenv (length resources))
            (kubed-ext--refresh-list-buffers-after-update
             type context namespace eenv
             (kubed-ext--server-table-format headers rows)))))))))

(defun kubed-ext--kubed-package-version ()
  "Return installed kubed package version string when available."
  (when (and (boundp 'package-alist)
             (fboundp 'package-desc-version)
             (fboundp 'package-version-join))
    (when-let ((desc (cadr (assq 'kubed package-alist))))
      (package-version-join (package-desc-version desc)))))

(defun kubed-ext--kubed-update-compatible-p ()
  "Return non-nil when `kubed-update' has the shape kubed-ext overrides.
kubed 0.7.0 renamed the 4th argument from HOST to EENV; both shapes share
the same arity and call contract, so either is accepted."
  (and (fboundp 'kubed-update)
       (member (help-function-arglist 'kubed-update t)
               '((type context &optional namespace eenv)
                 (type context &optional namespace host)))))

(defun kubed-ext--install-kubed-update-override ()
  "Install kubed-ext's guarded `kubed-update' override."
  (let* ((version (kubed-ext--kubed-package-version))
         (supported-version-p
          (or (null version)
              (string= version kubed-ext--supported-kubed-version))))
    (cond
     ((not (kubed-ext--kubed-update-compatible-p))
      (display-warning
       'kubed-ext
       (format "Not overriding kubed-update; unexpected signature: %S"
               (and (fboundp 'kubed-update)
                    (help-function-arglist 'kubed-update t)))
       :error))
     ((or supported-version-p
          kubed-ext-force-unsupported-kubed-update-override)
      (when (and version (not supported-version-p))
        (display-warning
         'kubed-ext
         (format "Forcing kubed-update override validated with kubed %s; loaded kubed is %s."
                 kubed-ext--supported-kubed-version version)
         :warning))
      (dolist (fn '(kubed-ext--safe-update
                    kubed-ext--update-error-advice
                    kubed-ext--ensure-age-formatting
                    kubed-ext-update-with-selector
                    kubed-ext--cb-update-advice
                    kubed-ext--kubed-update-advice
                    kubed-ext--kubed-update))
        (advice-remove 'kubed-update fn))
      (advice-add 'kubed-update :override #'kubed-ext--kubed-update))
     (t
      (display-warning
       'kubed-ext
       (format "Not overriding kubed-update: kubed-ext was validated with kubed %s; loaded kubed is %s.  Customize `kubed-ext-force-unsupported-kubed-update-override' to force it."
               kubed-ext--supported-kubed-version version)
       :warning)))))

(dolist (fn '(kubed-ext--safe-update
              kubed-ext--update-error-advice
              kubed-ext--ensure-age-formatting
              kubed-ext-update-with-selector
              kubed-ext--cb-update-advice
              kubed-ext--kubed-update-advice
              kubed-ext--kubed-update))
  (advice-remove 'kubed-update fn))
(advice-remove 'make-process 'kubed-ext--make-process-logger)
(advice-remove 'call-process 'kubed-ext--call-process-logger)
(advice-remove 'call-process-region 'kubed-ext--call-process-region-logger)
(kubed-ext--install-kubed-update-override)

;; ── `g' clears the circuit before the manual refresh fires ───────

(defun kubed-ext--cb-before-manual-refresh (&rest _)
  "Before-advice on `kubed-list-update': clear an open circuit on `g'.
This gives the cluster an immediate retry without waiting for a probe."
  (kubed-ext--repair-current-list-context)
  (when (and (derived-mode-p 'kubed-list-mode)
             (kubed-ext--cb-open-p kubed-list-context))
    (kubed-ext--cb-reset-state kubed-list-context)
    (kubed-ext--cb-clear-headers kubed-list-context)
    (kubed-ext--cb-refresh-mode-lines kubed-list-context)))

(advice-add 'kubed-list-update :before #'kubed-ext--cb-before-manual-refresh)

;; ── Manual reset command ──────────────────────────────────────────

(defun kubed-ext-circuit-breaker-reset (&optional context)
  "Reset the circuit breaker for CONTEXT and resume auto-refresh.
Call this after reconnecting to VPN or refreshing expired credentials.
With a prefix argument, prompt for CONTEXT; otherwise use the context
of the current buffer or the configured default."
  (interactive
   (list (if current-prefix-arg
             (kubed-read-context "Reset circuit for context"
                                 (kubed-local-context))
           (kubed-local-context))))
  (let ((ctx (or context (kubed-local-context))))
    (kubed-ext--cb-reset-state ctx)
    (kubed-ext--cb-clear-headers ctx)
    (kubed-ext--cb-refresh-mode-lines ctx)
    (kubed-ext--cb-in-context-bufs
     ctx (lambda ()
           (condition-case nil (kubed-list-update t)
             (error nil))))
    (message "Kubed [%s]: circuit reset — auto-refresh resumed." ctx)))

(kubed-ext--set-key 'kubed-list-mode-map "Z" #'kubed-ext-circuit-breaker-reset)
(kubed-ext--set-key 'kubed-prefix-map    "Z" #'kubed-ext-circuit-breaker-reset)

;; ── Auto-refresh tick ─────────────────────────────────────────────

(defun kubed-ext--auto-refresh-tick ()
  "Refresh visible kubed list buffers; skip any with an open circuit.
Reschedules itself after each tick with a fresh random delay."
  (unless (active-minibuffer-window)
    (let ((refreshed 0)
          (skipped   0)
          (seen-buffers nil))
      (dolist (win (window-list))
        (let ((buf (window-buffer win)))
          (when (and (buffer-live-p buf)
                     (not (memq buf seen-buffers)))
            (push buf seen-buffers)
            (with-current-buffer buf
              (when (derived-mode-p 'kubed-list-mode)
                (let ((eenv (kubed-ext--eenv-key)))
                  (cond
                   ;; Circuit is open — skip and count.
                   ((kubed-ext--cb-open-p kubed-list-context)
                    (cl-incf skipped))
                   ;; An update is already in progress — skip silently.
                   ((process-live-p
                     (alist-get 'process
                                (kubed--alist eenv kubed-list-type
                                              kubed-list-context
                                              kubed-list-namespace))))
                   ;; Large cached lists are expensive; manual `g' still refreshes.
                   ((kubed-ext--large-list-auto-refresh-skip-p
                     kubed-list-type kubed-list-context kubed-list-namespace
                     eenv)
                    (cl-incf skipped))
                   ;; Normal case — refresh.
                   (t
                    (condition-case nil
                        (progn (kubed-list-update t) (cl-incf refreshed))
                      (error nil))))))))))
      (ignore refreshed skipped)))
  (kubed-ext--auto-refresh-schedule))

;; ── Mode-line ─────────────────────────────────────────────────────

(unless kubed-ext--original-list-mode-line-format-saved
  (setq kubed-ext--original-list-mode-line-format
        kubed-list-mode-line-format
        kubed-ext--original-list-mode-line-format-saved t))

(setq kubed-list-mode-line-format
      '(:eval
        (let* ((ctx      kubed-list-context)
               (eenv     (kubed-ext--eenv-key))
               (reason   (and ctx (kubed-ext--cb-open-p ctx)))
               (updating (process-live-p
                          (alist-get 'process
                                     (kubed--alist eenv kubed-list-type
                                                   ctx
                                                   kubed-list-namespace))))
               (is-prod  (and ctx
                              (bound-and-true-p
                               kubed-ext-production-context-regexp)
                              (stringp
                               kubed-ext-production-context-regexp)
                              (string-match-p
                               kubed-ext-production-context-regexp ctx))))
          (concat
           ;; ── Context badge ──────────────────────────────────
           (when ctx
             (propertize
              (if is-prod
                  (format " [☢ %s ☢]" ctx)
                (format " [%s]" ctx))
              'face      (if is-prod 'error 'success)
              'help-echo (format "Context: %s  (%s)"
                                 ctx
                                 (if is-prod "PRODUCTION" "non-prod"))))
           ;; ── Status indicators ──────────────────────────────
           (cond
            (updating
             (propertize " [...]"
                         'help-echo "kubectl update in progress…"))

            ((eq reason 'network)
             (let ((secs (round (kubed-ext--cb-backoff-for ctx))))
               (propertize (format " ⚡~%ds" secs)
                           'face      'error
                           'help-echo
                           (format (concat "Network unreachable for `%s'.\n"
                                           "Auto-refresh paused; probing in ~%ds.\n"
                                           "Press `g' or `Z' to retry immediately.")
                                   ctx secs))))

            (t
             (concat
              (when kubed-list-filter
                (propertize
                 (format " [%s]"
                         (mapconcat #'prin1-to-string
                                    kubed-list-filter " "))
                 'help-echo "Active s-expression filter"))
              (when (bound-and-true-p kubed-ext-list-label-selector)
                (propertize
                 (format " {%s}" kubed-ext-list-label-selector)
                 'help-echo "Active label selector"
                 'face      'warning))
              (when (and (bound-and-true-p kubed-ext-resource-filter)
                         (not (string-empty-p kubed-ext-resource-filter)))
                (propertize
                 (format " /%s/" kubed-ext-resource-filter)
                 'help-echo "Active substring filter"
                 'face      'italic)))))))))

;;; ═══════════════════════════════════════════════════════════════
;;; § 22.  Enhanced Log Filtering and Parallel Streaming
;;; ═══════════════════════════════════════════════════════════════

(defgroup kubed-ext-logs nil
  "Kubernetes log filtering and streaming with stern."
  :group 'kubed-ext)

(defcustom kubed-ext-log-error-pattern
  "exception\\|error\\|critical\\|failure\\|fatal\\|panic"
  "Case-insensitive regex for filtering error log lines."
  :type 'string
  :group 'kubed-ext-logs)

(defcustom kubed-ext-log-default-since "1h"
  "Default --since duration for filtered log commands (e.g. \"30m\", \"24h\")."
  :type 'string
  :group 'kubed-ext-logs)

(defvar kubed-ext-log-filter-history nil
  "Minibuffer history for log-filter pattern prompts.")

;;; ─── Filter prompt ────────────────────────────────────────────

(defun kubed-ext--read-log-filter (prompt &optional default)
  "Prompt with PROMPT for a log-filter regex, defaulting to DEFAULT."
  (let* ((opt-err    (concat "(errors)  " kubed-ext-log-error-pattern))
         (opt-none   "(none)    show all lines")
         (opt-custom "(custom)  enter pattern…")
         (choice     (completing-read
                      (format-prompt prompt (or default ""))
                      (list opt-err opt-none opt-custom)
                      nil nil nil
                      'kubed-ext-log-filter-history
                      (or default ""))))
    (cond
     ((string= choice opt-err)    kubed-ext-log-error-pattern)
     ((string= choice opt-none)   "")
     ((string= choice opt-custom)
      (read-string (format-prompt "Pattern" default)
                   default 'kubed-ext-log-filter-history))
     (t choice))))

;;; ─── ANSI-color filter ────────────────────────────────────────

(defun kubed-ext--log-ansi-filter (proc string)
  "Process filter inserting STRING into PROC's buffer with ANSI decoding."
  (require 'ansi-color)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (unless (local-variable-p 'ansi-color-context)
        (setq-local ansi-color-context nil))
      (let* ((inhibit-read-only t)
             (mark (or (process-mark proc)
                       (set-marker (process-mark proc) (point-max))))
             (moving (= (point) mark))
             (text (ansi-color-apply string)))
        (save-excursion
          (goto-char mark)
          (insert text)
          (set-marker (process-mark proc) (point)))
        (when moving
          (goto-char (process-mark proc)))))))

;;; ─── Core launch helper ───────────────────────────────────────

(defun kubed-ext--stern-query-for-resource (resource)
  "Return a stern pod-query for Kubernetes log RESOURCE."
  (let ((name (if (string-match "\\`[^/]+/\\(.+\\)\\'" resource)
                  (match-string 1 resource)
                resource)))
    (cond
     ((string-empty-p name) ".*")
     ((string-search "|" name) name)
     (t (regexp-quote name)))))

(defun kubed-ext--stern-log-args
    (type name context namespace follow prefix since tail timestamps
          &optional container all-containers selector)
  "Build kubectl-log-shaped args for stern.
TYPE and NAME identify a resource unless SELECTOR is non-nil.
CONTEXT and NAMESPACE select the cluster scope.  FOLLOW, PREFIX, SINCE,
TAIL, and TIMESTAMPS map to their stern equivalents.  CONTAINER and
ALL-CONTAINERS choose which container streams to include."
  (append
   (if selector
       (list "logs" "-l" selector)
     (list "logs" (if (string= type "pods")
                      name
                    (concat type "/" name))))
   (when context (list "--context" context))
   (when namespace (list "-n" namespace))
   (cond
    ((stringp container) (list "-c" container))
    (all-containers '("--all-containers")))
   (when follow '("--follow"))
   (when prefix '("--prefix"))
   (when timestamps '("--timestamps"))
   (when since (list "--since" since))
   (when tail (list "--tail" (if (numberp tail)
                                 (number-to-string tail)
                               tail)))))

(defun kubed-ext--stern-resource-args
    (type name context namespace follow prefix since tail timestamps
          &optional container all-containers)
  "Build safe stern args for TYPE/NAME in CONTEXT/NAMESPACE.
FOLLOW, PREFIX, SINCE, TAIL, and TIMESTAMPS map to stern flags.
CONTAINER and ALL-CONTAINERS choose which container streams to include.
Pods are queried directly.  Unsupported resource types signal
`user-error' instead of guessing pod names."
  (if (member type '("pod" "pods"))
      (kubed-ext--stern-log-args
       "pods" name context namespace follow prefix since tail timestamps
       container all-containers)
    (user-error "Stern logs require a pod or explicit selector, not %s/%s"
                type name)))

(defun kubed-ext--read-stern-selector (type name)
  "Read a label selector for stern logs for TYPE/NAME."
  (read-string (format "Label selector for %s/%s: " type name)
               nil 'kubed-ext-label-selector-history))

(defun kubed-ext--stern-resource-or-selector-args
    (type name context namespace follow prefix since tail timestamps
          &optional container all-containers)
  "Build stern args for TYPE/NAME or prompt for a selector for non-pods.
CONTEXT and NAMESPACE select the cluster scope.  FOLLOW, PREFIX, SINCE,
TAIL, and TIMESTAMPS map to stern flags.  CONTAINER and ALL-CONTAINERS
choose which container streams to include."
  (if (member type '("pod" "pods"))
      (kubed-ext--stern-resource-args
       type name context namespace follow prefix since tail timestamps
       container all-containers)
    (let ((selector (kubed-ext--read-stern-selector type name)))
      (unless (and (stringp selector)
                   (not (string-empty-p (string-trim selector))))
        (user-error "No label selector specified"))
      (kubed-ext--stern-log-args
       type name context namespace follow prefix since tail timestamps
       container all-containers selector))))

(defun kubed-ext--stern-args-from-kubectl-logs (kubectl-args filter-pattern)
  "Translate kubectl log style KUBECTL-ARGS into stern arguments.
FILTER-PATTERN becomes a stern include/highlight expression."
  (let ((args (if (equal (car kubectl-args) "logs")
                  (cdr kubectl-args)
                kubectl-args))
        (query nil)
        (selector nil)
        (context nil)
        (namespace nil)
        (container nil)
        (follow nil)
        (since nil)
        (tail nil)
        (timestamps nil)
        (include (and (stringp filter-pattern)
                      (not (string-empty-p filter-pattern))
                      filter-pattern)))
    (while args
      (let ((arg (pop args)))
        (cond
         ((member arg '("-l" "--selector"))
          (setq selector (pop args)
                query ".*"))
         ((string-prefix-p "--selector=" arg)
          (setq selector (substring arg (length "--selector="))
                query ".*"))
         ((member arg '("-n" "--namespace"))
          (setq namespace (pop args)))
         ((string-prefix-p "--namespace=" arg)
          (setq namespace (substring arg (length "--namespace="))))
         ((string= arg "--context")
          (setq context (pop args)))
         ((string-prefix-p "--context=" arg)
          (setq context (substring arg (length "--context="))))
         ((member arg '("-c" "--container"))
          (setq container (pop args)))
         ((string-prefix-p "--container=" arg)
          (setq container (substring arg (length "--container="))))
         ((member arg '("--since" "--since-time"))
          (setq since (pop args)))
         ((string-prefix-p "--since=" arg)
          (setq since (substring arg (length "--since="))))
         ((string-prefix-p "--since-time=" arg)
          (setq since (substring arg (length "--since-time="))))
         ((string= arg "--tail")
          (setq tail (pop args)))
         ((string-prefix-p "--tail=" arg)
          (setq tail (substring arg (length "--tail="))))
         ((string= arg "--follow")
          (setq follow t))
         ((string= arg "--timestamps")
          (setq timestamps t))
         ((string-prefix-p "-" arg)
          nil)
         ((not query)
          (setq query (kubed-ext--stern-query-for-resource arg))))))
    (append
     (list (or query ".*") "--color" "always")
     (when selector (list "--selector" selector))
     (when context (list "--context" context))
     (when namespace (list "--namespace" namespace))
     (when container
       (list "--container" (format "^%s$" (regexp-quote container))))
     (when since (list "--since" since))
     (when tail (list "--tail" tail))
     (unless follow (list "--no-follow"))
     (when timestamps (list "--timestamps=default"))
     (when include (list "--include" include "--highlight" include)))))

(defun kubed-ext--launch-log-process (buf kubectl-args filter-pattern)
  "Start a stern process writing to BUF.

KUBECTL-ARGS uses the same shape as old kubectl log calls so existing
callers can stay small while log streaming is handled by `stern'.
FILTER-PATTERN is a regex string, or nil/empty for no filtering."
  (unless (executable-find kubed-ext-stern-program)
    (user-error "Cannot find stern executable `%s'" kubed-ext-stern-program))
  (let* ((args (kubed-ext--stern-args-from-kubectl-logs
                kubectl-args filter-pattern))
         (proc (apply #'start-process
                      "*kubed-stern-logs*" buf
                      kubed-ext-stern-program args)))
    (set-process-filter proc #'kubed-ext--log-ansi-filter)
    (set-process-sentinel
     proc
     (lambda (_p status)
       (when (and (buffer-live-p buf)
                  (not (member status '("finished\n" "killed\n"))))
         (with-current-buffer buf
           (let ((inhibit-read-only t))
             (goto-char (point-max))
             (insert (propertize
                      (format "[stern: %s]\n" (string-trim status))
                      'face 'error)))))))
    proc))

(defconst kubed-ext--read-container-marker 'kubed-ext-read-container
  "Marker requesting async container completion before starting logs.")

(defvar kubed-ext--original-kubed-logs
  (and (fboundp 'kubed-logs)
       (symbol-function 'kubed-logs))
  "Original function binding for `kubed-logs'.")

(defun kubed-ext--call-original-kubed-logs (&rest args)
  "Call upstream `kubed-logs' with ARGS, or signal a clear error."
  (if kubed-ext--original-kubed-logs
      (apply kubed-ext--original-kubed-logs args)
    (user-error "Cannot find stern executable `%s' and no upstream kubed-logs fallback is available"
                kubed-ext-stern-program)))

(defun kubed-ext-stern-logs
    (type resource &optional context namespace
          container follow limit prefix since tail timestamps)
  "Show Kubernetes logs for TYPE/RESOURCE using `stern'.
CONTEXT and NAMESPACE select the cluster scope.  CONTAINER, FOLLOW,
LIMIT, PREFIX, SINCE, TAIL, and TIMESTAMPS match `kubed-logs'.
When `kubed-ext-stern-program' is unavailable, fall back to upstream
`kubed-logs' so basic log viewing remains functional."
  (if (not (executable-find kubed-ext-stern-program))
      (kubed-ext--call-original-kubed-logs
       type resource context namespace container follow limit prefix since tail
       timestamps)
    (let ((context (or context (kubed-local-context))))
      (setq namespace (or namespace (kubed--namespace context)))
      (if (eq container kubed-ext--read-container-marker)
          (kubed-ext--read-pod-container-async
           resource "Container" context namespace
           (lambda (selected)
             (kubed-ext-stern-logs
              type resource context namespace selected follow limit prefix
              since tail timestamps))
           nil t)
        (when limit
          (message "stern does not support kubectl --limit-bytes; ignoring %s" limit))
        (let* ((buf (generate-new-buffer
                     (format "*kubed-stern %s/%s%s in %s[%s]*"
                             type resource
                             (cond
                              ((stringp container) (concat "[" container "]"))
                              (container "[all containers]")
                              (t ""))
                             namespace context)))
               (args (kubed-ext--stern-resource-or-selector-args
                      type resource context namespace follow prefix since tail
                      timestamps container container)))
          (with-current-buffer buf (run-hooks 'kubed-logs-setup-hook))
          (kubed-ext--launch-log-process buf args nil)
          (display-buffer buf))))))

(advice-add 'kubed-logs :override #'kubed-ext-stern-logs)

(defun kubed-ext--pod-log-transient-options ()
  "Return pod log options from `kubed-transient-logs-for-pod'."
  (let (pod context namespace container follow limit prefix since tail timestamps)
    (dolist (arg (kubed-transient-args 'kubed-transient-logs-for-pod))
      (cond
       ((string-match "\\`pods/\\(.+\\)" arg)
        (setq pod (match-string 1 arg)))
       ((string-match "--namespace=\\(.+\\)" arg)
        (setq namespace (match-string 1 arg)))
       ((string-match "--context=\\(.+\\)" arg)
        (setq context (match-string 1 arg)))
       ((string-match "--limit-bytes=\\(.+\\)" arg)
        (setq limit (string-to-number (match-string 1 arg))))
       ((string-match "--tail=\\(.+\\)" arg)
        (setq tail (string-to-number (match-string 1 arg))))
       ((string-match "--since-time=\\(.+\\)" arg)
        (setq since (match-string 1 arg)))
       ((equal "--all-containers" arg) (setq container t))
       ((equal "--follow" arg) (setq follow t))
       ((equal "--prefix" arg) (setq prefix t))
       ((equal "--timestamps" arg) (setq timestamps t))))
    (list pod context namespace container follow limit prefix since tail timestamps)))

(defun kubed-ext-logs-for-pod
    (pod &optional context namespace container follow limit prefix since tail timestamps)
  "Show stern logs for Kubernetes pod POD with async container completion.
CONTEXT and NAMESPACE select the cluster scope.  CONTAINER, FOLLOW,
LIMIT, PREFIX, SINCE, TAIL, and TIMESTAMPS match `kubed-logs'."
  (interactive
   (pcase-let ((`(,pod ,context ,namespace ,container ,follow ,limit
                       ,prefix ,since ,tail ,timestamps)
                (kubed-ext--pod-log-transient-options)))
     (unless context
       (setq context
             (let ((cxt (kubed-local-context)))
               (if (equal current-prefix-arg '(16))
                   (kubed-read-context "Context" cxt)
                 cxt))))
     (unless namespace
       (setq namespace (kubed--namespace context current-prefix-arg)))
     (unless pod
       (setq pod
             (kubed-read-pod
              "Show logs for"
              (and (equal context kubed-list-context)
                   (equal namespace kubed-list-namespace)
                   (equal kubed-list-type "pods")
                   (tabulated-list-get-id
                    (kubed--event-point last-nonmenu-event)))
              nil context namespace)))
     (unless container
       (setq container kubed-ext--read-container-marker))
     (list pod context namespace container follow limit prefix since tail timestamps)))
  (kubed-ext-stern-logs
   "pods" pod context namespace container follow limit prefix since tail timestamps))

(defvar kubed-ext--original-kubed-logs-for-pod
  (and (fboundp 'kubed-logs-for-pod)
       (symbol-function 'kubed-logs-for-pod))
  "Original function binding for `kubed-logs-for-pod'.")

(fset 'kubed-logs-for-pod #'kubed-ext-logs-for-pod)

;;; ─── Pod: error logs ──────────────────────────────────────────

(defun kubed-ext-pods-logs-errors (click)
  "Show error-filtered logs for the pod at CLICK position."
  (interactive (list last-nonmenu-event) kubed-pods-mode)
  (if-let ((pod (kubed-ext--resource-at-event click)))
      (let* ((ctx  kubed-list-context)
             (ns   kubed-list-namespace)
             (args (append
                    (list "logs" pod
                          "--all-containers"
                          "--prefix"
                          "--since" kubed-ext-log-default-since)
                    (when ctx (list "--context" ctx))
                    (when ns  (list "-n" ns))))
             (buf  (generate-new-buffer
                    (format "*kubed-logs-errors pods/%s@%s[%s]*"
                            pod (or ns "default") (or ctx "current")))))
        (with-current-buffer buf (run-hooks 'kubed-logs-setup-hook))
        (kubed-ext--launch-log-process buf args kubed-ext-log-error-pattern)
        (display-buffer buf))
    (user-error "No Kubernetes pod at point")))

;;; ─── Pod: custom-filtered logs ────────────────────────────────

(defun kubed-ext-pods-logs-custom-filter (click)
  "Show filtered logs for the pod at CLICK, prompting for options."
  (interactive (list last-nonmenu-event) kubed-pods-mode)
  (if-let ((pod (kubed-ext--resource-at-event click)))
      (let* ((ctx       kubed-list-context)
             (ns        kubed-list-namespace)
             (filt      (kubed-ext--read-log-filter
                         "Filter pattern" kubed-ext-log-error-pattern))
             (since     (read-string (format-prompt "Since"
                                                    kubed-ext-log-default-since)
                                     nil nil kubed-ext-log-default-since))
             (follow    (y-or-n-p "Follow (stream) logs? "))
             ;; Ask all-containers first; only prompt for a specific
             ;; container when the user says no.
             (all-con   (y-or-n-p "All containers? "))
             (start-log
              (lambda (&optional container)
                (let* ((args
                        (append
                         (list "logs" pod "--prefix" "--since" since)
                         (when ctx     (list "--context" ctx))
                         (when ns      (list "-n" ns))
                         (when follow  '("--follow"))
                         (if all-con
                             '("--all-containers")
                           (list "-c" container))))
                       (buf
                        (generate-new-buffer
                         (format "*kubed-logs-filtered pods/%s@%s[%s]*"
                                 pod (or ns "default") (or ctx "current")))))
                  (with-current-buffer buf (run-hooks 'kubed-logs-setup-hook))
                  (kubed-ext--launch-log-process buf args filt)
                  (display-buffer buf)))))
        (if all-con
            (funcall start-log)
          (kubed-ext--read-pod-container-async
           pod "Container" ctx ns start-log nil t)))
    (user-error "No Kubernetes pod at point")))

;;; ─── Standalone: error logs for any resource ──────────────────

;;;###autoload
(defun kubed-ext-logs-errors (&optional context namespace)
  "Show error-filtered logs for a Kubernetes resource (CONTEXT and NAMESPACE)."
  (interactive
   (let* ((c (kubed-local-context))
          (c (if (equal current-prefix-arg '(16))
                 (kubed-read-context "Context" c) c))
          (n (kubed--namespace c current-prefix-arg)))
     (list c n)))
  (let* ((ctx    (or context (kubed-local-context)))
         (ns     (or namespace (kubed--namespace ctx)))
         (type   (kubed-read-resource-type "Resource type" nil ctx))
         (res    (kubed-read-resource-name type "Error logs for" nil nil ctx ns))
         (since  (read-string (format-prompt "Since" kubed-ext-log-default-since)
                              nil nil kubed-ext-log-default-since))
         (follow (y-or-n-p "Follow (stream) logs? "))
         (args   (kubed-ext--stern-resource-or-selector-args
                  type res ctx ns follow t since nil nil nil nil))
         (buf    (generate-new-buffer
                  (format "*kubed-logs-errors %s/%s@%s[%s]*"
                          type res (or ns "default") ctx))))
    (with-current-buffer buf (run-hooks 'kubed-logs-setup-hook))
    (kubed-ext--launch-log-process buf args kubed-ext-log-error-pattern)
    (display-buffer buf)))

;;; ─── Standalone: logs by label with filtering ─────────────────

;;;###autoload
(defun kubed-ext-logs-by-label-filtered (&optional context namespace)
  "Stream filtered logs for pods matching a label selector (CONTEXT and NAMESPACE)."
  (interactive
   (let* ((c (kubed-local-context))
          (c (if (equal current-prefix-arg '(16))
                 (kubed-read-context "Context" c) c))
          (n (kubed--namespace c current-prefix-arg)))
     (list c n)))
  (let* ((ctx      (or context (kubed-local-context)))
         (ns       (or namespace (kubed--namespace ctx)))
         (selector (read-string "Label selector: "
                                nil 'kubed-ext-label-selector-history))
         (filt     (kubed-ext--read-log-filter
                    "Filter pattern" kubed-ext-log-error-pattern))
         (since    (read-string (format-prompt "Since" kubed-ext-log-default-since)
                                nil nil kubed-ext-log-default-since))
         (follow   (y-or-n-p "Follow (stream) logs? "))
         (args     (append
                    (list "logs" "-l" selector
                          "--all-containers=true"
                          "--prefix"
                          "--since" since)
                    (when ctx    (list "--context" ctx))
                    (when ns     (list "-n" ns))
                    (when follow '("--follow"))))
         (buf      (generate-new-buffer
                    (format "*kubed-logs-filtered -l %s@%s[%s]*"
                            selector (or ns "default") ctx))))
    (with-current-buffer buf (run-hooks 'kubed-logs-setup-hook))
    (kubed-ext--launch-log-process buf args filt)
    (display-buffer buf)))

;;; ─── Post-hoc filter on existing log buffer ───────────────────

(defun kubed-ext-filter-log-buffer (pattern)
  "Show lines from the current buffer matching PATTERN using `occur'."
  (interactive
   (list (kubed-ext--read-log-filter
          "Filter log buffer" kubed-ext-log-error-pattern)))
  (when (or (null pattern) (string-empty-p (string-trim pattern)))
    (user-error "No filter pattern specified"))
  (let ((case-fold-search t))
    (occur pattern)))

;;; ─── Keybindings § 22 ─────────────────────────────────────────

(kubed-ext--set-key 'kubed-pods-mode-map "E"   #'kubed-ext-pods-logs-errors)
(kubed-ext--set-key 'kubed-pods-mode-map "H"   #'kubed-ext-pods-logs-custom-filter)
(kubed-ext--set-key 'kubed-prefix-map    "e"   #'kubed-ext-logs-errors)
(kubed-ext--set-key 'kubed-prefix-map    "l f" #'kubed-ext-logs-by-label-filtered)
(kubed-ext--set-key 'kubed-prefix-map    "l p" #'kubed-ext-logs-parallel-by-label)
(kubed-ext--set-key 'kubed-prefix-map    "l o" #'kubed-ext-filter-log-buffer)

;;; ─── Extend pod transient suffixes ────────────────────────────

(defun kubed-ext--replace-extra-transient-suffixes (type keys suffixes)
  "For TYPE, replace transient suffixes with KEYS by SUFFIXES."
  (when-let ((entry (assoc type kubed-ext-extra-transient-suffixes)))
    (setcdr entry
            (append
             (cl-remove-if (lambda (suffix)
                             (and (consp suffix)
                                  (member (car suffix) keys)))
                           (cdr entry))
             suffixes))))

(let ((entry (assoc "pods" kubed-ext-extra-transient-suffixes)))
  (when entry
    (kubed-ext--replace-extra-transient-suffixes
     "pods" '("le" "lf" "lp")
     '(("le" "Error logs (stern)" kubed-ext-pods-logs-errors)
       ("lf" "Filter logs (stern)" kubed-ext-pods-logs-custom-filter)
       ("lp" "Label logs (stern)" kubed-ext-logs-parallel-by-label)))))

;;; ═══════════════════════════════════════════════════════════════
;;; § 22b.  Log Filtering for Deployments and Generic Workloads
;;; ═══════════════════════════════════════════════════════════════

;;; ─── Shared selector log runner ───────────────────────────────

(defun kubed-ext--selector-logs-run
    (selector context namespace filter since follow all-containers buf)
  "Stream logs for SELECTOR into BUF using one `stern' process.

SELECTOR       — Kubernetes label selector string.
CONTEXT        — kubectl context, or nil for current context.
NAMESPACE      — Kubernetes namespace, or nil for current namespace.
FILTER         — case-insensitive regex; nil or empty passes all lines.
SINCE          — kubectl --since value (e.g. \"1h\", \"30m\").
FOLLOW         — non-nil appends --follow flag.
ALL-CONTAINERS — non-nil appends --all-containers flag.
BUF            — pre-existing output buffer."
  (unless (and (stringp selector)
               (not (string-empty-p (string-trim selector))))
    (user-error "No label selector specified"))
  (message "Streaming selector `%s' with stern..." selector)
  (kubed-ext--launch-log-process
   buf
   (append
    (list "logs" "-l" selector "--prefix" "--since" since)
    (when context (list "--context" context))
    (when namespace (list "-n" namespace))
    (when follow '("--follow"))
    (when all-containers '("--all-containers")))
   filter))

;;; ─── Standalone: parallel by label ───────────────────────────

;;;###autoload
(defun kubed-ext-logs-parallel-by-label (&optional context namespace)
  "Stream logs with stern from pods matching a label selector.

CONTEXT and NAMESPACE are optional; defaults to current context/namespace."
  (declare (advertised-calling-convention (context namespace) "1.0"))
  (interactive
   (let* ((c (kubed-local-context))
          (c (if (equal current-prefix-arg '(16))
                 (kubed-read-context "Context" c) c))
          (n (kubed--namespace c current-prefix-arg)))
     (list c n)))
  (let* ((ctx      (or context (kubed-local-context)))
         (ns       (or namespace (kubed--namespace ctx)))
         (selector (read-string "Label selector: "
                                nil 'kubed-ext-label-selector-history))
         (filt     (kubed-ext--read-log-filter
                    "Filter pattern" kubed-ext-log-error-pattern))
         (since    (read-string (format-prompt "Since" kubed-ext-log-default-since)
                                nil nil kubed-ext-log-default-since))
         (follow   (y-or-n-p "Follow (stream) logs? "))
         (all-con  (y-or-n-p "All containers? "))
         (buf      (generate-new-buffer
                    (format "*kubed-parallel-logs -l %s@%s[%s]*"
                            selector (or ns "default") ctx))))
    (with-current-buffer buf (run-hooks 'kubed-logs-setup-hook))
    (kubed-ext--selector-logs-run selector ctx ns filt since follow all-con buf)
    (display-buffer buf)))

;;; ─── Deployment: error logs ───────────────────────────────────

(defun kubed-ext-deployments-logs-errors (click)
  "Show error-filtered logs for the deployment at CLICK position."
  (interactive (list last-nonmenu-event) kubed-deployments-mode)
  (if-let ((dep (kubed-ext--resource-at-event click)))
      (let* ((ctx kubed-list-context)
             (ns kubed-list-namespace)
             (sel (read-string "Label selector: "))
             (buf (generate-new-buffer
                   (format "*kubed-logs-errors deployments/%s@%s[%s]*"
                           dep (or ns "default") (or ctx "current")))))
        (with-current-buffer buf (run-hooks 'kubed-logs-setup-hook))
        (kubed-ext--selector-logs-run
         sel ctx ns kubed-ext-log-error-pattern
         kubed-ext-log-default-since nil t buf)
        (display-buffer buf))
    (user-error "No Kubernetes deployment at point")))

;;; ─── Deployment: custom-filtered logs ────────────────────────

(defun kubed-ext-deployments-logs-custom-filter (click)
  "Show filtered logs for the deployment at CLICK, prompting for options."
  (interactive (list last-nonmenu-event) kubed-deployments-mode)
  (if-let ((dep (kubed-ext--resource-at-event click)))
      (let* ((ctx    kubed-list-context)
             (ns     kubed-list-namespace)
             (sel    (read-string "Label selector: "))
             (filt   (kubed-ext--read-log-filter
                      "Filter pattern" kubed-ext-log-error-pattern))
             (since  (read-string (format-prompt "Since" kubed-ext-log-default-since)
                                  nil nil kubed-ext-log-default-since))
             (follow (y-or-n-p "Follow (stream) logs? "))
             (buf    (generate-new-buffer
                      (format "*kubed-logs-filtered deployments/%s@%s[%s]*"
                              dep (or ns "default") (or ctx "current")))))
        (with-current-buffer buf (run-hooks 'kubed-logs-setup-hook))
        (kubed-ext--selector-logs-run sel ctx ns filt since follow t buf)
        (display-buffer buf))
    (user-error "No Kubernetes deployment at point")))

;;; ─── Deployment: parallel logs ────────────────────────────────

(defun kubed-ext-deployments-logs-parallel (click)
  "Stream filtered logs from all pods of the deployment at CLICK."
  (interactive (list last-nonmenu-event) kubed-deployments-mode)
  (if-let ((dep (kubed-ext--resource-at-event click)))
      (let* ((ctx kubed-list-context)
             (ns  kubed-list-namespace)
             (sel (read-string "Label selector: ")))
        (let* ((filt   (kubed-ext--read-log-filter
                        "Filter pattern" kubed-ext-log-error-pattern))
               (since  (read-string (format-prompt "Since" kubed-ext-log-default-since)
                                    nil nil kubed-ext-log-default-since))
               (follow (y-or-n-p "Follow (stream) logs? "))
               (all-c  (y-or-n-p "All containers per pod? "))
               (buf    (generate-new-buffer
                        (format "*kubed-parallel-logs deployments/%s@%s[%s]*"
                                dep (or ns "default") (or ctx "current")))))
          (with-current-buffer buf (run-hooks 'kubed-logs-setup-hook))
          (kubed-ext--selector-logs-run sel ctx ns filt since follow all-c buf)
          (display-buffer buf)))
    (user-error "No Kubernetes deployment at point")))

;;; ─── Standalone: parallel for any workload ────────────────────

;;;###autoload
(defun kubed-ext-logs-parallel-for-workload (&optional context namespace)
  "Stream filtered stern logs from pods of any workload resource.

CONTEXT and NAMESPACE are optional; defaults to current context/namespace."
  (declare (advertised-calling-convention (context namespace) "1.0"))
  (interactive
   (let* ((c (kubed-local-context))
          (c (if (equal current-prefix-arg '(16))
                 (kubed-read-context "Context" c) c))
          (n (kubed--namespace c current-prefix-arg)))
     (list c n)))
  (let* ((ctx  (or context (kubed-local-context)))
         (ns   (or namespace (kubed--namespace ctx)))
         (type (completing-read "Workload type: "
                                '("deployments" "statefulsets" "daemonsets"
                                  "replicasets" "jobs")
                                nil t nil nil "deployments"))
         (name (kubed-read-resource-name type "Parallel logs for" nil nil ctx ns))
         (sel  (read-string "Label selector: ")))
    (let* ((filt   (kubed-ext--read-log-filter
                    "Filter pattern" kubed-ext-log-error-pattern))
           (since  (read-string (format-prompt "Since" kubed-ext-log-default-since)
                                nil nil kubed-ext-log-default-since))
           (follow (y-or-n-p "Follow (stream) logs? "))
           (all-c  (y-or-n-p "All containers per pod? "))
           (buf    (generate-new-buffer
                    (format "*kubed-parallel-logs %s/%s@%s[%s]*"
                            type name (or ns "default") ctx))))
      (with-current-buffer buf (run-hooks 'kubed-logs-setup-hook))
      (kubed-ext--selector-logs-run sel ctx ns filt since follow all-c buf)
      (display-buffer buf))))

;;; ─── Keybindings § 22b ────────────────────────────────────────

(kubed-ext--set-key 'kubed-deployments-mode-map "E" #'kubed-ext-deployments-logs-errors)
(kubed-ext--set-key 'kubed-deployments-mode-map "H" #'kubed-ext-deployments-logs-custom-filter)
(kubed-ext--set-key 'kubed-prefix-map           "w" #'kubed-ext-logs-parallel-for-workload)

;;; ─── Extend deployment transient suffixes ─────────────────────

(let ((entry (assoc "deployments" kubed-ext-extra-transient-suffixes)))
  (when entry
    (kubed-ext--replace-extra-transient-suffixes
     "deployments" '("le" "lf" "lp")
     '(("le" "Error logs (stern)" kubed-ext-deployments-logs-errors)
       ("lf" "Filter logs (stern)" kubed-ext-deployments-logs-custom-filter)
       ("lp" "Workload pods (stern)" kubed-ext-deployments-logs-parallel)))))

;;;; ═══════════════════════════════════════════════════════════════
;;; § 23.  Setup
;;; ═══════════════════════════════════════════════════════════════

(defun kubed-ext-setup ()
  "Initialize kubed-ext runtime hooks that need compatibility check."
  (kubed-ext--install-kubed-update-override)
  (when (and kubed-ext-auto-discover-crds
             (executable-find (kubed-kubectl-program)))
    (kubed-ext-warm-crd-cache))
  (message "Kubed-ext: setup complete."))

(defun kubed-ext--unset-key-if-bound (map-symbol key command)
  "In MAP-SYMBOL, unset KEY when it is currently bound to COMMAND."
  (when (and (boundp map-symbol) (keymapp (symbol-value map-symbol)))
    (let ((map (symbol-value map-symbol)))
      (when (eq (keymap-lookup map key) command)
        (keymap-unset map key t)))))

(defun kubed-ext--restore-key-if-bound (map-symbol key command old-binding)
  "In MAP-SYMBOL, restore KEY to OLD-BINDING if it still maps to COMMAND."
  (when (and (boundp map-symbol) (keymapp (symbol-value map-symbol)))
    (let ((map (symbol-value map-symbol)))
      (when (eq (keymap-lookup map key) command)
        (if old-binding
            (keymap-set map key old-binding)
          (keymap-unset map key t))))))

(defun kubed-ext--unset-domain-action-bindings ()
  "Remove dynamic domain action keybindings installed into CRD mode maps."
  (dolist (binding kubed-ext--domain-action-bindings)
    (pcase-let ((`(,map-symbol ,key ,command ,old-binding) binding))
      (kubed-ext--restore-key-if-bound map-symbol key command old-binding)))
  (setq kubed-ext--domain-action-bindings nil))

(defun kubed-ext--restore-keybinding-snapshots ()
  "Restore upstream keybindings overwritten by kubed-ext."
  (dolist (binding kubed-ext--keybinding-snapshots)
    (pcase-let ((`(,map-symbol ,key ,command ,old-binding) binding))
      (kubed-ext--restore-key-if-bound map-symbol key command old-binding)))
  (setq kubed-ext--keybinding-snapshots nil))

(defun kubed-ext--restore-column-state ()
  "Restore upstream kubed column state changed by kubed-ext."
  (when (and (boundp 'kubed--columns) kubed-ext--original-kubed-columns)
    (setq kubed--columns (copy-tree kubed-ext--original-kubed-columns t)))
  (dolist (entry kubed-ext--original-column-variables)
    (when (boundp (car entry))
      (set (car entry) (copy-tree (cdr entry) t)))))

(defun kubed-ext--cancel-circuit-breaker-timers ()
  "Cancel all circuit-breaker probe timers."
  (when (hash-table-p kubed-ext--cb-probe-timers)
    (maphash (lambda (_ctx timer)
               (when (timerp timer)
                 (cancel-timer timer)))
             kubed-ext--cb-probe-timers)
    (clrhash kubed-ext--cb-probe-timers)))

(defun kubed-ext-unload-function ()
  "Remove kubed-ext hooks, advice, timers, display rules, and keybindings."
  (remove-hook 'kubed-list-mode-hook #'kubed-ext--register-current-list-buffer)
  (remove-hook 'kubed-list-mode-hook #'kubed-ext--prefetch-namespaces-hook)
  (remove-hook 'kubed-list-mode-hook #'kubed-ext-optimize-tramp)
  (remove-hook 'window-configuration-change-hook
               #'kubed-ext--schedule-stale-refresh)
  (when (timerp kubed-ext--stale-refresh-timer)
    (cancel-timer kubed-ext--stale-refresh-timer)
    (setq kubed-ext--stale-refresh-timer nil))
  (remove-hook 'kubed-pods-mode-hook #'kubed-ext--pod-mode-setup)
  (remove-hook 'kubed-deployments-mode-hook #'kubed-ext--deployment-mode-setup)
  (advice-remove 'kubed-use-context #'kubed-ext--prefetch-after-use-context)
  (advice-remove 'kubed-list-update #'kubed-ext--maybe-enrich-columns)
  (advice-remove 'kubed-list-update #'kubed-ext--cb-before-manual-refresh)
  (advice-remove 'kubed-list-revert #'kubed-ext-mark-port-forwards)
  (advice-remove 'kubed-list-revert #'kubed-ext--reapply-overlays)
  (advice-remove 'kubed-list-revert #'kubed-ext--mark-cache-dirty-after-revert)
  (advice-remove 'kubed-list-mark-for-deletion #'kubed-ext--mark-cache-dirty)
  (advice-remove 'kubed-list-unmark #'kubed-ext--mark-cache-dirty)
  (advice-remove 'kubed-stop-port-forward
                 #'kubed-ext-refresh-pf-markers-in-visible-buffers)
  (advice-remove 'kubed-display-resource-revert
                 #'kubed-ext--display-revert-format)
  (advice-remove 'kubed-update #'kubed-ext--kubed-update)
  (advice-remove 'kubed-default-context-and-namespace
                 #'kubed-ext--around-default-context-and-namespace)
  (advice-remove 'kubed-logs #'kubed-ext-stern-logs)
  (advice-remove 'make-process 'kubed-ext--make-process-logger)
  (advice-remove 'call-process 'kubed-ext--call-process-logger)
  (advice-remove 'call-process-region 'kubed-ext--call-process-region-logger)
  (kubed-ext--cleanup-list-buffer-state)
  (setq kubed-ext--list-buffers nil)
  (setq kubed-ext--list-buffer-initial-scan-done nil)
  (clrhash kubed-ext--selector-cache)
  (setq kubed-ext--stale-list-buffer-count 0)
  (kubed-ext--auto-refresh-cancel-timer)
  (kubed-ext--cancel-circuit-breaker-timers)
  (kubed-ext--cancel-tracked-timers)
  (when (timerp kubed-ext--crd-warm-timer)
    (cancel-timer kubed-ext--crd-warm-timer)
    (setq kubed-ext--crd-warm-timer nil))
  (dolist (rule kubed-ext--display-buffer-rules)
    (setq display-buffer-alist (delete rule display-buffer-alist)))
  (kubed-ext--unset-domain-action-bindings)
  (kubed-ext--restore-keybinding-snapshots)
  (kubed-ext--restore-column-state)
  (kubed-ext--restore-tramp-state)
  (when kubed-ext--original-kubed-logs-for-pod
    (fset 'kubed-logs-for-pod kubed-ext--original-kubed-logs-for-pod))
  (when kubed-ext--original-list-mode-line-format-saved
    (setq kubed-list-mode-line-format
          kubed-ext--original-list-mode-line-format))
  (dolist (binding
           '((kubed-pods-mode-map "T" kubed-ext-top-pods)
             (kubed-pods-mode-map "M" kubed-ext-top-pods)
             (kubed-pods-mode-map "v" kubed-ext-pods-vterm)
             (kubed-pods-mode-map "t" kubed-ext-pods-eat)
             (kubed-pods-mode-map "G" kubed-ext-pods-ghostel)
             (kubed-pods-mode-map "i" kubed-ext-pods-logs-init-container)
             (kubed-pods-mode-map "E" kubed-ext-pods-logs-errors)
             (kubed-pods-mode-map "H" kubed-ext-pods-logs-custom-filter)
             (kubed-pod-prefix-map "T" kubed-ext-top-pods)
             (kubed-pod-prefix-map "v" kubed-ext-vterm-pod)
             (kubed-pod-prefix-map "t" kubed-ext-eat-pod)
             (kubed-pod-prefix-map "G" kubed-ext-ghostel-pod)
             (kubed-pod-prefix-map "S" kubed-ext-eshell-pod)
             (kubed-pod-prefix-map "i" kubed-ext-logs-init-container)
             (kubed-nodes-mode-map "M" kubed-ext-top-nodes)
             (kubed-node-prefix-map "T" kubed-ext-top-nodes)
             (kubed-services-mode-map "F" kubed-ext-services-forward-port)
             (kubed-service-prefix-map "F" kubed-ext-forward-port-to-service)
             (kubed-deployments-mode-map "F" kubed-ext-deployments-forward-port)
             (kubed-deployments-mode-map "r" kubed-ext-deployments-rollout-history)
             (kubed-deployments-mode-map "j" kubed-ext-deployments-jab)
             (kubed-deployments-mode-map "U" kubed-ext-deployments-rollout-undo)
             (kubed-deployments-mode-map "M-u" kubed-ext-unmark-all)
             (kubed-deployments-mode-map "E" kubed-ext-deployments-logs-errors)
             (kubed-deployments-mode-map "H" kubed-ext-deployments-logs-custom-filter)
             (kubed-deployment-prefix-map "F" kubed-ext-forward-port-to-deployment)
             (kubed-deployment-prefix-map "r" kubed-ext-rollout-history)
             (kubed-deployment-prefix-map "j" kubed-ext-jab-deployment)
             (kubed-deployment-prefix-map "U" kubed-ext-rollout-undo)
             (kubed-list-mode-map "M-d" kubed-ext-list-describe-resource)
             (kubed-list-mode-map "S" kubed-ext-list-set-label-selector)
             (kubed-list-mode-map "c" kubed-ext-copy-popup)
             (kubed-list-mode-map "b" kubed-ext-switch-buffer)
             (kubed-list-mode-map "f" kubed-ext-set-filter)
             (kubed-list-mode-map "V" kubed-ext-list-wide)
             (kubed-list-mode-map "m" kubed-ext-mark-item)
             (kubed-list-mode-map "M-m" kubed-ext-mark-all)
             (kubed-list-mode-map "M-u" kubed-ext-unmark-all)
             (kubed-list-mode-map "M-n" kubed-ext-jump-to-next-highlight)
             (kubed-list-mode-map "M-p" kubed-ext-jump-to-previous-highlight)
             (kubed-list-mode-map "X" kubed-ext-delete-marked)
             (kubed-list-mode-map "A" kubed-ext-auto-refresh-mode)
             (kubed-list-mode-map "r" kubed-ext-switch-resource)
             (kubed-list-mode-map "$" kubed-ext-show-process-buffer)
             (kubed-list-mode-map "M-?" kubed-ext-transient-menu)
             (kubed-list-mode-map "C" kubed-ext-switch-context)
             (kubed-list-mode-map "N" kubed-ext-switch-namespace)
             (kubed-list-mode-map "R" kubed-ext-switch-resource)
             (kubed-list-mode-map "Z" kubed-ext-circuit-breaker-reset)
             (kubed-list-mode-map "C-c C-k" kubed-ext-kill-all-buffers)
             (kubed-prefix-map "D" kubed-ext-describe-resource)
             (kubed-prefix-map "F" kubed-ext-list-port-forwards)
             (kubed-prefix-map "b" kubed-ext-switch-buffer)
             (kubed-prefix-map "#" kubed-ext-show-command-log)
             (kubed-prefix-map "l L" kubed-ext-logs-by-label)
             (kubed-prefix-map "Q" kubed-ext-kill-all-buffers)
             (kubed-prefix-map "Z" kubed-ext-circuit-breaker-reset)
             (kubed-prefix-map "e" kubed-ext-logs-errors)
             (kubed-prefix-map "l f" kubed-ext-logs-by-label-filtered)
             (kubed-prefix-map "l p" kubed-ext-logs-parallel-by-label)
             (kubed-prefix-map "l o" kubed-ext-filter-log-buffer)
             (kubed-prefix-map "w" kubed-ext-logs-parallel-for-workload)))
    (pcase-let ((`(,map-symbol ,key ,command) binding))
      (kubed-ext--unset-key-if-bound map-symbol key command)))
  nil)

(provide 'kubed-ext)
;;; kubed-ext.el ends here
