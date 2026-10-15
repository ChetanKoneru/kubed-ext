;;; kubed-ext.el --- Modular Kubernetes navigation and lifecycle -*- lexical-binding: t; -*-
;; Author: Chetan Koneru
;; Version: 0.8.0
;; URL: https://github.com/CsBigDataHub/kubed-ext
;; Package-Requires: ((emacs "29.1") (kubed "0.7.1") (transient "0.4"))
;;; Commentary:
;; Require this entry point, or explicitly load its .el during development.
;; Modules own their implementations; this file installs reversible keys and
;; action-history advice only.  Native Kubed refresh/actions remain native.
;;; Code:
(require 'kubed)
(require 'transient)
(require 'cl-lib)
(require 'subr-x)
(require 'kubed-ext-core)
(require 'kubed-ext-resources)
(require 'kubed-ext-logs)
(require 'kubed-ext-actions)
(require 'kubed-ext-metrics)
(require 'kubed-ext-terminals)

(defvar kubed-ext--installed-bindings nil
  "Owned binding snapshots: (MAP KEY ORIGINAL INSTALLED).")
(defvar kubed-ext--command-history nil
  "Most recent explicit action argv, newest first; limited to 100 entries.")

(defun kubed-ext--install-binding (map key command)
  "Install COMMAND in MAP at KEY once, recording the original binding.
Repeated setup leaves later user changes alone.  MAP is the actual keymap,
so cleanup never restores into a replacement map installed by the user."
  (unless (cl-find-if (lambda (entry)
                       (and (eq (car entry) map) (equal (cadr entry) key)))
                     kubed-ext--installed-bindings)
    (push (list map key (lookup-key map (kbd key)) command)
          kubed-ext--installed-bindings)
    (define-key map (kbd key) command)))

(defun kubed-ext--record-action-command (&rest _args)
  "Record the argv produced by the action module, never shell process traffic."
  (when kubed-ext--last-action-command
    (push (copy-sequence kubed-ext--last-action-command)
          kubed-ext--command-history)
    (when (> (length kubed-ext--command-history) 100)
      (setcdr (nthcdr 99 kubed-ext--command-history) nil))))

;;;###autoload
(defun kubed-ext-disable ()
  "Remove integration and stop extension-owned requests, timers and sessions.
Do not unload shared modules or kill native Kubed buffers.  Later user key
changes survive.  Each owner is attempted even if another cleanup fails."
  (interactive)
  (dolist (entry kubed-ext--installed-bindings)
    (pcase-let ((`(,map ,key ,old . ,installed) entry))
      ;; Accept the previous entry point's three-field snapshots on reload.
      (when (eq (lookup-key map (kbd key))
                (if installed (car installed) #'kubed-ext-transient-menu))
        (define-key map (kbd key) old))))
  (setq kubed-ext--installed-bindings nil)
  (advice-remove 'kubed-ext--action-run #'kubed-ext--record-action-command)
  ;; Keep the guard across ALL owners, including process deletion sentinels.
  (let ((kubed-ext--unloading t))
    (dolist (cleanup '(kubed-ext--cancel-all-requests kubed-ext-resources-stop-all
                       kubed-ext-stop-all-logs kubed-ext-terminals-stop-all
                       kubed-ext-port-forwards-stop-all))
      (condition-case err (funcall cleanup)
        (error (display-warning 'kubed-ext
                                (format "Cleanup %s: %s" cleanup
                                        (error-message-string err))))))
    (kubed-ext--stop-owned-buffers #'kubed-ext--top-cancel)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'kubed-ext-top-mode)
          (kubed-ext--top-cancel)
          (setq header-line-format "Metrics stopped"))))))

(defun kubed-ext-unload-function ()
  "Disable entry integration without globally unloading its dependencies."
  (kubed-ext-disable)
  nil)

(defun kubed-ext--resource-view-p ()
  "Return non-nil in a native or enhanced resource list."
  (derived-mode-p 'kubed-list-mode 'kubed-ext-resources-mode))
(defun kubed-ext--enhanced-view-p ()
  "Return non-nil in an enhanced resource list."
  (derived-mode-p 'kubed-ext-resources-mode))
(defun kubed-ext--log-view-p ()
  "Return non-nil in a managed log buffer."
  (derived-mode-p 'kubed-ext-log-mode))
(defun kubed-ext--refreshable-p ()
  "Return non-nil when the current buffer has a supported refresh command."
  (or (kubed-ext--resource-view-p) (kubed-ext--log-view-p)
      (derived-mode-p 'kubed-ext-top-mode 'kubed-ext-port-forwards-mode
                      'kubed-ext-rollout-history-mode)))
(defun kubed-ext--selected-type-p (types)
  "Return non-nil if an exact selected row's type is in TYPES."
  (member (plist-get (kubed-ext--action-selected-scope) :type) types))
(defun kubed-ext--pod-selected-p ()
  "Return non-nil for a selected pod, not merely a pod list header."
  (kubed-ext--selected-type-p '("pod" "pods")))
(defun kubed-ext--workload-selected-p ()
  "Return non-nil for a rollout-supported workload row."
  (kubed-ext--selected-type-p '("deployment" "deployments" "statefulset"
                               "statefulsets" "daemonset" "daemonsets")))
(defun kubed-ext--logs-selected-p ()
  "Return non-nil for a Stern-supported selected row."
  (kubed-ext--selected-type-p '("pod" "pods" "deployment" "deployments"
                               "statefulset" "statefulsets" "daemonset"
                               "daemonsets" "replicaset" "replicasets"
                               "job" "jobs" "service" "services")))
(defun kubed-ext--forward-selected-p ()
  "Return non-nil for an explicitly forwardable selected row."
  (kubed-ext--selected-type-p '("pod" "pods" "service" "services"
                               "deployment" "deployments")))
(defun kubed-ext--row-selected-p ()
  "Return non-nil for a selected resource row."
  (and (kubed-ext--resource-view-p) (kubed-ext--action-selected-scope)))
(defun kubed-ext--strimzi-selected-p ()
  "Return non-nil for a selected Strimzi resource."
  (let ((type (plist-get (kubed-ext--action-selected-scope) :type)))
    (and type (string-suffix-p ".kafka.strimzi.io" type))))

(defun kubed-ext-refresh ()
  "Refresh the current enhanced/native/resource/log/metric workflow."
  (interactive)
  (cond ((kubed-ext--enhanced-view-p) (kubed-ext-refresh-resources))
        ((derived-mode-p 'kubed-list-mode) (call-interactively #'kubed-list-update))
        ((derived-mode-p 'kubed-ext-top-mode) (kubed-ext-top-refresh))
        ((kubed-ext--log-view-p) (kubed-ext-restart-logs))
        ((derived-mode-p 'kubed-ext-port-forwards-mode) (kubed-ext-port-forwards-refresh))
        ((derived-mode-p 'kubed-ext-rollout-history-mode) (kubed-ext-rollout-refresh))
        (t (user-error "No Kubed workflow to refresh here"))))

(defun kubed-ext-selected-logs ()
  "Stream the exact selected pod or supported workload, following output."
  (interactive)
  (let ((scope (kubed-ext--action-selected)))
    (apply #'kubed-ext-stern-logs
           (append (mapcar (lambda (key) (plist-get scope key))
                           '(:type :name :context :namespace))
                   '(nil t)))))

(defun kubed-ext-view-top-pods ()
  "Open pod metrics in the current list scope, including from its header."
  (interactive)
  (let ((scope (kubed-ext--current-scope)))
    (kubed-ext-top-pods (plist-get scope :context) (plist-get scope :namespace))))
(defun kubed-ext-view-top-nodes ()
  "Open node metrics in the current list context, including from its header."
  (interactive)
  (kubed-ext-top-nodes (plist-get (kubed-ext--current-scope) :context)))

(defun kubed-ext-toggle-output-format ()
  "Toggle the format of subsequent resource displays between YAML and JSON."
  (interactive)
  (kubed-ext-set-output-format (if (equal kubed-ext-output-format "yaml") "json" "yaml")))
(defun kubed-ext-set-output-format (&optional format)
  "Set subsequent resource displays to FORMAT, either yaml or json.
Interactively use completion; without FORMAT ask for it."
  (interactive)
  (let ((value (or format (completing-read "Output format: " '("yaml" "json") nil t))))
    (unless (member value '("yaml" "json")) (user-error "Use yaml or json"))
    (setq kubed-ext-output-format value)
    (message "Resource output: %s (next display)" value)))

(defun kubed-ext-list-set-label-selector (selector)
  "Open the current resource scope with server-side label SELECTOR.
Empty SELECTOR clears filtering.  Native lists open an isolated enhanced view."
  (interactive "sLabel selector (empty clears): ")
  (let ((scope (kubed-ext--current-scope)))
    (unless scope (user-error "Not a resource list"))
    (kubed-ext-list-resources (plist-get scope :type) (plist-get scope :context)
                             (plist-get scope :namespace)
                             (unless (string-empty-p (string-trim selector)) selector))))

(defun kubed-ext--workflow-buffer-p (buffer)
  "Return non-nil if BUFFER has a native or enhanced Kubernetes workflow."
  (with-current-buffer buffer
    (or (kubed-ext--refreshable-p) kubed-ext--terminal-scope
        kubed-ext--action-scope)))
(defun kubed-ext-switch-buffer ()
  "Switch to a live native or enhanced Kubernetes workflow using completion."
  (interactive)
  (let ((names (mapcar #'buffer-name
                       (cl-remove-if-not #'kubed-ext--workflow-buffer-p (buffer-list)))))
    (unless names (user-error "No Kubernetes workflow buffers"))
    (pop-to-buffer (completing-read "Kubernetes buffer: " names nil t))))
(defun kubed-ext-show-command-log ()
  "Display up to 100 explicit action commands recorded while setup is active.
This is not a global kubectl/process audit; discovery and streaming are not
recorded.  Commands are shell-quoted for copying, never executed here."
  (interactive)
  (let ((buffer (get-buffer-create "*kubed-command-log*")))
    (with-current-buffer buffer
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Explicit action history (newest first; no generic process advice)\n\n")
        (dolist (argv (or kubed-ext--command-history
                          (and kubed-ext--last-action-command
                               (list kubed-ext--last-action-command))))
          (insert (mapconcat #'shell-quote-argument argv " ") "\n"))))
    (pop-to-buffer buffer)))

(defalias 'kubed-ext-list-ingresss #'kubed-list-ingresses)
(defalias 'kubed-ext-list-ingressclasss #'kubed-list-ingressclasses)

(defun kubed-ext--jump-resource (type)
  "Open enhanced TYPE preserving current context, namespace and selector."
  (let ((scope (kubed-ext--current-scope)))
    (kubed-ext-list-resources type (plist-get scope :context)
                             (plist-get scope :namespace)
                             (plist-get scope :selector))))
(defun kubed-ext-jump-pods ()
  "Open pods in the current list scope."
  (interactive) (kubed-ext--jump-resource "pods"))
(defun kubed-ext-jump-deployments ()
  "Open deployments in the current list scope."
  (interactive) (kubed-ext--jump-resource "deployments"))
(defun kubed-ext-jump-services ()
  "Open services in the current list scope."
  (interactive) (kubed-ext--jump-resource "services"))
(defun kubed-ext-jump-jobs ()
  "Open jobs in the current list scope."
  (interactive) (kubed-ext--jump-resource "jobs"))
(defun kubed-ext-jump-configmaps ()
  "Open ConfigMaps in the current list scope."
  (interactive) (kubed-ext--jump-resource "configmaps"))
(defun kubed-ext-jump-secrets ()
  "Open Secret metadata tables, not Secret values, in the current scope."
  (interactive) (kubed-ext--jump-resource "secrets"))
(defun kubed-ext-jump-ingresses ()
  "Open ingresses in the current list scope."
  (interactive) (kubed-ext--jump-resource "ingresses"))
(defun kubed-ext-jump-pvcs ()
  "Open persistent volume claims in the current list scope."
  (interactive) (kubed-ext--jump-resource "persistentvolumeclaims"))

(defun kubed-ext-jab-deployment (deployment &optional context namespace)
  "Confirm a rollout restart of DEPLOYMENT in CONTEXT/NAMESPACE."
  (interactive
   (let ((scope (kubed-ext--action-selected)))
     (unless (member (plist-get scope :type) '("deployment" "deployments"))
       (user-error "Select a deployment"))
     (list (plist-get scope :name) (plist-get scope :context)
           (plist-get scope :namespace))))
  (kubed-ext-rollout-restart "deployments" deployment context namespace))
(defun kubed-ext-deployments-jab (click)
  "Confirm rollout restart of the deployment selected at CLICK."
  (interactive (list last-nonmenu-event))
  (let ((scope (kubed-ext--action-selected click)))
    (unless (member (plist-get scope :type) '("deployment" "deployments"))
      (user-error "Select a deployment"))
    (kubed-ext--at-scope scope
      (kubed-ext-jab-deployment (plist-get scope :name) (plist-get scope :context)
                               (plist-get scope :namespace)))))

(transient-define-prefix kubed-ext-transient-menu ()
  "Context-aware Kubernetes workflows; native list refresh remains native."
  [["Navigate"
    ("r" "Switch resource (enhanced)" kubed-ext-switch-resource)
    ("c" "Switch context" kubed-ext-switch-context)
    ("n" "Switch namespace" kubed-ext-switch-namespace)
    ("b" "Switch buffer" kubed-ext-switch-buffer)
    ("g" "Refresh current workflow" kubed-ext-refresh :if kubed-ext--refreshable-p)]
   ["Resource views"
    ("/" "Literal filter" kubed-ext-set-filter :if kubed-ext--enhanced-view-p)
    ("L" "Server label selector" kubed-ext-list-set-label-selector :if kubed-ext--resource-view-p)
    ("a" "Auto-refresh" kubed-ext-auto-refresh-mode :if kubed-ext--enhanced-view-p)
    ("p" "Pod metrics" kubed-ext-view-top-pods :if kubed-ext--resource-view-p)
    ("N" "Node metrics" kubed-ext-view-top-nodes :if kubed-ext--resource-view-p)
    ("C" "Refresh CRD metadata" kubed-ext-refresh-crd-columns :if kubed-ext--resource-view-p)]
   ["Selected row"
    ("RET" "Show YAML/JSON" kubed-ext-show-resource :if kubed-ext--row-selected-p)
    ("d" "Describe" kubed-ext-describe-resource :if kubed-ext--row-selected-p)
    ("y" "Copy popup" kubed-ext-copy-popup :if kubed-ext--row-selected-p)
    ("l" "Follow logs" kubed-ext-selected-logs :if kubed-ext--logs-selected-p)
    ("e" "Error logs (RE2)" kubed-ext-logs-errors :if kubed-ext--logs-selected-p)
    ("h" "Rollout history" kubed-ext-rollout-history :if kubed-ext--workload-selected-p)
    ("R" "Restart rollout (confirm)" kubed-ext-rollout-restart :if kubed-ext--workload-selected-p)
    ("f" "Forward port" kubed-ext-pods-forward-port :if kubed-ext--forward-selected-p)
    ("D" "Delete (confirm)" kubed-ext-delete-resource :if kubed-ext--row-selected-p)]
   ["Pod / Strimzi"
    ("v" "vterm" kubed-ext-vterm-pod :if kubed-ext--pod-selected-p)
    ("s" "eshell" kubed-ext-eshell-pod :if kubed-ext--pod-selected-p)
    ("!" "Exec command" kubed-ext-pods-command :if kubed-ext--pod-selected-p)
    ("i" "Init-container logs" kubed-ext-logs-init-container :if kubed-ext--pod-selected-p)
    ("P" "Pause reconciliation" kubed-ext-strimzi-pause-at-point :if kubed-ext--strimzi-selected-p)
    ("S" "Resume reconciliation" kubed-ext-strimzi-resume-at-point :if kubed-ext--strimzi-selected-p)]
   ["Sessions / output"
    ("F" "Port forwards" kubed-ext-list-port-forwards)
    ("o" "Toggle YAML/JSON" kubed-ext-toggle-output-format)
    ("H" "Action command history" kubed-ext-show-command-log)
    ("k" "Stop this log" kubed-ext-stop-logs :if kubed-ext--log-view-p)
    ("t" "Follow at end" kubed-ext-log-toggle-follow :if kubed-ext--log-view-p)
    ("E" "Live RE2 filter" kubed-ext-log-set-filter :if kubed-ext--log-view-p)
    ("q" "Close menu" transient-quit-one)
    ("Q" "Disable extension" kubed-ext-disable)]])

;;;###autoload
(defun kubed-ext-setup ()
  "Install idempotent, reversible integration without contacting a cluster.
Native Kubed commands stay native.  Enhanced mode workflow keys supplement
resource-module marking/filter/refresh keys without overriding their functions."
  (interactive)
  (dolist (map (list kubed-list-mode-map kubed-ext-resources-mode-map
                     kubed-ext-log-mode-map kubed-ext-top-mode-map))
    (kubed-ext--install-binding map "?" #'kubed-ext-transient-menu))
  (dolist (binding '(("RET" . kubed-ext-show-resource)
                     ("d" . kubed-ext-describe-resource)
                     ("y" . kubed-ext-copy-popup)
                     ("l" . kubed-ext-selected-logs)
                     ("e" . kubed-ext-logs-errors)
                     ("p" . kubed-ext-view-top-pods)
                     ("N" . kubed-ext-view-top-nodes)
                     ("h" . kubed-ext-rollout-history)
                     ("R" . kubed-ext-rollout-restart)
                     ("f" . kubed-ext-pods-forward-port)
                     ("F" . kubed-ext-list-port-forwards)
                     ("v" . kubed-ext-vterm-pod)
                     ("!" . kubed-ext-pods-command)
                     ("D" . kubed-ext-delete-resource)
                     ("x" . kubed-ext-delete-marked)
                     ("L" . kubed-ext-list-set-label-selector)
                     ("b" . kubed-ext-switch-buffer)))
    (kubed-ext--install-binding kubed-ext-resources-mode-map
                                (car binding) (cdr binding)))
  (advice-add 'kubed-ext--action-run :after #'kubed-ext--record-action-command)
  t)

(kubed-ext-setup)
(provide 'kubed-ext)
;;; kubed-ext.el ends here
