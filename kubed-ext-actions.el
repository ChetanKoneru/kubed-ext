;;; kubed-ext-actions.el --- Scoped asynchronous actions -*- lexical-binding: t; -*-
;;; Commentary:
;; Actions capture their origin and never refresh an unrelated selected view.
;;; Code:
(require 'kubed-ext-core)
(require 'kubed-ext-resources)
(require 'tabulated-list)
(require 'json)

(defcustom kubed-ext-output-format "yaml" "Resource output format."
  :type '(choice (const "yaml") (const "json")) :group 'kubed-ext)

(defcustom kubed-ext-highlight-output t
  "Syntax-highlight resource output instead of showing it as plain text."
  :type 'boolean :group 'kubed-ext)

(defcustom kubed-ext-highlight-describe t
  "Highlight `kubectl describe' output with the YAML mode.
Describe output is not YAML, but its indented key/value shape highlights
closely enough to be useful."
  :type 'boolean :group 'kubed-ext)

(defcustom kubed-ext-yaml-modes '(yaml-ts-mode yaml-mode)
  "Major modes tried for YAML output, most preferred first.
Tree-sitter modes are skipped when their grammar is unavailable."
  :type '(repeat symbol) :group 'kubed-ext)

(defcustom kubed-ext-json-modes '(json-ts-mode json-mode js-json-mode)
  "Major modes tried for JSON output, most preferred first.
Tree-sitter modes are skipped when their grammar is unavailable."
  :type '(repeat symbol) :group 'kubed-ext)

(defvar-local kubed-ext--output-mode nil
  "Major mode chosen for this output buffer, or nil for plain text.")

(defvar kubed-ext-output-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "q" #'quit-window)
    map)
  "Keys added to highlighted output buffers, which are not `special-mode'.")

(defun kubed-ext--output-mode-for (format)
  "Return an available major mode for FORMAT, or nil to keep plain text.
FORMAT is `yaml', `json' or `describe'."
  (when kubed-ext-highlight-output
    (let ((candidates (pcase format
                        ("json" kubed-ext-json-modes)
                        ("yaml" kubed-ext-yaml-modes)
                        ("describe" (and kubed-ext-highlight-describe
                                         kubed-ext-yaml-modes)))))
      (seq-find (lambda (mode)
                  (and (fboundp mode)
                       ;; A tree-sitter mode without its grammar fails loudly.
                       (or (not (string-suffix-p "-ts-mode" (symbol-name mode)))
                           (and (fboundp 'treesit-ready-p)
                                (treesit-ready-p
                                 (intern (string-remove-suffix
                                          "-ts-mode" (symbol-name mode)))
                                 t)))))
                candidates))))

(defun kubed-ext--apply-output-mode (format)
  "Prepare the current buffer to display FORMAT output.
Fall back to `special-mode' when no highlighting mode is available.  The
major mode is selected before any buffer-local state is stored, because
changing it afterwards would discard that state."
  (let ((mode (kubed-ext--output-mode-for format)))
    (if (not mode)
        (unless (derived-mode-p 'special-mode) (special-mode))
      (unless (eq major-mode mode) (funcall mode))
      (setq buffer-read-only t)
      (use-local-map (make-composed-keymap kubed-ext-output-mode-map
                                           (current-local-map))))
    (setq-local kubed-ext--output-mode mode)))
(defcustom kubed-ext-port-forward-address "::1" "Port-forward bind address."
  :type 'string :group 'kubed-ext)
(defvar kubed-ext--port-forwards nil "Owned forwarding session plists.")
(defvar kubed-ext--last-action-command nil "Last explicit action argv.")
(defvar-local kubed-ext--action-scope nil)
(defvar-local kubed-ext--action-environment nil)
(defvar-local kubed-ext--rollout-origin nil)

(defun kubed-ext--action-scope-args (scope)
  "Return explicit context and namespace arguments for SCOPE."
  (append (when (plist-get scope :context)
            (list "--context" (plist-get scope :context)))
          (when (plist-get scope :namespace)
            (list "--namespace" (plist-get scope :namespace)))))

(defun kubed-ext--action-scope (type name context namespace)
  "Validate TYPE and NAME, returning their CONTEXT/NAMESPACE scope."
  (dolist (value (list type name))
    (unless (and (stringp value) (string-match-p "\\`[[:alnum:]][[:alnum:]._-]*\\'" value))
      (user-error "Invalid resource type or name: %S" value)))
  (dolist (value (list context namespace))
    (when (and value (or (not (stringp value)) (string-empty-p value)
                         (string-prefix-p "-" value)))
      (user-error "Invalid explicit context or namespace: %S" value)))
  (let ((selected (kubed-ext-selected-scope)))
    (list :type type :name name :context context :namespace namespace
          :environment (or (and (equal type (plist-get selected :type))
                                (equal name (plist-get selected :name))
                                (plist-get selected :environment))
                           kubed-ext--environment (kubed-ext--capture-environment)))))

(defun kubed-ext--action-selected-scope (&optional event)
  "Adapt current and planned resource scope APIs, preserving EVENT's buffer."
  (if (mouse-event-p event)
      (let* ((pos (event-start event)) (window (posn-window pos)) (point (posn-point pos)))
        (when (and (windowp window) (integer-or-marker-p point))
          (with-current-buffer (window-buffer window)
            (save-excursion (goto-char point) (kubed-ext--action-selected-scope)))))
    (let ((scope (kubed-ext-selected-scope)))
      (when scope (append (list :origin (current-buffer)) scope)))))

(defun kubed-ext--action-marked-scopes ()
  "Resolve marked IDs to exact row scopes; accept planned plist API too."
  (let ((items (kubed-ext-marked-items)))
    (mapcar
     (lambda (item)
       (if (and (listp item) (plist-get item :name)) item
         (or (and (boundp 'kubed-ext--view-identities)
                  kubed-ext--view-identities (gethash item kubed-ext--view-identities))
             (save-excursion
               (goto-char (point-min))
               (while (and (not (eobp)) (not (equal item (tabulated-list-get-id)))) (forward-line 1))
               (or (kubed-ext--action-selected-scope) (user-error "Marked row %s no longer exists" item))))))
     items)))

(defun kubed-ext--action-refresh ()
  "Refresh using the available resource API."
  (if (derived-mode-p 'kubed-ext-resources-mode)
      (kubed-ext-refresh-resources)
    (when (derived-mode-p 'kubed-list-mode) (kubed-list-update t))))

(defun kubed-ext--action-selected (&optional event)
  "Return validated scope at EVENT or point, or ask for an explicit target."
  (let ((scope (or (kubed-ext--action-selected-scope event) kubed-ext--action-scope)))
    (or scope (kubed-ext--action-scope
               (read-string "Resource type: ") (read-string "Resource name: ")
               (read-string "Context: ") (let ((n (read-string "Namespace (empty for cluster scope): ")))
                                          (unless (string-empty-p n) n))))))

(defun kubed-ext--scoped-argv (verb type name context namespace &rest args)
  "Build a full executable argv for VERB TYPE NAME and optional ARGS."
  (let ((scope (kubed-ext--action-scope type name context namespace)))
    (append (list (kubed-ext--kubectl-program) verb type name)
            (kubed-ext--action-scope-args scope) args)))

(defun kubed-ext--action-output-buffer (title)
  "Allocate an independent action result buffer with TITLE."
  (generate-new-buffer (format "*Kubed %s*" title)))

(defun kubed-ext--show-action-output (target output &optional failure)
  "Render OUTPUT in TARGET, with FAILURE indicated in its header."
  (let ((buffer (if (bufferp target) target (kubed-ext--action-output-buffer target))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (unless (or kubed-ext--output-mode (derived-mode-p 'special-mode))
            (special-mode))
          (erase-buffer) (insert (or output ""))
          (when kubed-ext--output-mode (font-lock-ensure))
          (setq header-line-format
                (concat (if failure "Request failed — " "Request complete — ")
                        (kubed-ext--scope-label kubed-ext--action-scope)))))
      (unless (bufferp target) (display-buffer buffer))
      buffer)))

(defun kubed-ext--action-refresh-origin (origin scope environment &optional launch-list)
  "Refresh ORIGIN if its list still matches LAUNCH-LIST or explicit SCOPE."
  (when (buffer-live-p origin)
    (with-current-buffer origin
      (let ((current (kubed-ext--current-scope)))
        (when (and current
                   (or (and launch-list (equal launch-list current))
                       (and (not launch-list)
                   (cl-every (lambda (key) (equal (plist-get scope key) (plist-get current key)))
                             '(:type :context))
                   (or (plist-get current :all-namespaces)
                       (equal (plist-get scope :namespace) (plist-get current :namespace)))))
                   (equal (kubed-ext--environment-key environment)
                          (kubed-ext--environment-key (plist-get current :environment))))
          (kubed-ext--action-refresh))))))

(defun kubed-ext--action-run (scope arguments title &optional mutation origin)
  "Resolve SCOPE once and run ARGUMENTS, retaining TITLE and ORIGIN."
  (let* ((origin (or origin (plist-get scope :origin) kubed-ext--rollout-origin (current-buffer)))
         (launch-list (and (buffer-live-p origin)
                           (with-current-buffer origin (kubed-ext--current-scope)))))
    (setq scope (plist-put (copy-sequence scope) :launch-list launch-list))
    (kubed-ext--resolve-scope
     scope (if (null (kubed-ext--resource-namespaced-p (plist-get scope :type)
                                                      (plist-get scope :context))) 'all 'default)
     (lambda (resolved)
       (kubed-ext--action-run-resolved resolved arguments title mutation origin)))))

(defun kubed-ext--action-run-resolved (scope arguments title &optional mutation origin)
  "Run resolved ARGUMENTS in SCOPE, displaying TITLE; refresh ORIGIN on MUTATION."
  (let* ((environment (or (plist-get scope :environment) kubed-ext--action-environment
                          kubed-ext--environment (kubed-ext--capture-environment)))
         (origin (or origin kubed-ext--rollout-origin (current-buffer)))
         (argv (append (list (kubed-ext--with-environment environment (kubed-ext--kubectl-program))) arguments
                       (kubed-ext--action-scope-args scope)))
         (buffer (kubed-ext--action-output-buffer
                  (format "%s %s/%s [%s/%s]" title (plist-get scope :type)
                          (plist-get scope :name) (plist-get scope :context)
                          (plist-get scope :namespace)))))
    (when (and mutation (not (plist-get scope :context)))
      (kill-buffer buffer)
      (user-error "An explicit context is required for mutation"))
    (setq kubed-ext--last-action-command argv)
    (with-current-buffer buffer
      (if (equal (car arguments) "rollout")
          (kubed-ext-rollout-history-mode)
        (kubed-ext--apply-output-mode (car arguments)))
      (kubed-ext--apply-environment-to-new-buffer environment)
      (setq-local kubed-ext--action-scope scope
                  kubed-ext--action-environment environment
                  kubed-ext--rollout-origin origin
                  header-line-format "Request pending"))
    (display-buffer buffer)
    (kubed-ext--with-environment environment
      (kubed-ext--async-kubectl
       argv (lambda (output)
              (kubed-ext--show-action-output buffer output)
              (when mutation (kubed-ext--action-refresh-origin origin scope environment
                                                              (plist-get scope :launch-list))))
       (lambda (error) (kubed-ext--show-action-output buffer error t))))))

(defun kubed-ext--action-interactive ()
  "Read action arguments from the selected row."
  (let ((s (kubed-ext--action-selected)))
    (mapcar (lambda (k) (plist-get s k)) '(:type :name :context :namespace))))

(defun kubed-ext-describe-resource (type name &optional context namespace)
  "Describe TYPE/NAME in CONTEXT and NAMESPACE asynchronously."
  (interactive (kubed-ext--action-interactive))
  (kubed-ext--action-run (kubed-ext--action-scope type name context namespace)
                         (list "describe" type name) "describe"))

(defun kubed-ext--resource-output (type name context namespace format copy)
  "Show FORMAT output for TYPE/NAME in CONTEXT/NAMESPACE; optionally COPY."
  (when (member (downcase type) '("secret" "secrets"))
    (user-error "Secret-value retrieval is not provided by this extension"))
  (let ((scope (kubed-ext--action-scope type name context namespace)))
    (kubed-ext--resolve-scope
     scope (if (null (kubed-ext--resource-namespaced-p type context)) 'all 'default)
     (lambda (resolved)
       (let* ((environment (plist-get resolved :environment))
              (buffer (kubed-ext--action-output-buffer (format "%s %s" format (kubed-ext--scope-label resolved)))))
         (with-current-buffer buffer
           (kubed-ext--apply-output-mode format)
           (kubed-ext--apply-environment-to-new-buffer environment)
           (setq-local kubed-ext--action-scope resolved kubed-ext--action-environment environment
                       header-line-format "Request pending"))
         (display-buffer buffer)
         (kubed-ext--async-kubectl
          (append (list (kubed-ext--kubectl-program) "get" type name "-o" format)
                  (kubed-ext--action-scope-args resolved))
          (lambda (text) (when copy (kill-new text)) (kubed-ext--show-action-output buffer text))
          (lambda (error) (kubed-ext--show-action-output buffer error t))))))))

(defun kubed-ext-copy-resource-as-yaml (type name &optional context namespace)
  "Fetch and copy YAML for TYPE/NAME in CONTEXT/NAMESPACE."
  (interactive (kubed-ext--action-interactive))
  (kubed-ext--resource-output type name context namespace "yaml" t))

(defun kubed-ext-copy-resource-as-json (type name &optional context namespace)
  "Fetch and copy JSON for TYPE/NAME in CONTEXT/NAMESPACE."
  (interactive (kubed-ext--action-interactive))
  (kubed-ext--resource-output type name context namespace "json" t))

(defun kubed-ext-show-resource (type name &optional context namespace)
  "Display TYPE/NAME using `kubed-ext-output-format'."
  (interactive (kubed-ext--action-interactive))
  (kubed-ext--resource-output type name context namespace kubed-ext-output-format nil))

(defun kubed-ext-wide-describe-resource ()
  "Describe the selected resource in an independent output buffer."
  (interactive) (call-interactively #'kubed-ext-describe-resource))

(defun kubed-ext-list-describe-resource (click)
  "Describe the row at CLICK without changing its scope."
  (interactive (list last-nonmenu-event))
  (let ((s (kubed-ext--action-selected click)))
    (kubed-ext--at-scope s
      (apply #'kubed-ext-describe-resource
             (mapcar (lambda (k) (plist-get s k)) '(:type :name :context :namespace))))))

(defun kubed-ext-list-copy-resource-as-yaml (click)
  "Copy the row at CLICK as YAML asynchronously."
  (interactive (list last-nonmenu-event))
  (let ((s (kubed-ext--action-selected click)))
    (kubed-ext--at-scope s
      (apply #'kubed-ext-copy-resource-as-yaml
             (mapcar (lambda (k) (plist-get s k)) '(:type :name :context :namespace))))))

(defun kubed-ext-list-copy-kubectl-prefix ()
  "Copy a shell-quoted kubectl prefix with selected scope."
  (interactive)
  (kill-new (mapconcat #'shell-quote-argument
                       (cons (kubed-ext--kubectl-program)
                             (kubed-ext--action-scope-args (kubed-ext--action-selected))) " ")))

(defun kubed-ext-list-copy-last-command ()
  "Copy the most recent explicit action argv as a shell command."
  (interactive)
  (unless kubed-ext--last-action-command (user-error "No action command recorded"))
  (kill-new (mapconcat #'shell-quote-argument kubed-ext--last-action-command " ")))

(defun kubed-ext-list-copy-log-command (click)
  "Copy a kubectl log command for the exact row at CLICK."
  (interactive (list last-nonmenu-event))
  (let ((s (kubed-ext--action-selected click)))
    (kill-new (mapconcat #'shell-quote-argument
                         (append (list (kubed-ext--kubectl-program) "logs"
                                       (format "%s/%s" (plist-get s :type) (plist-get s :name)))
                                 (kubed-ext--action-scope-args s) '("--follow")) " "))))

(defun kubed-ext--resolve-confirmation-scope (scope callback)
  "Resolve SCOPE before CALLBACK prompts, deferring only asynchronous prompts."
  (let* ((origin (current-buffer))
         (policy (if (null (kubed-ext--resource-namespaced-p (plist-get scope :type)
                                                           (plist-get scope :context))) 'all 'default)))
    (if (and (plist-get scope :context)
             (or (plist-get scope :namespace) (eq policy 'all)))
        (funcall callback scope)
      (kubed-ext--resolve-scope
       scope policy
       (lambda (resolved)
         (kubed-ext--defer-action-prompt (plist-get resolved :environment) origin
                                         (lambda () (funcall callback resolved))))))))

(defun kubed-ext-delete-resource (type name &optional context namespace)
  "Delete TYPE/NAME in CONTEXT/NAMESPACE after scope-specific confirmation."
  (interactive (kubed-ext--action-interactive))
  (let ((s (kubed-ext--action-scope type name context namespace)) (origin (current-buffer)))
    (kubed-ext--resolve-confirmation-scope
     s (lambda (resolved)
         (when (yes-or-no-p (format "Delete %s? " (kubed-ext--scope-label resolved)))
           (kubed-ext--action-run resolved (list "delete" type name) "delete" t origin))))))

(defun kubed-ext-delete-marked ()
  "Confirm and delete marked rows using each row's exact namespace."
  (interactive)
  (let ((items (kubed-ext--action-marked-scopes)) (origin (current-buffer))
        (environment (kubed-ext--capture-environment)))
    (unless items (user-error "No marked resources"))
    (if (cl-some (lambda (s) (or (not (plist-get s :context))
                                (and (kubed-ext--resource-namespaced-p (plist-get s :type)
                                                                      (plist-get s :context))
                                     (not (plist-get s :namespace))))) items)
        (let (resolved)
          (cl-labels ((next (remaining)
                        (if remaining
                            (kubed-ext--resolve-confirmation-scope
                             (car remaining) (lambda (s) (push s resolved) (next (cdr remaining))))
                          (kubed-ext--delete-marked-resolved (nreverse resolved) origin environment))))
            (next items)))
      (kubed-ext--delete-marked-resolved items origin environment))))
(defun kubed-ext--delete-marked-resolved (items origin environment)
  "Confirm and delete explicit ITEMS at ORIGIN in ENVIRONMENT."
    (when (yes-or-no-p (format "Delete these resources?\n%s\n"
                              (mapconcat (lambda (s) (format "%s/%s [%s/%s]" (plist-get s :type)
                                                             (plist-get s :name) (plist-get s :context)
                                                             (plist-get s :namespace))) items "\n")))
      (dolist (s items)
        (kubed-ext--with-environment environment
          (kubed-ext--action-scope (plist-get s :type) (plist-get s :name)
                                  (plist-get s :context) (plist-get s :namespace))
          (kubed-ext--action-run s (list "delete" (plist-get s :type) (plist-get s :name))
                                 "delete marked" t origin)))))

(defun kubed-ext--rollout (verb type name context namespace &optional revision mutation)
  "Perform rollout VERB for TYPE/NAME, with REVISION and MUTATION intent."
  (unless (member type '("deployment" "deployments" "daemonset" "daemonsets" "statefulset" "statefulsets"))
    (user-error "Rollout is not supported for %s" type))
  (when revision (unless (and (integerp revision) (> revision 0)) (user-error "Revision must be positive")))
  (let ((s (kubed-ext--action-scope type name context namespace))
        (origin (or kubed-ext--rollout-origin (current-buffer))))
    (kubed-ext--resolve-confirmation-scope
     s (lambda (resolved)
         (when (or (not mutation)
                   (yes-or-no-p (format "Rollout %s %s%s? " verb (kubed-ext--scope-label resolved)
                                        (if revision (format " revision %d" revision) ""))))
           (kubed-ext--action-run
            resolved
            (append (list "rollout" verb (format "%s/%s" type name))
                    (when revision (list (format "%s=%d"
                                               (if (equal verb "undo") "--to-revision" "--revision") revision))))
            (concat "rollout " verb) mutation origin))))))

(defun kubed-ext-rollout-history (type name &optional context namespace)
  "Show rollout history for TYPE/NAME in CONTEXT/NAMESPACE."
  (interactive (kubed-ext--action-interactive))
  (kubed-ext--rollout "history" type name context namespace))
(defun kubed-ext-rollout-restart (type name &optional context namespace)
  "Confirm and restart TYPE/NAME in CONTEXT/NAMESPACE."
  (interactive (kubed-ext--action-interactive))
  (kubed-ext--rollout "restart" type name context namespace nil t))
(defun kubed-ext-rollout-undo (type name &optional revision context namespace)
  "Confirm and undo TYPE/NAME to REVISION in CONTEXT/NAMESPACE."
  (interactive (let ((a (kubed-ext--action-interactive)))
                 (list (nth 0 a) (nth 1 a) (read-number "Revision: ") (nth 2 a) (nth 3 a))))
  (kubed-ext--rollout "undo" type name context namespace revision t))
(defun kubed-ext-rollout-show-revision ()
  "Show a revision of the selected workload's rollout history."
  (interactive)
  (let ((a (kubed-ext--action-interactive)))
    (kubed-ext--rollout "history" (nth 0 a) (nth 1 a) (nth 2 a) (nth 3 a)
                       (read-number "Revision: "))))
(defun kubed-ext-rollout-undo-from-history ()
  "Undo a workload selected by its captured result-buffer scope."
  (interactive)
  (let ((s (or kubed-ext--action-scope (kubed-ext--action-selected))))
    (kubed-ext--at-scope s
      (kubed-ext-rollout-undo (plist-get s :type) (plist-get s :name) (read-number "Revision: ")
                             (plist-get s :context) (plist-get s :namespace)))))
(defun kubed-ext-rollout-refresh ()
  "Refresh history for the captured workload scope."
  (interactive)
  (let ((s (or kubed-ext--action-scope (kubed-ext--action-selected))))
    (apply #'kubed-ext-rollout-history
           (mapcar (lambda (k) (plist-get s k)) '(:type :name :context :namespace)))))
(defalias 'kubed-ext-deployments-rollout-history #'kubed-ext-list-rollout-history)
(defun kubed-ext-list-rollout-history (click)
  "Show history for workload at CLICK."
  (interactive (list last-nonmenu-event))
  (let ((s (kubed-ext--action-selected click)))
    (kubed-ext--at-scope s
      (apply #'kubed-ext-rollout-history (mapcar (lambda (k) (plist-get s k)) '(:type :name :context :namespace))))))
(defun kubed-ext-deployments-rollout-undo (click)
  "Undo the deployment at CLICK after confirmation."
  (interactive (list last-nonmenu-event))
  (let ((s (kubed-ext--action-selected click)))
    (kubed-ext-rollout-undo (plist-get s :type) (plist-get s :name) (read-number "Revision: ")
                           (plist-get s :context) (plist-get s :namespace))))

(defun kubed-ext--validate-count (count label &optional positive)
  "Validate integer COUNT for LABEL; POSITIVE disallows zero."
  (unless (and (integerp count) (>= count (if positive 1 0)))
    (user-error "%s must be a %s integer" label (if positive "positive" "nonnegative"))) count)
(defun kubed-ext--strimzi-type-p (type)
  "Whether TYPE is an explicitly qualified Strimzi resource."
  (and (stringp type) (string-suffix-p ".kafka.strimzi.io" type)))
(defun kubed-ext--strimzi-change (type name context namespace arguments title)
  "Confirm Strimzi ARGUMENTS for TYPE/NAME under TITLE."
  (unless (kubed-ext--strimzi-type-p type) (user-error "Expected a qualified Strimzi resource"))
  (let ((s (kubed-ext--action-scope type name context namespace)) (origin (current-buffer)))
    (kubed-ext--resolve-confirmation-scope
     s (lambda (resolved)
         (when (yes-or-no-p (format "%s %s? " title (kubed-ext--scope-label resolved)))
           (kubed-ext--action-run resolved arguments title t origin))))))
(defun kubed-ext-strimzi-pause-reconciliation (type name &optional context namespace)
  "Confirm and pause reconciliation of Strimzi TYPE/NAME."
  (interactive (kubed-ext--action-interactive))
  (kubed-ext--strimzi-change type name context namespace
                            (list "annotate" type name "strimzi.io/pause-reconciliation=true" "--overwrite") "Pause reconciliation"))
(defun kubed-ext-strimzi-resume-reconciliation (type name &optional context namespace)
  "Confirm and remove the reconciliation pause annotation on TYPE/NAME."
  (interactive (kubed-ext--action-interactive))
  (kubed-ext--strimzi-change type name context namespace
                            (list "annotate" type name "strimzi.io/pause-reconciliation-" "--overwrite") "Resume reconciliation"))
(defun kubed-ext-strimzi-restart-connector (name &optional context namespace)
  "Confirm and restart Kafka connector NAME in CONTEXT/NAMESPACE."
  (interactive (let ((s (kubed-ext--action-selected)))
                 (list (plist-get s :name) (plist-get s :context) (plist-get s :namespace))))
  (kubed-ext--strimzi-change "kafkaconnectors.kafka.strimzi.io" name context namespace
                            (list "annotate" "kafkaconnectors.kafka.strimzi.io" name
                                  "strimzi.io/restart=true" "--overwrite") "Restart connector"))
(defun kubed-ext-strimzi-pause-at-point ()
  "Pause reconciliation of the selected Strimzi resource."
  (interactive) (call-interactively #'kubed-ext-strimzi-pause-reconciliation))
(defun kubed-ext-strimzi-resume-at-point ()
  "Resume reconciliation of the selected Strimzi resource."
  (interactive) (call-interactively #'kubed-ext-strimzi-resume-reconciliation))
(defun kubed-ext-strimzi-restart-connector-at-point ()
  "Restart the selected Kafka connector."
  (interactive)
  (let ((s (kubed-ext--action-selected)))
    (unless (equal (plist-get s :type) "kafkaconnectors.kafka.strimzi.io") (user-error "Select a KafkaConnector"))
    (kubed-ext-strimzi-restart-connector (plist-get s :name) (plist-get s :context) (plist-get s :namespace))))
(defun kubed-ext-strimzi-scale-kafkaconnect-at-point (replicas)
  "Confirm and set selected KafkaConnect spec.replicas to REPLICAS."
  (interactive "nReplicas: ")
  (kubed-ext--validate-count replicas "Replicas")
  (let ((s (kubed-ext--action-selected)))
    (unless (equal (plist-get s :type) "kafkaconnects.kafka.strimzi.io") (user-error "Select a KafkaConnect"))
    (kubed-ext--strimzi-change (plist-get s :type) (plist-get s :name) (plist-get s :context) (plist-get s :namespace)
                              (list "patch" (plist-get s :type) (plist-get s :name) "--type=merge" "-p"
                                    (format "{\"spec\":{\"replicas\":%d}}" replicas)) "Scale KafkaConnect")))
(defun kubed-ext-strimzi-set-topic-partitions-at-point (partitions)
  "Increase selected KafkaTopic partition count to PARTITIONS after validation."
  (interactive "nPartitions: ")
  (kubed-ext--validate-count partitions "Partitions" t)
  (let* ((s (kubed-ext--action-selected)) (env (kubed-ext--capture-environment)) (origin (current-buffer)))
    (unless (equal (plist-get s :type) "kafkatopics.kafka.strimzi.io") (user-error "Select a KafkaTopic"))
    (kubed-ext--async-kubectl
     (append (list (kubed-ext--kubectl-program) "get" (plist-get s :type) (plist-get s :name)
                   "-o" "jsonpath={.spec.partitions}") (kubed-ext--action-scope-args s))
     (lambda (text)
       (unless (string-match-p "\\`[0-9]+\\'" (string-trim text)) (user-error "Topic partition count unavailable"))
       (when (< partitions (string-to-number text)) (user-error "Kafka partitions cannot be decreased"))
       (kubed-ext--defer-action-prompt
        env origin
        (lambda ()
          (kubed-ext--strimzi-change (plist-get s :type) (plist-get s :name) (plist-get s :context) (plist-get s :namespace)
                                    (list "patch" (plist-get s :type) (plist-get s :name) "--type=merge" "-p"
                                          (format "{\"spec\":{\"partitions\":%d}}" partitions)) "Set topic partitions"))))
     (lambda (error) (kubed-ext--show-action-output "topic partitions error" error t)))))

(defvar kubed-ext--action-prompt-timers nil)
(defun kubed-ext--defer-action-prompt (environment origin callback)
  "Defer CALLBACK until no minibuffer is active, restoring ENVIRONMENT at ORIGIN."
  (let (timer)
    (setq timer
          (run-at-time 0.1 nil
                       (lambda ()
                         (setq kubed-ext--action-prompt-timers (delq timer kubed-ext--action-prompt-timers))
                         (when (buffer-live-p origin)
                           (if (active-minibuffer-window)
                               (kubed-ext--defer-action-prompt environment origin callback)
                             (with-current-buffer origin
                               (kubed-ext--with-environment environment (funcall callback))))))))
    (when (timerp timer) (push timer kubed-ext--action-prompt-timers)) timer))
(defun kubed-ext--cancel-action-prompts ()
  "Cancel pending owned prompts during teardown."
  (mapc (lambda (timer) (when (timerp timer) (cancel-timer timer))) kubed-ext--action-prompt-timers)
  (setq kubed-ext--action-prompt-timers nil))

(defun kubed-ext--port-forward-argv (resource ports context namespace)
  "Build IPv6-first full forwarding argv for RESOURCE and PORTS."
  (append (list (kubed-ext--kubectl-program) "port-forward" resource ports
                (concat "--address=" kubed-ext-port-forward-address))
          (kubed-ext--action-scope-args (list :context context :namespace namespace))))
(defun kubed-ext--forward-update (entry text &optional terminal)
  "Update forwarding ENTRY from TEXT; TERMINAL marks process termination."
  (let* ((scan (concat (plist-get entry :pending) text))
         (state (cond (terminal (if (memq (plist-get entry :state) '(stopped error))
                                    (plist-get entry :state) 'exited))
                      ((string-match-p "\\(?:[Ee]rror\\|[Uu]nable\\|[Ff]ailed\\)" scan) 'error)
                      ((string-match-p "Forwarding from " scan) 'ready))))
    (unless (plist-member entry :pending) (nconc entry (list :pending nil)))
    (setf (plist-get entry :pending)
          (substring scan (max 0 (- (length scan) 1000))))
    (when state (setf (plist-get entry :state) state))
    (setf (plist-get entry :message) (truncate-string-to-width (string-trim text) 1000 nil nil "…"))
    (when (buffer-live-p (plist-get entry :buffer))
      (with-current-buffer (plist-get entry :buffer)
        (let ((inhibit-read-only t))
          (goto-char (point-max)) (insert text)
          (when (> (buffer-size) 20000) (delete-region (point-min) (- (point-max) 20000))))
        (setq header-line-format (format "Port forward: %s" (plist-get entry :state)))))
    (when terminal
      (setq kubed-ext--port-forwards (delq entry kubed-ext--port-forwards))
      (setf (plist-get entry :environment) nil))
    (kubed-ext-port-forwards-refresh)))
(defun kubed-ext-forward-port (type name local-port remote-port &optional context namespace)
  "Start a tracked TYPE/NAME forward from LOCAL-PORT to REMOTE-PORT."
  (interactive (let ((args (kubed-ext--action-interactive)))
                 (append (cl-subseq args 0 2)
                         (list (read-number "Local port: ") (read-number "Remote port: "))
                         (cl-subseq args 2))))
  (dolist (p (list local-port remote-port))
    (unless (and (integerp p) (<= 1 p 65535)) (user-error "Ports must be in 1..65535")))
  (kubed-ext--resolved context namespace 'default
    (kubed-ext--forward-port-resolved type name local-port remote-port context namespace)))
(defun kubed-ext--forward-port-resolved (type name local-port remote-port context namespace)
  "Launch an owned forward after explicit scope resolution."
  (let* ((s (kubed-ext--action-scope type name context namespace))
         (env (or kubed-ext--environment (kubed-ext--capture-environment)))
         (buffer (generate-new-buffer (format "*Kubed forward %s/%s [%s/%s] %d*" type name context namespace local-port)))
         (entry (append s (list :local-port local-port :remote-port remote-port
                                :address kubed-ext-port-forward-address :environment env
                                :buffer buffer :state 'starting :message "" :process nil))))
    (with-current-buffer buffer
      (special-mode)
      (kubed-ext--apply-environment-to-new-buffer env)
      (setq-local kubed-ext--action-scope s kubed-ext--action-environment env)
      (kubed-ext--own-buffer (lambda () (kubed-ext--stop-forward entry))))
    (push entry kubed-ext--port-forwards)
    (condition-case error
        (kubed-ext--with-environment env
          (setf (plist-get entry :process)
                (make-process :name (buffer-name buffer) :buffer nil :connection-type 'pipe
                              :file-handler t :noquery t
                              :command (kubed-ext--port-forward-argv (format "%s/%s" type name)
                                                                    (format "%d:%d" local-port remote-port) context namespace)
                              :filter (lambda (_process text) (kubed-ext--forward-update entry text))
                              :sentinel (lambda (process text)
                                          (when (memq (process-status process) '(exit signal failed))
                                            (when (and (not (eq (plist-get entry :state) 'stopped))
                                                       (not (zerop (process-exit-status process))))
                                              (setf (plist-get entry :state) 'error))
                                            (kubed-ext--forward-update entry text t))))))
      (error (kubed-ext--forward-update entry (error-message-string error))
             (setf (plist-get entry :state) 'error)))
    (kubed-ext-port-forwards-refresh)
    (plist-get entry :process)))
(defun kubed-ext--stop-forward (entry)
  "Stop owned forwarding ENTRY."
  (setf (plist-get entry :state) 'stopped)
  (when (process-live-p (plist-get entry :process)) (delete-process (plist-get entry :process)))
  (setq kubed-ext--port-forwards (delq entry kubed-ext--port-forwards))
  (setf (plist-get entry :environment) nil)
  (kubed-ext-port-forwards-refresh))
(defun kubed-ext-port-forwards-stop-all ()
  "Stop every extension-owned forward and pending action prompt."
  (interactive)
  (kubed-ext--cancel-action-prompts)
  (dolist (entry (copy-sequence kubed-ext--port-forwards))
    (kubed-ext--stop-forward entry)
    (when (buffer-live-p (plist-get entry :buffer)) (kill-buffer (plist-get entry :buffer))))
  (setq kubed-ext--port-forwards nil) (kubed-ext-port-forwards-refresh))
(defun kubed-ext-port-forwards-stop (click)
  "Stop the forwarding row at CLICK or point."
  (interactive (list last-nonmenu-event))
  (let ((entry (if (mouse-event-p click)
                   (let* ((pos (event-start click)) (w (posn-window pos)))
                     (with-current-buffer (window-buffer w) (tabulated-list-get-id (posn-point pos))))
                 (tabulated-list-get-id))))
    (unless entry (user-error "No forward selected")) (kubed-ext--stop-forward entry)))
(defun kubed-ext-port-forward-entries ()
  "Return tabulated rows for structured forwarding metadata."
  (mapcar (lambda (e)
            (list e (vector (format "%s/%s" (plist-get e :type) (plist-get e :name))
                            (or (plist-get e :context) "default") (or (plist-get e :namespace) "default")
                            (format "[%s]:%d → %d" (plist-get e :address) (plist-get e :local-port) (plist-get e :remote-port))
                            (symbol-name (plist-get e :state)) (plist-get e :message)))) kubed-ext--port-forwards))
(define-derived-mode kubed-ext-port-forwards-mode tabulated-list-mode "Kubed forwards"
  "Inspect extension-owned forwarding sessions."
  (setq tabulated-list-format [("Resource" 28 t) ("Context" 24 t) ("Namespace" 20 t)
                               ("Ports" 28 t) ("State" 12 t) ("Message" 50 nil)])
  (tabulated-list-init-header))
(keymap-set kubed-ext-port-forwards-mode-map "g" #'kubed-ext-port-forwards-refresh)
(keymap-set kubed-ext-port-forwards-mode-map "d" #'kubed-ext-port-forwards-stop)
(defun kubed-ext-port-forwards-refresh ()
  "Refresh an existing forwarding table without creating one."
  (interactive)
  (when-let* ((b (get-buffer "*Kubed port forwards*")))
    (with-current-buffer b (setq tabulated-list-entries (kubed-ext-port-forward-entries)) (tabulated-list-print t)))
  (kubed-ext-refresh-pf-markers-in-visible-buffers))
(defun kubed-ext-list-port-forwards ()
  "List tracked forwarding sessions and readiness/error states."
  (interactive)
  (let ((b (get-buffer-create "*Kubed port forwards*")))
    (with-current-buffer b (kubed-ext-port-forwards-mode))
    (kubed-ext-port-forwards-refresh) (pop-to-buffer b)))
(defvar-local kubed-ext--forward-overlays nil)
(defun kubed-ext-mark-port-forwards (&rest _)
  "Show live forwarding overlays without modifying batch-mark tags."
  (mapc #'delete-overlay kubed-ext--forward-overlays)
  (setq kubed-ext--forward-overlays nil)
  (when (derived-mode-p 'tabulated-list-mode)
    (let ((environment (kubed-ext--capture-environment)))
      (save-excursion
        (goto-char (point-min))
        (while (< (point) (point-max))
          (let ((s (kubed-ext--action-selected-scope)))
            (when (and s
                       (cl-some (lambda (e)
                                  (and (process-live-p (plist-get e :process))
                                       (equal (plist-get environment :eenv)
                                              (plist-get (plist-get e :environment) :eenv))
                                       (cl-every (lambda (k) (equal (plist-get s k) (plist-get e k)))
                                                 '(:type :name :context :namespace)))) kubed-ext--port-forwards))
              (let ((overlay (make-overlay (line-beginning-position) (line-beginning-position))))
                (overlay-put overlay 'before-string (propertize "F " 'face 'success))
                (push overlay kubed-ext--forward-overlays))))
          (forward-line 1))))))
(defun kubed-ext-refresh-pf-markers-in-visible-buffers ()
  "Refresh forwarding indicators only in visible resource buffers."
  (dolist (buffer (buffer-list))
    (when (get-buffer-window buffer t)
      (with-current-buffer buffer
        (when (or (derived-mode-p 'kubed-list-mode)
                  (derived-mode-p 'kubed-ext-resources-mode)
                  kubed-ext--forward-overlays)
          (kubed-ext-mark-port-forwards))))))
(defun kubed-ext--forward-selected (event)
  "Prompt for ports and forward the exact selected EVENT resource."
  (let ((s (kubed-ext--action-selected event)))
    (kubed-ext--at-scope s
      (kubed-ext-forward-port (plist-get s :type) (plist-get s :name)
                             (read-number "Local port: " 8080) (read-number "Remote port: " 80)
                             (plist-get s :context) (plist-get s :namespace)))))
(defun kubed-ext-pods-forward-port (click)
  "Forward the pod at CLICK."
  (interactive (list last-nonmenu-event)) (kubed-ext--forward-selected click))
(defalias 'kubed-ext-services-forward-port #'kubed-ext-pods-forward-port)
(defalias 'kubed-ext-deployments-forward-port #'kubed-ext-pods-forward-port)
(defun kubed-ext-forward-port-to-pod (pod local-port remote-port &optional context namespace)
  "Forward POD from LOCAL-PORT to REMOTE-PORT in CONTEXT/NAMESPACE."
  (interactive (list (read-string "Pod: ") (read-number "Local port: ") (read-number "Remote port: ")))
  (kubed-ext-forward-port "pod" pod local-port remote-port context namespace))
(defun kubed-ext-forward-port-to-service (service local-port remote-port &optional context namespace)
  "Forward SERVICE from LOCAL-PORT to REMOTE-PORT in CONTEXT/NAMESPACE."
  (interactive (list (read-string "Service: ") (read-number "Local port: ") (read-number "Remote port: ")))
  (kubed-ext-forward-port "service" service local-port remote-port context namespace))
(defun kubed-ext-forward-port-to-deployment (deployment local-port remote-port &optional context namespace)
  "Forward DEPLOYMENT from LOCAL-PORT to REMOTE-PORT in CONTEXT/NAMESPACE."
  (interactive (list (read-string "Deployment: ") (read-number "Local port: ") (read-number "Remote port: ")))
  (kubed-ext-forward-port "deployment" deployment local-port remote-port context namespace))

(defun kubed-ext-read-resource-port (type name prompt &optional context namespace)
  "Read and validate a port for TYPE/NAME under PROMPT in CONTEXT/NAMESPACE."
  (kubed-ext--action-scope type name context namespace)
  (let ((port (read-number (concat prompt ": "))))
    (unless (and (integerp port) (<= 1 port 65535)) (user-error "Port must be in 1..65535")) port))
(defun kubed-ext-read-service-port (service prompt &optional context namespace)
  "Read and validate SERVICE port under PROMPT in CONTEXT/NAMESPACE."
  (kubed-ext-read-resource-port "services" service prompt context namespace))
(defun kubed-ext-scale-kafkaconnect (name replicas &optional context namespace)
  "Confirm and scale KafkaConnect NAME to REPLICAS in CONTEXT/NAMESPACE."
  (interactive (let ((s (kubed-ext--action-selected)))
                 (list (plist-get s :name) (read-number "Replicas: ")
                       (plist-get s :context) (plist-get s :namespace))))
  (kubed-ext--validate-count replicas "Replicas")
  (kubed-ext--strimzi-change "kafkaconnects.kafka.strimzi.io" name context namespace
                            (list "patch" "kafkaconnects.kafka.strimzi.io" name "--type=merge" "-p"
                                  (format "{\"spec\":{\"replicas\":%d}}" replicas)) "Scale KafkaConnect"))

(define-derived-mode kubed-ext-rollout-history-mode special-mode "Kubed Rollout"
  "Captured workload history with revision, undo and refresh commands.")
(keymap-set kubed-ext-rollout-history-mode-map "g" #'kubed-ext-rollout-refresh)
(keymap-set kubed-ext-rollout-history-mode-map "r" #'kubed-ext-rollout-show-revision)
(keymap-set kubed-ext-rollout-history-mode-map "u" #'kubed-ext-rollout-undo-from-history)

(require 'transient)
(transient-define-prefix kubed-ext-copy-popup ()
  "Copy scoped resource data or reproducible commands."
  [["Resource"
    ("y" "YAML" kubed-ext-copy-resource-as-yaml)
    ("j" "JSON" kubed-ext-copy-resource-as-json)]
   ["Commands"
    ("p" "Scoped kubectl prefix" kubed-ext-list-copy-kubectl-prefix)
    ("l" "Logs command" kubed-ext-list-copy-log-command)
    ("c" "Last action" kubed-ext-list-copy-last-command)]])

(provide 'kubed-ext-actions)
;;; kubed-ext-actions.el ends here
