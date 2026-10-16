;;; kubed-ext-test.el --- Entry integration regressions -*- lexical-binding: t; -*-
;;; Commentary:
;; Module tests remain in their dedicated files.  These tests exercise the
;; source entry point, real module interfaces and reversible integration.
;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'kubed-ext "kubed-ext.el")

(defmacro kubed-ext-test--isolated (&rest body)
  "Run BODY with private integration state and no live-session shutdown."
  (declare (indent 0) (debug t))
  `(let ((kubed-ext--installed-bindings nil)
         (kubed-ext--command-history nil))
     (cl-letf (((symbol-function 'kubed-ext--cancel-all-requests) #'ignore)
               ((symbol-function 'kubed-ext-stop-all-logs) #'ignore)
               ((symbol-function 'kubed-ext-terminals-stop-all) #'ignore)
               ((symbol-function 'kubed-ext-port-forwards-stop-all) #'ignore)
               ((symbol-function 'kubed-ext-resources-stop-all) #'ignore)
               ((symbol-function 'kubed-ext--cancel-action-prompts) #'ignore))
       ,@body)))

(ert-deftest kubed-ext-entry-module-definitions-survive-load ()
  "The entry point must not replace resource and workflow implementations."
  (dolist (pair '((kubed-ext-switch-resource . "kubed-ext-resources.el")
                  (kubed-ext-switch-context . "kubed-ext-resources.el")
                  (kubed-ext-switch-namespace . "kubed-ext-resources.el")
                  (kubed-ext-discover-crds-async . "kubed-ext-resources.el")
                  (kubed-ext-mark-item . "kubed-ext-resources.el")
                  (kubed-ext-unmark-item . "kubed-ext-resources.el")
                  (kubed-ext-mark-all . "kubed-ext-resources.el")
                  (kubed-ext-set-filter . "kubed-ext-resources.el")
                  (kubed-ext-copy-popup . "kubed-ext-actions.el")))
    (should (equal (file-name-nondirectory (symbol-file (car pair) 'defun))
                   (cdr pair)))))

(ert-deftest kubed-ext-entry-binding-restore-is-command-specific ()
  "Any installed command restores, but subsequent user edits survive."
  (kubed-ext-test--isolated
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "x") #'ignore)
      (kubed-ext--install-binding map "x" #'kubed-ext-switch-resource)
      (kubed-ext--install-binding map "x" #'kubed-ext-switch-resource)
      (should (= 1 (length kubed-ext--installed-bindings)))
      (kubed-ext-disable)
      (should (eq (lookup-key map (kbd "x")) #'ignore))
      (kubed-ext--install-binding map "x" #'kubed-ext-switch-resource)
      (define-key map (kbd "x") #'forward-char)
      (kubed-ext--install-binding map "x" #'kubed-ext-switch-resource)
      (should (eq (lookup-key map (kbd "x")) #'forward-char))
      (kubed-ext-disable)
      (should (eq (lookup-key map (kbd "x")) #'forward-char)))))

(ert-deftest kubed-ext-entry-disable-calls-all-owners ()
  "Disable invokes every owner, including metrics, without unloading modules."
  (let ((kubed-ext--installed-bindings nil) calls)
    (cl-letf (((symbol-function 'kubed-ext--cancel-all-requests) (lambda () (push 'requests calls)))
              ((symbol-function 'kubed-ext-stop-all-logs) (lambda () (push 'logs calls)))
              ((symbol-function 'kubed-ext-terminals-stop-all) (lambda () (push 'terminals calls)))
              ((symbol-function 'kubed-ext-port-forwards-stop-all) (lambda () (push 'forwards calls)))
              ((symbol-function 'kubed-ext-resources-stop-all) (lambda () (push 'resources calls)))
              ((symbol-function 'kubed-ext--cancel-action-prompts) #'ignore)
              ((symbol-function 'unload-feature) (lambda (&rest _) (ert-fail "Dependency unloaded"))))
      (with-temp-buffer
        (kubed-ext-top-mode)
        (let ((generation kubed-ext--top-generation))
          (kubed-ext-disable)
          (should (> kubed-ext--top-generation generation))))
      (should (equal (sort calls (lambda (a b) (string< (symbol-name a) (symbol-name b))))
                     '(forwards logs requests resources terminals))))))

(ert-deftest kubed-ext-entry-refresh-dispatch ()
  "Refresh dispatch follows enhanced, native, metrics and log buffer modes."
  (let (called)
    (cl-letf (((symbol-function 'kubed-ext-refresh-resources) (lambda () (setq called 'resources)))
              ((symbol-function 'kubed-list-update) (lambda () (interactive) (setq called 'native)))
              ((symbol-function 'kubed-ext-top-refresh) (lambda () (setq called 'metrics)))
              ((symbol-function 'kubed-ext-restart-logs) (lambda () (setq called 'logs))))
      (dolist (pair '((kubed-ext-resources-mode . resources) (kubed-list-mode . native)
                      (kubed-ext-top-mode . metrics) (kubed-ext-log-mode . logs)))
        (with-temp-buffer
          ;; Native Kubed mode startup can contact a cluster; test dispatch only.
          (setq major-mode (car pair))
          (kubed-ext-refresh)
          (should (eq called (cdr pair)))))
      (with-temp-buffer (should-error (kubed-ext-refresh) :type 'user-error)))))

(ert-deftest kubed-ext-entry-navigation-retains-resource-scope ()
  "Module context/namespace commands retain workload, selector and context."
  (with-temp-buffer
    (kubed-ext-resources-mode)
    (setq kubed-ext--view-scope '(:type "deployments" :context "old" :namespace "ns" :selector "app=api"))
    (let (args prompts)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt &rest _) (push prompt prompts) "new"))
                ;; Completion candidates arrive asynchronously; prompting must
                ;; never wait for them.
                ((symbol-function 'kubed-ext--async-kubectl)
                 (lambda (&rest _) (ert-fail "Blocking discovery during prompt")))
                ((symbol-function 'kubed-ext-list-resources) (lambda (&rest a) (setq args a))))
        (call-interactively #'kubed-ext-switch-context)
        (should (equal args '("deployments" "new" "ns" "app=api")))
        (call-interactively #'kubed-ext-switch-namespace)
        (should (equal args '("deployments" "old" "new" "app=api")))
        ;; Both prompts offer the current value as the minibuffer default.
        (should (= (length prompts) 2))))))

(ert-deftest kubed-ext-entry-selected-workload-logs-use-true-signature ()
  "Menu logs call Stern with type/name/context/namespace, not hardcoded pods."
  (let (args)
    (cl-letf (((symbol-function 'kubed-ext--action-selected)
               (lambda (&rest _) '(:type "deployments" :name "api" :context "ctx" :namespace "team")))
              ((symbol-function 'kubed-ext-stern-logs) (lambda (&rest a) (setq args a))))
      (call-interactively #'kubed-ext-selected-logs)
      (should (equal args '("deployments" "api" "ctx" "team" nil t))))))

(ert-deftest kubed-ext-entry-metrics-without-selected-row-retain-list-scope ()
  "Metrics on a list header retain its context/namespace."
  (with-temp-buffer
    (kubed-ext-resources-mode)
    (setq kubed-ext--view-scope '(:type "deployments" :context "ctx" :namespace "team"))
    (let (args)
      (cl-letf (((symbol-function 'kubed-ext-top-pods) (lambda (&rest a) (setq args a))))
        (call-interactively #'kubed-ext-view-top-pods)
        (should (equal args '("ctx" "team")))))))

(ert-deftest kubed-ext-entry-public-output-format-validation ()
  "Output format supports original interactive command and rejects bad values."
  (let ((kubed-ext-output-format "yaml"))
    (kubed-ext-toggle-output-format)
    (should (equal kubed-ext-output-format "json"))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "yaml")))
      (call-interactively #'kubed-ext-set-output-format))
    (should (equal kubed-ext-output-format "yaml"))
    (should-error (kubed-ext-set-output-format "xml") :type 'user-error)))

(ert-deftest kubed-ext-entry-native-keys-stay-native ()
  "Setup is idempotent, makes no requests and preserves native refresh/actions."
  (kubed-ext-test--isolated
    (let ((kubed-list-mode-map (make-sparse-keymap))
          (kubed-ext-resources-mode-map (make-sparse-keymap))
          (kubed-ext-log-mode-map (make-sparse-keymap))
          (kubed-ext-top-mode-map (make-sparse-keymap)))
      (define-key kubed-list-mode-map (kbd "g") #'kubed-list-update)
      (define-key kubed-list-mode-map (kbd "D") #'kubed-list-delete)
      (cl-letf (((symbol-function 'kubed-ext--async-kubectl)
                 (lambda (&rest _) (ert-fail "Setup contacted cluster"))))
        (kubed-ext-setup)
        (let ((count (length kubed-ext--installed-bindings)))
          (kubed-ext-setup)
          (should (= count (length kubed-ext--installed-bindings))))
        (should (eq (lookup-key kubed-list-mode-map (kbd "g")) #'kubed-list-update))
        (should (eq (lookup-key kubed-list-mode-map (kbd "D")) #'kubed-list-delete))
        (should (eq (lookup-key kubed-list-mode-map (kbd "?")) #'kubed-ext-transient-menu))
        (should (eq (lookup-key kubed-ext-resources-mode-map (kbd "l")) #'kubed-ext-selected-logs))
        (kubed-ext-disable)))))

(ert-deftest kubed-ext-entry-unmark-does-not-toggle ()
  "Unmarking an unmarked row does not mark it through an obsolete adapter."
  (with-temp-buffer
    (kubed-ext-resources-mode)
    (setq kubed-ext--view-scope '(:type "pods" :context "ctx" :namespace "ns"))
    (kubed-ext--render-resource-response kubed-ext--view-generation '("NAME") '(("api")))
    (goto-char (point-min))
    (kubed-ext-unmark-item)
    (should-not (kubed-ext-marked-items))))

(ert-deftest kubed-ext-entry-reload-keeps-module-functions ()
  "Reloading the source entry point cannot replace already-loaded modules."
  (kubed-ext-test--isolated
    (let* ((functions '(kubed-ext-switch-resource kubed-ext-discover-crds-async
                       kubed-ext-unmark-item kubed-ext-copy-popup))
           (before (mapcar #'symbol-function functions))
           (kubed-list-mode-map (make-sparse-keymap))
           (kubed-ext-resources-mode-map (make-sparse-keymap))
           (kubed-ext-log-mode-map (make-sparse-keymap))
           (kubed-ext-top-mode-map (make-sparse-keymap)))
      (unwind-protect
          (progn
            (load (expand-file-name "kubed-ext.el"
                                    (file-name-directory (locate-library "kubed-ext.el")))
                  nil t t)
            (should (cl-every #'eq before (mapcar #'symbol-function functions))))
        (kubed-ext-disable)))))

(ert-deftest kubed-ext-entry-predicates-reflect-row-and-mode ()
  "Native list headers and unrelated buffers do not enable selected actions."
  (with-temp-buffer
    (should-not (kubed-ext--refreshable-p))
    (should-not (kubed-ext--row-selected-p)))
  (with-temp-buffer
    (setq major-mode 'kubed-list-mode)
    (should (kubed-ext--refreshable-p))
    (should-not (kubed-ext--enhanced-view-p))
    (should-not (kubed-ext--row-selected-p)))
  (with-temp-buffer
    (kubed-ext-resources-mode)
    (setq kubed-ext--view-scope '(:type "deployments" :context "ctx" :namespace "ns"))
    (kubed-ext--render-resource-response kubed-ext--view-generation '("NAME") '(("api")))
    (goto-char (point-min))
    (should (kubed-ext--workload-selected-p))
    (should (kubed-ext--logs-selected-p))
    (should (kubed-ext--forward-selected-p))
    (should-not (kubed-ext--pod-selected-p))))

(ert-deftest kubed-ext-entry-history-is-bounded-and-reversible ()
  "History records actual action argv and removal preserves other advice."
  (kubed-ext-test--isolated
    (let ((kubed-ext--last-action-command '("kubectl" "describe" "pods" "api name")))
      (dotimes (_ 110) (kubed-ext--record-action-command))
      (should (= 100 (length kubed-ext--command-history)))
      (should (equal (car kubed-ext--command-history) kubed-ext--last-action-command))
      (should-not (eq (car kubed-ext--command-history) kubed-ext--last-action-command))
      (advice-add 'kubed-ext--action-run :after #'kubed-ext--record-action-command)
      (kubed-ext-disable)
      (should-not (advice-member-p #'kubed-ext--record-action-command 'kubed-ext--action-run)))))

(ert-deftest kubed-ext-entry-jump-and-jab-use-exact-scope ()
  "Legacy convenience commands delegate without manufacturing pod targets."
  (with-temp-buffer
    (kubed-ext-resources-mode)
    (setq kubed-ext--view-scope '(:type "jobs" :context "ctx" :namespace "team" :selector "app=x"))
    (let (args)
      (cl-letf (((symbol-function 'kubed-ext-list-resources) (lambda (&rest a) (setq args a))))
        (call-interactively #'kubed-ext-jump-services)
        (should (equal args '("services" "ctx" "team" "app=x"))))
      (cl-letf (((symbol-function 'kubed-ext-rollout-restart) (lambda (&rest a) (setq args a))))
        (kubed-ext-jab-deployment "api" "ctx" "team")
        (should (equal args '("deployments" "api" "ctx" "team")))))))

(ert-deftest kubed-ext-entry-unload-keeps-dependencies-and-user-advice ()
  "The unload hook restores integration but leaves dependency features loaded."
  (kubed-ext-test--isolated
    (let ((advice (lambda (&rest _) nil)))
      (unwind-protect
          (progn
            (advice-add 'kubed-ext--action-run :after advice)
            (should-not (kubed-ext-unload-function))
            (should (advice-member-p advice 'kubed-ext--action-run))
            (dolist (feature '(kubed kubed-ext-core kubed-ext-resources
                              kubed-ext-actions kubed-ext-logs
                              kubed-ext-metrics kubed-ext-terminals))
              (should (featurep feature))))
        (advice-remove 'kubed-ext--action-run advice)))))

(defun kubed-ext-test--menu-entries ()
  "Return (KEY DESCRIPTION COMMAND) for every suffix of the transient menu."
  (let (entries)
    (dolist (group (append (get 'kubed-ext-transient-menu 'transient--layout) nil))
      (dolist (column (append (if (vectorp group) (aref group 3) nil) nil))
        (dolist (suffix (append (if (vectorp column) (aref column 3) nil) nil))
          (let ((plist (and (vectorp suffix) (aref suffix 2))))
            (when (plist-get plist :key)
              (push (list (plist-get plist :key)
                          (plist-get plist :description)
                          (plist-get plist :command))
                    entries))))))
    (nreverse entries)))

(ert-deftest kubed-ext-entry-menu-keys-are-unique ()
  "No two menu entries may claim the same key."
  (let* ((keys (mapcar #'car (kubed-ext-test--menu-entries)))
         (duplicates (cl-remove-if-not (lambda (k) (> (cl-count k keys :test #'equal) 1))
                                       (delete-dups (copy-sequence keys)))))
    (should (equal duplicates nil))))

(ert-deftest kubed-ext-entry-menu-commands-have-direct-keys ()
  "Every row or table command in the menu is reachable without the menu."
  (kubed-ext-setup)
  (let ((maps (list kubed-ext-resources-mode-map kubed-ext-pods-mode-map
                    kubed-ext-log-mode-map))
        ;; Menu-only entries: closing the menu, disabling the package, and the
        ;; refresh dispatcher, which each table binds to its own `g'.
        (exempt '(transient-quit-one kubed-ext-disable kubed-ext-refresh))
        missing)
    (pcase-dolist (`(,_key ,_description ,command) (kubed-ext-test--menu-entries))
      (when (and command (not (memq command exempt))
                 (not (seq-some (lambda (map) (where-is-internal command map t)) maps)))
        (push command missing)))
    (should (equal missing nil))))

(provide 'kubed-ext-test)
;;; kubed-ext-test.el ends here
