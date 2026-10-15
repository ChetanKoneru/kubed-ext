;;; kubed-ext-review-test.el --- Review correction regressions -*- lexical-binding: t; -*-
(require 'ert)
(require 'kubed-ext)
(require 'kubed-ext-core-test)

(ert-deftest kubed-ext-review-log-header-redacted ()
  "Headers must not serialize launch environments, even with custom locals."
  (with-temp-buffer
    (kubed-ext-log-mode)
    (setq kubed-ext--log-spec '(:type "pods" :query "p" :context "prod-east"
                              :environment (:process-environment ("TOKEN=never-display-me"))))
    (let ((header (eval (cadr header-line-format))))
      (should-not (string-match-p "never-display-me\\|process-environment" header))
      (should (string-match-p "prod-east" header)))))

(ert-deftest kubed-ext-review-log-nil-options-and-tail-zero ()
  "A legacy nil SINCE keeps Kubed's no-cutoff semantics, not Stern's 48h default."
  (let (fallback)
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "/usr/bin/stern"))
              ((symbol-function 'kubed-ext--call-original-kubed-logs)
               (lambda (&rest args) (setq fallback args))))
      (kubed-ext--stern-logs-resolved "pods" "p" "c" "n" nil t nil nil nil nil nil))
    (should (equal fallback '("pods" "p" "c" "n" nil t nil nil nil nil nil))))
  (should-error (kubed-ext--stern-args '(:type "pod" :query "p" :tail 0 :follow nil)) :type 'user-error)
  (should (member "--tail=0" (kubed-ext--stern-args '(:type "pod" :query "p" :tail 0 :follow t)))))

(ert-deftest kubed-ext-review-native-stern-default-has-explicit-since ()
  "Native Stern workflows explicitly set their bounded default duration."
  (should (member "--since=1h"
                  (kubed-ext--stern-args '(:type "pod" :query "p" :follow t)))))

(ert-deftest kubed-ext-review-mode-change-stops-real-log ()
  "A major-mode change cannot hide a live owned process from stop-all."
  (let ((b (generate-new-buffer " *review log*")) p (kubed-ext--log-buffers nil)
        (kubed-ext--owners (make-hash-table :test 'eq)))
    (unwind-protect
        (cl-letf (((symbol-function 'kubed-ext--stern-args)
                   (lambda (_) '("/bin/sh" "-c" "exec sleep 5"))))
          (with-current-buffer b
            (kubed-ext-log-mode)
            (setq kubed-ext--log-spec '(:type "pod" :query "p")
                  kubed-ext--log-environment (kubed-ext--capture-environment))
            (push b kubed-ext--log-buffers)
            (setq p (kubed-ext-restart-logs))
            (fundamental-mode)
            (kubed-ext-stop-all-logs)
            (should-not (process-live-p p))))
      (when (process-live-p p) (delete-process p))
      (when (buffer-live-p b) (kill-buffer b)))))

(ert-deftest kubed-ext-review-refresh-origin-header ()
  "Completion refresh is list-scoped, independent of the selected row."
  (with-temp-buffer
    (kubed-ext-resources-mode)
    (let* ((env (kubed-ext--capture-environment)) (calls 0))
      (setq kubed-ext--view-scope (list :type "pods" :context "c" :all-namespaces t :environment env))
      (cl-letf (((symbol-function 'kubed-ext-refresh-resources) (lambda () (cl-incf calls))))
        (kubed-ext--action-refresh-origin (current-buffer)
                                         '(:type "pods" :name "p" :context "c" :namespace "n") env)
        (should (= calls 1))))))

(ert-deftest kubed-ext-review-unknown-cluster-scope ()
  "A cold cluster-scoped table drops an irrelevant namespace."
  (with-temp-buffer
    (kubed-ext-resources-mode)
    (setq kubed-ext--view-scope '(:type "things.example.io" :context "c" :namespace "n" :scope-kind unknown))
    (kubed-ext--render-resource-response 0 '("NAME" "METADATA STATE") '(("thing" "two words")))
    (should-not (plist-get (kubed-ext-selected-scope) :namespace))
    (should-not (plist-get kubed-ext--view-scope :namespace))))

(ert-deftest kubed-ext-review-multiword-header ()
  "A multiword header and a multiword value each remain one cell."
  (should (equal (kubed-ext--parse-server-table "NAME   METADATA STATE   AGE\nx      two words        1d\n")
                 '(("NAME" "METADATA STATE" "AGE") ("x" "two words" "1d")))))

(ert-deftest kubed-ext-review-real-output-byte-limits ()
  "Actual stdout and stderr streams fail visibly and release resources."
  (let ((kubed-ext-request-byte-limit 512) (kubed-ext--requests nil)
        (kubed-ext--request-queue nil) (kubed-ext-request-timeout 2))
    (dolist (redirect '("" " >&2"))
      (let (failure success)
        (let ((r (kubed-ext--async-kubectl
                  (list "/bin/sh" "-c" (concat "printf '%2048s' x" redirect))
                  (lambda (_) (setq success t)) (lambda (s) (setq failure s)))))
          (kubed-ext-core-test-wait r)
          (should-not success)
          (should (string-match-p "byte limit" failure))
          (should-not (process-live-p (kubed-ext-request-process r)))
          (should-not (buffer-live-p (kubed-ext-request-stderr-buffer r))))))))

(ert-deftest kubed-ext-review-default-resolution-real-once ()
  "A real config fixture honors configured defaults once, not current context."
  (let* ((directory (make-temp-file "kubed-scope-" t))
         (fixture (expand-file-name "kubectl" directory))
         (counter (expand-file-name "calls" directory))
         (kubed-ext--requests nil) (kubed-ext--request-queue nil) result origin)
    (unwind-protect
        (progn
          (with-temp-file fixture
            (insert "#!/bin/sh\necho call >> " (shell-quote-argument counter)
                    "\nprintf 'current\\ncurrent\\tcurrent-ns\\nconfigured\\tconfig-ns\\n'\n"))
          (set-file-modes fixture #o700)
          (with-temp-buffer
            (setq origin (current-buffer))
            (setq-local kubed-ext--environment (plist-put (kubed-ext--capture-environment) :kubectl fixture))
            (setq-local kubed-kubectl-program fixture)
            (setq-local kubed--context-and-namespace-alist nil)
            (setq-local kubed-default-context-and-namespace-alist '((nil "configured" . "preferred")))
            (setq-local kubed-ext--environment (plist-put (kubed-ext--capture-environment) :kubectl fixture))
            (let ((request (kubed-ext--resolve-scope
                            '(:type "pods" :name "p") 'default
                            (lambda (scope)
                              (should (eq origin (current-buffer)))
                              (kubed-ext--resolve-scope scope 'default (lambda (s) (setq result s)))))))
              (kubed-ext-core-test-wait request))
            (should (equal (plist-get result :context) "configured"))
            (should (equal (plist-get result :namespace) "preferred")))
          (with-temp-buffer (insert-file-contents counter) (should (= (count-lines (point-min) (point-max)) 1))))
      (delete-directory directory t))))

(ert-deftest kubed-ext-review-native-public-marks ()
  "Public mark operations consume native D/* tags without private API stubs."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq-local kubed-list-type "pods" kubed-list-context "c" kubed-list-namespace "n")
    (setq tabulated-list-padding 2 tabulated-list-format '[("Name" 10 t)]
          tabulated-list-entries '(("a" ["a"]) ("b" ["b"])))
    (tabulated-list-print)
    (goto-char (point-min)) (tabulated-list-put-tag "D")
    (forward-line) (tabulated-list-put-tag "*")
    (should (equal (mapcar (lambda (s) (plist-get s :name)) (kubed-ext--action-marked-scopes)) '("a" "b")))
    (kubed-ext-unmark-all) (should-not (kubed-ext-marked-items))
    (kubed-ext-mark-all) (should (= (length (kubed-ext-marked-items)) 2))))

(ert-deftest kubed-ext-review-resource-output-environment ()
  "Output buffers retain executable, directory and explicit scope."
  (let ((before (buffer-list)) (env (kubed-ext--capture-environment)))
    (unwind-protect
        (kubed-ext--with-environment env
          (cl-letf (((symbol-function 'display-buffer) #'ignore)
                    ((symbol-function 'kubed-ext--async-kubectl)
                     (lambda (_argv success &optional _error) (funcall success "result"))))
            (kubed-ext--resource-output "pods" "p" "c" "n" "yaml" nil)
            (let ((b (cl-find-if (lambda (b) (not (memq b before))) (buffer-list))))
              (with-current-buffer b
                (should (equal kubed-ext--environment env))
                (should (equal (plist-get kubed-ext--action-scope :namespace) "n"))
                (should (local-variable-p 'process-environment))))))
      (dolist (b (buffer-list)) (unless (memq b before) (kill-buffer b))))))

(ert-deftest kubed-ext-review-node-aggregation-linear ()
  "Each pod budget is evaluated four times regardless of node count."
  (let* ((pod '((spec . ((nodeName . "node")
                        (containers . (((resources . ((requests . ((cpu . "1m") (memory . "1Mi")))
                                                        (limits . ((cpu . "2m") (memory . "2Mi"))))))))))))
         (pods (make-list 1000 pod)) (calls 0)
         (original (symbol-function 'kubed-ext--pod-budget-value)))
    (cl-letf (((symbol-function 'kubed-ext--pod-budget-value)
               (lambda (&rest args) (cl-incf calls) (apply original args))))
      (let ((table (kubed-ext--aggregate-node-budgets pods)))
        (should (= (gethash '("node" requests cpu) table) 1000))
        (should (= calls 4000))))))

(ert-deftest kubed-ext-review-real-terminal-mode-cleanup ()
  "Actual terminal ownership survives the loss of major-mode locals."
  (let ((b (generate-new-buffer " *review terminal*")) p
        (kubed-ext--terminal-processes (make-hash-table :test 'eq))
        (kubed-ext--owners (make-hash-table :test 'eq)))
    (unwind-protect
        (with-current-buffer b
          (setq p (make-process :name "review-terminal" :buffer b :noquery t
                                :command '("/bin/sh" "-c" "exec sleep 5")))
          (puthash b p kubed-ext--terminal-processes)
          (kubed-ext--own-buffer #'kubed-ext--terminal-stop)
          (fundamental-mode)
          (kubed-ext-terminals-stop-all)
          (should-not (process-live-p p))
          (should (= 0 (hash-table-count kubed-ext--terminal-processes))))
      (when (process-live-p p) (delete-process p))
      (when (buffer-live-p b) (kill-buffer b)))))

(ert-deftest kubed-ext-review-parser-bounded-batches ()
  "Refresh parser yields between bounded batches and reports truncation."
  (with-temp-buffer
    (kubed-ext-resources-mode)
    (let ((kubed-ext-resource-retain-limit 5) (kubed-ext-resource-parse-batch-size 2)
          jobs result (turns 0))
      (cl-letf (((symbol-function 'run-with-idle-timer)
                 (lambda (_delay _repeat fn &rest args) (push (cons fn args) jobs))))
        (kubed-ext--parse-table-async
         (concat "NAME   METADATA STATE\n"
                 (mapconcat (lambda (i) (format "p%-6dready now" i)) (number-sequence 1 20) "\n"))
         (current-buffer) 0 (lambda (headers rows) (setq result (cons headers rows))))
        (should-not result)
        (while jobs
          (cl-incf turns)
          (let ((job (pop jobs))) (apply (car job) (cdr job))))
        (should (= turns 3))
        (should (= (length (cdr result)) 5))
        (should kubed-ext--view-truncated)))))

(ert-deftest kubed-ext-review-native-refresh-dispatch ()
  "Native completion refresh uses Kubed update, not enhanced API or point."
  (with-temp-buffer
    (kubed-list-mode)
    (let ((calls 0))
      (cl-letf (((symbol-function 'kubed-list-update) (lambda (&rest _) (cl-incf calls)))
                ((symbol-function 'kubed-ext-refresh-resources) (lambda () (ert-fail "Wrong dispatch"))))
        (kubed-ext--action-refresh) (should (= calls 1))))))

(ert-deftest kubed-ext-review-metric-dataset-bound ()
  "Oversized resource/usage responses fail instead of retaining unbounded data."
  (let ((kubed-ext-metric-item-limit 1))
    (should-error (kubed-ext--metric-items "{\"items\":[{},{}]}"))
    (should-error (kubed-ext--top-parse "a 1m 1Mi\nb 2m 2Mi\n" '(:type "pods" :namespace "n")))))

(ert-deftest kubed-ext-review-alias-and-cold-namespace ()
  "Builtin aliases and cold-cache namespaced tables preserve actual scope."
  (dolist (alias '("no" "nodes.v1" "pv" "crds.apiextensions.k8s.io" "sc"))
    (should-not (kubed-ext--resource-namespaced-p alias "c")))
  (dolist (alias '("po" "hpa" "netpol" "rc" "svc"))
    (should (eq t (kubed-ext--resource-namespaced-p alias "c"))))
  (with-temp-buffer
    (kubed-ext-resources-mode)
    (setq kubed-ext--view-scope '(:type "widgets.example.io" :context "c" :namespace "n" :scope-kind unknown))
    (should (member "-A" (kubed-ext--resource-list-argv kubed-ext--view-scope)))
    (kubed-ext--render-resource-response 0 '("NAMESPACE" "NAME") '(("other" "p") ("n" "p")))
    (should (= (length tabulated-list-entries) 1))
    (should (equal (plist-get (kubed-ext-selected-scope) :namespace) "n"))))

(ert-deftest kubed-ext-review-eshell-tramp-no-sync-discovery ()
  "Eshell starts in resolved Kubed TRAMP directory, never an exec subprocess."
  (let ((env (kubed-ext--capture-environment)) (b (generate-new-buffer " *review eshell*")) directory)
    (unwind-protect
        (cl-letf (((symbol-function 'eshell)
                   (lambda (&rest _) (setq directory default-directory) b))
                  ((symbol-function 'kubed-read-container)
                   (lambda (&rest _) (ert-fail "Synchronous container discovery")))
                  ((symbol-function 'eshell-send-input)
                   (lambda (&rest _) (ert-fail "Interactive exec sent to eshell"))))
          (kubed-ext--terminal-launch 'eshell "p" "main" "c" "n" env)
          (should (string-match-p "c%n%p%main:/" directory))
          (with-current-buffer b (should (equal kubed-ext--environment env))))
      (when (buffer-live-p b) (kill-buffer b)))))

(ert-deftest kubed-ext-review-real-upstream-fallback-owned ()
  "Actual upstream fallback gets explicit scope, no synchronous default helpers."
  (let* ((dir (make-temp-file "kubed-fallback-" t)) (fixture (expand-file-name "kubectl" dir))
         (before (buffer-list)) (kubed-ext--log-buffers nil)
         (kubed-ext--owners (make-hash-table :test 'eq)) p)
    (unwind-protect
        (progn
          (with-temp-file fixture (insert "#!/bin/sh\nprintf 'fallback\\n'\nexec sleep 5\n"))
          (set-file-modes fixture #o700)
          (let ((env (plist-put (kubed-ext--capture-environment) :kubectl fixture)))
            (kubed-ext--with-environment env
              (cl-letf (((symbol-function 'kubed-local-context) (lambda () (ert-fail "Sync context")))
                        ((symbol-function 'kubed--namespace) (lambda (&rest _) (ert-fail "Sync namespace")))
                        ((symbol-function 'display-buffer) #'ignore))
                (kubed-ext-stern-logs "pods" "p" "c" "n" "main" t 100 nil "2026-01-01T00:00:00Z" nil nil)))
            (should (= (length kubed-ext--log-buffers) 1))
            (with-current-buffer (car kubed-ext--log-buffers)
              (setq p kubed-ext--log-process)
              (should (equal kubed-ext--environment env))
              (should (member "--since-time" (process-command p)))
              (should-not (member "--tail" (process-command p)))
              (fundamental-mode)
              (should-not (process-live-p p)))))
      (when (process-live-p p) (delete-process p))
      (dolist (b (buffer-list)) (unless (memq b before) (kill-buffer b)))
      (delete-directory dir t))))

(ert-deftest kubed-ext-review-mouse-origin-environment ()
  "An adapter launched from another selected buffer uses the event row origin."
  (let ((origin (generate-new-buffer " *review mouse origin*")) scope observed)
    (unwind-protect
        (progn
          (with-current-buffer origin
            (setq-local default-directory "/tmp/")
            (setq-local process-environment '("REVIEW_ORIGIN=yes"))
            (setq scope (list :type "pods" :name "p" :context "c" :namespace "n"
                              :origin origin :environment (kubed-ext--capture-environment))))
          (with-temp-buffer
            (cl-letf (((symbol-function 'kubed-ext--action-selected) (lambda (&rest _) scope))
                      ((symbol-function 'kubed-ext--terminal-pod)
                       (lambda (&rest _) (setq observed (list (current-buffer) default-directory process-environment)))))
              (kubed-ext--terminal-at-event 'vterm nil)))
          (should (eq (car observed) origin))
          (should (equal (cadr observed) "/tmp/"))
          (should (equal (caddr observed) '("REVIEW_ORIGIN=yes"))))
      (kill-buffer origin))))

(ert-deftest kubed-ext-review-native-nil-origin-refresh ()
  "Resolved native row actions refresh the unchanged nil-default origin list."
  (with-temp-buffer
    (kubed-list-mode)
    (setq-local kubed-list-type "pods" kubed-list-context nil kubed-list-namespace nil)
    (let* ((list-scope (kubed-ext--current-scope))
           (env (plist-get list-scope :environment)) (calls 0))
      (cl-letf (((symbol-function 'kubed-list-update) (lambda (&rest _) (cl-incf calls))))
        (kubed-ext--action-refresh-origin (current-buffer)
                                         '(:type "pods" :name "p" :context "resolved" :namespace "default")
                                         env list-scope)
        (should (= calls 1))
        (setq-local kubed-list-context "changed")
        (kubed-ext--action-refresh-origin (current-buffer)
                                         '(:type "pods" :name "p" :context "resolved" :namespace "default")
                                         env list-scope)
        (should (= calls 1))))))

(ert-deftest kubed-ext-review-real-metric-mode-change ()
  "Mode change cancels actual pending finite metric processes."
  (let ((b (generate-new-buffer " *review metric*")) r p
        (kubed-ext--requests nil) (kubed-ext--request-queue nil)
        (kubed-ext--owners (make-hash-table :test 'eq)))
    (unwind-protect
        (with-current-buffer b
          (kubed-ext-top-mode)
          (setq r (kubed-ext--async-kubectl '("/bin/sh" "-c" "exec sleep 5")
                                          (lambda (_) (ert-fail "Cancelled response"))))
          (setq p (kubed-ext-request-process r) kubed-ext--top-requests (list r))
          (fundamental-mode)
          (should (kubed-ext-request-finished-p r))
          (should-not (process-live-p p))
          (should-not kubed-ext--requests))
      (when (process-live-p p) (delete-process p))
      (when (buffer-live-p b) (kill-buffer b)))))

(provide 'kubed-ext-review-test)
;;; kubed-ext-review-test.el ends here
