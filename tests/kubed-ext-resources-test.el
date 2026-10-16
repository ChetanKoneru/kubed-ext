;;; kubed-ext-resources-test.el --- Resource regressions -*- lexical-binding: t; -*-
;;; Code:
(require 'ert)
(require 'kubed-ext-resources)
(ert-deftest kubed-ext-resources-table-boundaries ()
  "Preserve Unicode, interior blanks and multiword server cells."
  (should (equal (kubed-ext--parse-server-table
                  "NAME   STATE   MESSAGE\npod    Ready   two words\nλ      \t       final words\n")
                 '(("NAME" "STATE" "MESSAGE")
                   ("pod" "Ready" "two words") ("λ" "" "final words"))))
  (should-not (kubed-ext--parse-server-table "No resources found in x namespace.\n")))
(ert-deftest kubed-ext-resources-all-namespace-identity ()
  "Same names in two namespaces remain distinct and selected scope is exact."
  (with-temp-buffer
    (kubed-ext-resources-mode)
    (setq kubed-ext--view-scope '(:type "pods" :context "c" :all-namespaces t))
    (kubed-ext--render-resource-response 0 '("NAMESPACE" "NAME" "STATUS")
                                         '(("a" "same" "Running") ("b" "same" "Pending")))
    (should (equal (mapcar #'car tabulated-list-entries) '("a/same" "b/same")))
    (goto-char (point-min))
    (let ((scope (kubed-ext-selected-scope)))
      (should (equal (plist-get scope :name) "same"))
      (should (equal (plist-get scope :namespace) "a"))
      (should-not (plist-get scope :all-namespaces)))
    (should (member "-A" (kubed-ext--resource-list-argv kubed-ext--view-scope)))))

(ert-deftest kubed-ext-resources-cold-namespaced-identity-survives-warm-refresh ()
  "A cold CRD's namespaced row ID remains stable after metadata settles."
  (with-temp-buffer
    (kubed-ext-resources-mode)
    (setq kubed-ext--view-scope '(:type "widgets.example.io" :context "c" :namespace "n"
                                  :scope-kind unknown))
    (kubed-ext--render-resource-response 0 '("NAMESPACE" "NAME") '(("n" "widget")))
    (should (equal (caar tabulated-list-entries) "n/widget"))
    (goto-char (point-min))
    (kubed-ext-mark-item)
    (setq kubed-ext--view-scope '(:type "widgets.example.io" :context "c" :namespace "n"
                                  :scope-kind t))
    (kubed-ext--render-resource-response 0 '("NAME") '(("widget")))
    (should (equal (caar tabulated-list-entries) "n/widget"))
    (goto-char (point-min))
    (should (gethash "n/widget" kubed-ext--view-marks))
    (let ((scope (kubed-ext-selected-scope)))
      (should (equal (plist-get scope :name) "widget"))
      (should (equal (plist-get scope :namespace) "n")))))
(ert-deftest kubed-ext-resources-stale-limit-marks ()
  "Ignore stale replies, bound rows, and retain marks while filtering."
  (with-temp-buffer
    (kubed-ext-resources-mode)
    (setq kubed-ext--view-scope '(:type "pods" :namespace "n")
          kubed-ext--view-generation 2)
    (should-not (kubed-ext--render-resource-response 1 '("NAME") '(("stale"))))
    (let ((kubed-ext-resource-display-limit 1))
      (kubed-ext--render-resource-response 2 '("NAME") '(("a") ("b")))
      (should (= (length tabulated-list-entries) 1))
      (goto-char (point-min))
      (kubed-ext-mark-item)
      (should (equal (kubed-ext-marked-items) '("a")))
      (kubed-ext-set-filter "b")
      (should (equal (caar tabulated-list-entries) "b"))
      (kubed-ext-set-filter "")
      (kubed-ext-resource-show-more)
      (should (= (length tabulated-list-entries) 2))
      (should (equal (kubed-ext-marked-items) '("a"))))))

(ert-deftest kubed-ext-resources-cold-namespace-retains-target-after-unrelated-rows ()
  "Cold namespaced CRDs filter before the retain cap, not after it."
  (with-temp-buffer
    (kubed-ext-resources-mode)
    (setq kubed-ext--view-scope '(:type "widgets.example.io" :context "c" :namespace "z"
                                  :scope-kind unknown))
    (let ((kubed-ext-resource-retain-limit 5)
          (kubed-ext-resource-parse-batch-size 6000)
          jobs result)
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat fn &rest args) (push (cons fn args) jobs))))
        (kubed-ext--parse-table-async
         (concat "NAMESPACE   NAME\n"
                 (mapconcat (lambda (i) (format "other       p%-10d" i))
                            (number-sequence 1 5001) "\n")
                 "\nz           target\n")
         (current-buffer) 0 (lambda (headers rows) (setq result (cons headers rows))))
        (let ((job (pop jobs))) (apply (car job) (cdr job)))
        (should (equal result '(("NAMESPACE" "NAME") ("z" "target"))))
        (should-not kubed-ext--view-truncated)))))
(ert-deftest kubed-ext-resources-native-scope ()
  "Adapt native Kubed lists without guessing from namespace columns."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq-local kubed-list-type "pods")
    (setq-local kubed-list-context "native")
    (setq-local kubed-list-namespace "ns")
    (setq tabulated-list-format [("Name" 10 t)]
          tabulated-list-entries '(("pod" ["pod"])))
    (tabulated-list-print)
    (goto-char (point-min))
    (should (equal (plist-get (kubed-ext-selected-scope) :name) "pod"))
    (should (equal (plist-get (kubed-ext-selected-scope) :namespace) "ns"))))
(ert-deftest kubed-ext-resources-crd-validation ()
  "Compact metadata validation rejects unsafe and malformed resource definitions."
  (let ((kubed-ext--crd-registry (make-hash-table :test 'equal)))
    (should (kubed-ext-register-crd '(:type "widgets.example.io" :plural "widgets"
                                     :group "example.io" :kind "Widget" :namespaced t)))
    (should-not (kubed-ext-register-crd '(:type "bad;command" :namespaced t)))
    (should-not (kubed-ext-register-crd '(:type "bad.io" :namespaced bogus)))
    (should-error (kubed-ext--parse-crd-compact-list "bad\trow"))))
(ert-deftest kubed-ext-resources-crd-cache-roundtrip ()
  "JSON caches round-trip scope, isolate environments, reject malformed entries."
  (let* ((kubed-ext-crd-cache-directory (make-temp-file "kubed-crd-test-" t))
         (kubed-ext--crd-registry (make-hash-table :test 'equal))
         (environment (kubed-ext--capture-environment))
         (entries (kubed-ext--parse-crd-compact-list
                   "widgets\texample.io\tWidget\tNamespaced\nthings\texample.io\tThing\tCluster\n")))
    (unwind-protect
        (progn
          (kubed-ext--write-crd-cache "a" environment entries)
          (should (= (kubed-ext--load-crd-cache "a" environment) 2))
          (should (kubed-ext-crd-metadata "things.example.io" "a" environment))
          (should-not (plist-get (kubed-ext-crd-metadata "things.example.io" "a" environment) :namespaced))
          (should-not (kubed-ext-crd-metadata "things.example.io" "b" environment))
          (let ((other (copy-tree environment t)))
            (plist-put other :eenv '(other))
            (should-not (kubed-ext-crd-metadata "things.example.io" "a" other)))
          (let ((file (kubed-ext--crd-cache-file "a" environment)))
            (with-temp-file file (insert "{\"version\":1,\"entries\":[{\"type\":\"bad\"}]}"))
            (should-not (kubed-ext--load-crd-cache "a" environment))
            (should (kubed-ext-crd-metadata "things.example.io" "a" environment))))
      (delete-directory kubed-ext-crd-cache-directory t))))
(ert-deftest kubed-ext-resources-discovery-stale-rbac-and-disk-failure ()
  "Stale discovery cannot replace newer metadata; disk/RBAC failures preserve live state."
  (let ((kubed-ext--crd-registry (make-hash-table :test 'equal))
        (kubed-ext--crd-discovery-state (make-hash-table :test 'equal))
        (kubed-ext-crd-discovery-enabled t) callbacks failures count)
    (cl-letf (((symbol-function 'kubed-ext--async-kubectl)
               (lambda (argv success &optional failure)
                 (should (member kubed-ext--crd-discovery-jsonpath argv))
                 (should-not (member "json" argv))
                 (push success callbacks) (push failure failures)
                 (kubed-ext--make-request :finished t)))
              ((symbol-function 'kubed-ext--write-crd-cache) #'ignore)
              ((symbol-function 'kubed-ext--load-crd-cache) #'ignore))
      (kubed-ext-discover-crds-async "c" t)
      (kubed-ext-discover-crds-async "c" t (lambda (number) (setq count number)))
      (funcall (car callbacks) "new\texample.io\tNew\tCluster\n")
      (funcall (cadr callbacks) "old\texample.io\tOld\tNamespaced\n")
      (should (= count 1))
      (should (equal (kubed-ext-crd-resource-choices "c") '("new.example.io")))
      (kubed-ext-discover-crds-async "c" t)
      (funcall (car failures) "Forbidden")
      (should (equal (kubed-ext-crd-resource-choices "c") '("new.example.io")))
      (should (equal (nth 2 (kubed-ext--resource-list-argv '(:type "explicit.example.io" :context "c")))
                     "explicit.example.io")))))
(ert-deftest kubed-ext-resources-discovery-real-subprocess ()
  "Read-only compact discovery succeeds through a real local fixture process."
  (let* ((kubed-ext--crd-registry (make-hash-table :test 'equal))
         (kubed-ext--crd-discovery-state (make-hash-table :test 'equal))
         (kubed-ext--requests nil) (kubed-ext--request-queue nil)
         (kubed-ext-crd-discovery-enabled t)
         (kubed-ext-crd-cache-directory (make-temp-file "kubed-crd-process-" t))
         (fixture (expand-file-name "fixture" kubed-ext-crd-cache-directory))
         (environment (kubed-ext--capture-environment)) count)
    (unwind-protect
        (progn
          (with-temp-file fixture
            (insert "#!/bin/sh\nprintf 'widgets\\texample.io\\tWidget\\tNamespaced\\n'\n"))
          (set-file-modes fixture #o700)
          (plist-put environment :kubectl fixture)
          (kubed-ext--with-environment environment
            (let ((request (kubed-ext-discover-crds-async "fixture" t
                              (lambda (number) (setq count number)))))
              (kubed-ext-core-test-wait request)
              (should (= count 1))
              (should (equal (kubed-ext-crd-resource-choices "fixture") '("widgets.example.io"))))))
      (delete-directory kubed-ext-crd-cache-directory t))))
(ert-deftest kubed-ext-resources-reuse-selector-environment ()
  "Reuse preserves generations and state; selector/environment identities are separate."
  (let ((kubed-ext-crd-discovery-enabled nil) buffers)
    (cl-letf (((symbol-function 'kubed-ext-refresh-resources)
               (lambda () (cl-incf kubed-ext--view-generation)))
              ((symbol-function 'pop-to-buffer) #'ignore))
      (unwind-protect
          (with-temp-buffer
            (let* ((a (kubed-ext-list-resources "pods" "c" "n" "a=1"))
                   (b (kubed-ext-list-resources "pods" "c" "n" "a=2")))
              (setq buffers (list a b))
              (should-not (eq a b))
              (with-current-buffer a (setq kubed-ext--view-filter "persistent"))
              (should (eq a (kubed-ext-list-resources "pods" "c" "n" "a=1")))
              (with-current-buffer a
                (should (= kubed-ext--view-generation 2))
                (should (equal kubed-ext--view-filter "persistent")))
              (setq-local process-environment (cons "KUBED_TEST=other" process-environment))
              (let ((other (kubed-ext-list-resources "pods" "c" "n" "a=1")))
                (push other buffers) (should-not (eq other a)))))
        (dolist (buffer buffers) (when (buffer-live-p buffer) (kill-buffer buffer)))))))
(ert-deftest kubed-ext-resources-auto-refresh-cleanup ()
  "Visible views deduplicate inflight refresh; kill/disable cancel timers and requests."
  (let ((buffer (generate-new-buffer " *kubed-resource-timer-test*")) (calls 0))
    (unwind-protect
        (with-current-buffer buffer
          (kubed-ext-resources-mode)
          (setq kubed-ext--view-scope '(:type "pods"))
          (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) t))
                    ((symbol-function 'kubed-ext-refresh-resources) (lambda () (cl-incf calls))))
            (kubed-ext-auto-refresh-mode 1)
            (setq kubed-ext--view-request (kubed-ext--make-request))
            (kubed-ext--auto-refresh-tick buffer)
            (should (= calls 0))
            (setf (kubed-ext-request-finished kubed-ext--view-request) t)
            (kubed-ext--auto-refresh-tick buffer)
            (should (= calls 1))
            (kubed-ext-auto-refresh-mode -1)
            (should-not kubed-ext--refresh-timer)
            (should-not kubed-ext--view-request)))
      (kill-buffer buffer))))
(ert-deftest kubed-ext-resources-rejects-ambiguous-table ()
  "All-namespace tables must provide a namespace, not ambiguous bare names."
  (with-temp-buffer
    (kubed-ext-resources-mode)
    (setq kubed-ext--view-scope '(:type "pods" :all-namespaces t))
    (should-error (kubed-ext--render-resource-response 0 '("NAME") '(("ambiguous")))))
  (should-error (kubed-ext--parse-server-table "STATUS AGE\nReady  3d\n")))
(ert-deftest kubed-ext-resources-shutdown-hook ()
  "The transport shutdown entry point also stops resource timers and discovery."
  (let ((buffer (generate-new-buffer " *kubed-shutdown-test*"))
        (kubed-ext--requests nil) (kubed-ext--request-queue nil)
        (kubed-ext--crd-discovery-state (make-hash-table :test 'equal))
        (kubed-ext-shutdown-hook '(kubed-ext-resources-stop-all)))
    (unwind-protect
        (with-current-buffer buffer
          (kubed-ext-resources-mode)
          (setq kubed-ext--view-scope '(:type "pods"))
          (kubed-ext-auto-refresh-mode 1)
          (cl-letf (((symbol-function 'buffer-list) (lambda (&rest _) (list buffer))))
            (kubed-ext--cancel-all-requests))
          (should-not kubed-ext--refresh-timer)
          (should-not kubed-ext-auto-refresh-mode))
      (kill-buffer buffer))))
(ert-deftest kubed-ext-resources-bookmark-selection-and-secrets ()
  "Bookmarks restore row identity and never serialize captured process environment."
  (with-temp-buffer
    (kubed-ext-resources-mode)
    (setq kubed-ext--environment (kubed-ext--capture-environment)
          kubed-ext--view-scope (list :type "pods" :namespace "n" :environment kubed-ext--environment)
          kubed-ext--view-bookmark-row "b")
    (kubed-ext--render-resource-response 0 '("NAME") '(("a") ("b")))
    (should (equal (tabulated-list-get-id) "b"))
    (let ((record (kubed-ext--bookmark-record)))
      (should (equal (alist-get 'row record) "b"))
      (should-not (plist-get (alist-get 'scope record) :environment))
      (should (= (length (alist-get 'environment-id record)) 64)))))
(ert-deftest kubed-ext-resources-cache-disk-failure-keeps-memory ()
  "A real persistence error cannot discard a successfully registered CRD."
  (let* ((kubed-ext-crd-cache-directory "/dev/null/not-a-directory/")
         (kubed-ext--crd-registry (make-hash-table :test 'equal))
         (environment (kubed-ext--capture-environment))
         (entries (kubed-ext--parse-crd-compact-list "things\texample.io\tThing\tCluster\n")))
    (kubed-ext--replace-crd-entries "c" environment entries)
    (kubed-ext--write-crd-cache "c" environment entries)
    (should (kubed-ext-crd-metadata "things.example.io" "c" environment))))
(ert-deftest kubed-ext-resources-completion-cache-is-async-and-deduplicated ()
  "Context and namespace prompts complete from an asynchronously filled cache."
  (let ((kubed-ext--contexts-cache (make-hash-table :test 'equal))
        (kubed-ext--namespaces-cache (make-hash-table :test 'equal))
        (kubed-ext--completion-requests (make-hash-table :test 'equal))
        (environment (kubed-ext--capture-environment))
        argvs)
    (cl-letf (((symbol-function 'kubed-ext--async-kubectl)
               (lambda (argv success &rest _)
                 (push argv argvs)
                 ;; Reply later, as a real request does.
                 (list :success success))))
      (kubed-ext-contexts-async environment)
      ;; A second request for the same key must not spawn another process.
      (kubed-ext-contexts-async environment)
      (should (= (length argvs) 1))
      (should (equal (cdr (car argvs)) '("config" "get-contexts" "-o" "name")))
      (let ((table (kubed-ext--cached-completion-table
                    'kubed-ext--contexts-cache (kubed-ext--environment-key environment))))
        ;; Nothing has arrived yet: the prompt still works, with no candidates.
        (should-not (all-completions "" table))
        (funcall (plist-get (gethash (list 'kubed-ext--contexts-cache
                                           (kubed-ext--environment-key environment))
                                     kubed-ext--completion-requests)
                            :success)
                 "alpha\nbeta\n")
        ;; The same table now sees the names, without reopening the prompt.
        (should (equal (all-completions "" table) '("alpha" "beta")))))))

(ert-deftest kubed-ext-resources-namespace-completion-is-context-scoped ()
  "Namespace candidates are requested and cached per context."
  (let ((kubed-ext--namespaces-cache (make-hash-table :test 'equal))
        (kubed-ext--completion-requests (make-hash-table :test 'equal))
        (environment (kubed-ext--capture-environment))
        argv)
    (cl-letf (((symbol-function 'kubed-ext--async-kubectl)
               (lambda (command success &rest _) (setq argv command) (list :success success))))
      (kubed-ext-namespaces-async "c" environment)
      (should (member "--context=c" argv))
      (should (member "namespaces" argv))
      (funcall (plist-get (gethash (list 'kubed-ext--namespaces-cache
                                         (kubed-ext--crd-context-key "c" environment))
                                   kubed-ext--completion-requests)
                          :success)
               "kube-system\nprod\n")
      (should (equal (gethash (kubed-ext--crd-context-key "c" environment)
                              kubed-ext--namespaces-cache)
                     '("kube-system" "prod")))
      ;; A different context does not inherit those candidates.
      (should-not (gethash (kubed-ext--crd-context-key "other" environment)
                           kubed-ext--namespaces-cache)))))

(ert-deftest kubed-ext-resources-hidden-columns-keep-cells-aligned ()
  "Hidden columns are dropped from headers and every row together."
  (let ((kubed-ext-resource-hidden-columns '("nominated node" "READINESS GATES")))
    (should (equal (kubed-ext--visible-columns
                    '("NAME" "AGE" "NODE" "NOMINATED NODE" "READINESS GATES")
                    '(("p" "8d" "n1" "<none>" "<none>")))
                   '(("NAME" "AGE" "NODE") ("p" "8d" "n1")))))
  ;; Identity columns are never hidden, whatever the user configures.
  (let ((kubed-ext-resource-hidden-columns '("NAME" "NAMESPACE" "AGE")))
    (should (equal (car (kubed-ext--visible-columns '("NAMESPACE" "NAME" "AGE") nil))
                   '("NAMESPACE" "NAME")))))

(ert-deftest kubed-ext-resources-wide-output-is-requested-and-toggleable ()
  "Wide columns are an explicit kubectl flag, toggled per view."
  (with-temp-buffer
    (kubed-ext-resources-mode)
    (setq kubed-ext--view-scope '(:type "pods" :context "c" :namespace "n" :wide t))
    (should (member "-o" (kubed-ext--resource-list-argv kubed-ext--view-scope)))
    (should (member "wide" (kubed-ext--resource-list-argv kubed-ext--view-scope)))
    (setq kubed-ext--view-wide t)
    (cl-letf (((symbol-function 'kubed-ext-refresh-resources) #'ignore))
      (kubed-ext-toggle-wide-output))
    (should-not kubed-ext--view-wide)
    (should-not (member "wide" (kubed-ext--resource-list-argv kubed-ext--view-scope)))))

(ert-deftest kubed-ext-resources-completion-spawn-failure-unblocks-key ()
  "A synchronous spawn failure must not leave a key permanently inflight."
  (let ((kubed-ext--contexts-cache (make-hash-table :test 'equal))
        (kubed-ext--completion-requests (make-hash-table :test 'equal))
        (environment (kubed-ext--capture-environment))
        (calls 0))
    (cl-letf (((symbol-function 'kubed-ext--async-kubectl)
               (lambda (_argv _success &optional error)
                 (cl-incf calls)
                 ;; The transport reports spawn failure before returning.
                 (when error (funcall error "process exited 1"))
                 (kubed-ext--make-request :argv '("kubectl") :finished t))))
      (kubed-ext-contexts-async environment)
      (should (zerop (hash-table-count kubed-ext--completion-requests)))
      ;; The key is free, so a later attempt really retries.
      (kubed-ext-contexts-async environment)
      (should (= calls 2)))))

(ert-deftest kubed-ext-resources-stop-all-releases-completion-requests ()
  "Shutdown cancels completion requests and frees their keys."
  (let ((kubed-ext--completion-requests (make-hash-table :test 'equal))
        (kubed-ext--crd-discovery-state (make-hash-table :test 'equal))
        (environment (kubed-ext--capture-environment))
        cancelled)
    (cl-letf (((symbol-function 'kubed-ext--async-kubectl)
               (lambda (&rest _) (kubed-ext--make-request :argv '("kubectl"))))
              ((symbol-function 'kubed-ext-cancel-request)
               (lambda (request) (push request cancelled))))
      (kubed-ext-contexts-async environment)
      (should (= (hash-table-count kubed-ext--completion-requests) 1))
      (kubed-ext-resources-stop-all)
      (should (= (length cancelled) 1))
      (should (zerop (hash-table-count kubed-ext--completion-requests))))))

(provide 'kubed-ext-resources-test)
;;; kubed-ext-resources-test.el ends here
