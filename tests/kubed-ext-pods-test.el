;;; kubed-ext-pods-test.el --- Tests for the pod usage table -*- lexical-binding: t; -*-

;;; Commentary:

;; Row construction is pure: it maps a pod object plus a usage table to a
;; tabulated-list entry, so it is tested without a cluster.

;;; Code:

(require 'ert)
(require 'kubed-ext-pods)

(defun kubed-ext-pods-test--pod (&rest overrides)
  "Return a pod object, merging OVERRIDES into its status alist."
  (let* ((resources '((requests . ((cpu . "100m") (memory . "128Mi")))
                      (limits . ((cpu . "200m") (memory . "256Mi")))))
         (metadata '((name . "kminion-86947fd67f-z2rgq")
                     (namespace . "monitoring")
                     (creationTimestamp . "2024-01-01T00:00:00Z")))
         (spec (list (cons 'nodeName "aks-pfd16dsv5-10045168-vmss000052")
                     (cons 'containers (list (list (cons 'resources resources))))))
         (status (list (cons 'phase "Running")
                       (cons 'podIP "10.132.3.10")
                       (cons 'containerStatuses
                             (list (list (cons 'ready t) (cons 'restartCount 0))))))
         (pod (list (cons 'metadata (copy-tree metadata))
                    (cons 'spec spec)
                    (cons 'status status))))
    (dolist (override overrides pod)
      (setf (alist-get (car override) (alist-get 'status pod)) (cdr override)))))

(ert-deftest kubed-ext-pods-row-joins-usage-and-budgets ()
  "A row carries readiness, usage and usage as a percentage of each budget."
  (let ((usage (make-hash-table :test 'equal))
        (kubed-ext--port-forwards nil))
    (puthash '("monitoring" . "kminion-86947fd67f-z2rgq") '("50m" "64Mi") usage)
    (let ((cells (append (cadr (kubed-ext--pods-row (kubed-ext-pods-test--pod) usage nil)) nil)))
      (should (equal (nth 0 cells) "kminion-86947fd67f-z2rgq"))
      (should (equal (nth 1 cells) ""))           ; no port forward
      (should (equal (nth 2 cells) "1/1"))
      (should (equal (nth 3 cells) "Running"))
      (should (equal (nth 4 cells) "0"))
      (should (equal (nth 5 cells) "50m"))
      (should (equal (nth 6 cells) "50"))         ; 50m of a 100m request
      (should (equal (nth 7 cells) "25"))         ; 50m of a 200m limit
      (should (equal (nth 8 cells) "64.0Mi"))
      (should (equal (nth 9 cells) "50"))         ; 64Mi of a 128Mi request
      (should (equal (nth 10 cells) "25"))        ; 64Mi of a 256Mi limit
      (should (equal (nth 11 cells) "10.132.3.10"))
      (should (equal (nth 12 cells) "aks-pfd16dsv5-10045168-vmss000052")))))

(ert-deftest kubed-ext-pods-row-without-metrics-server ()
  "Missing usage leaves metric columns unavailable instead of failing."
  (let ((usage (make-hash-table :test 'equal)) (kubed-ext--port-forwards nil))
    (let ((cells (append (cadr (kubed-ext--pods-row (kubed-ext-pods-test--pod) usage nil)) nil)))
      (should (equal (nth 5 cells) "n/a"))
      (should (equal (nth 6 cells) "n/a"))
      (should (equal (nth 9 cells) "n/a")))))

(ert-deftest kubed-ext-pods-row-without-declared-budgets ()
  "Usage is still shown when a pod declares no requests or limits."
  (let ((usage (make-hash-table :test 'equal))
        (kubed-ext--port-forwards nil)
        (pod (kubed-ext-pods-test--pod)))
    (setf (alist-get 'containers (alist-get 'spec pod)) '(((resources . nil))))
    (puthash '("monitoring" . "kminion-86947fd67f-z2rgq") '("1m" "18Mi") usage)
    (let ((cells (append (cadr (kubed-ext--pods-row pod usage nil)) nil)))
      (should (equal (nth 5 cells) "1m"))
      (should (equal (nth 6 cells) "n/a"))
      (should (equal (nth 7 cells) "n/a")))))

(ert-deftest kubed-ext-pods-status-prefers-blocking-reason ()
  "A waiting container reason and deletion outrank the pod phase."
  (should (equal (kubed-ext--pods-status (kubed-ext-pods-test--pod)) "Running"))
  (should (equal (kubed-ext--pods-status
                  (kubed-ext-pods-test--pod
                   '(containerStatuses . (((ready . nil)
                                           (state . ((waiting . ((reason . "CrashLoopBackOff")))))))))) 
                 "CrashLoopBackOff"))
  (let ((pod (kubed-ext-pods-test--pod)))
    (setf (alist-get 'deletionTimestamp (alist-get 'metadata pod)) "2024-01-02T00:00:00Z")
    (should (equal (kubed-ext--pods-status pod) "Terminating"))))

(ert-deftest kubed-ext-pods-ready-counts-only-ready-containers ()
  "READY counts ready containers against the total."
  (should (equal (kubed-ext--pods-ready
                  (kubed-ext-pods-test--pod
                   '(containerStatuses . (((ready . t)) ((ready . nil)) ((ready . t))))))
                 "2/3")))

(ert-deftest kubed-ext-pods-forward-indicator-tracks-registry ()
  "The PF column reflects this extension's own forwarding sessions."
  (let ((kubed-ext--port-forwards
         '((:type "pod" :name "kminion-86947fd67f-z2rgq" :namespace "monitoring"
            :state ready))))
    (should (equal (substring-no-properties
                    (aref (cadr (kubed-ext--pods-row (kubed-ext-pods-test--pod)
                                                     (make-hash-table :test 'equal) nil))
                          1))
                   "●")))
  ;; A session for another namespace must not mark this pod.
  (let ((kubed-ext--port-forwards
         '((:type "pod" :name "kminion-86947fd67f-z2rgq" :namespace "other" :state ready))))
    (should (equal (aref (cadr (kubed-ext--pods-row (kubed-ext-pods-test--pod)
                                                    (make-hash-table :test 'equal) nil))
                         1)
                   ""))))

(ert-deftest kubed-ext-pods-age-is-compact-and-sortable ()
  "Age renders like kubectl and carries seconds for sorting."
  (should (equal (cdr (kubed-ext--pods-age
                       (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                                           (time-subtract nil (* 9 24 3600)) t)))
                 "9d"))
  (should (equal (cdr (kubed-ext--pods-age
                       (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                                           (time-subtract nil 90) t)))
                 "1m"))
  (should (equal (kubed-ext--pods-age nil) '(nil . ""))))

(ert-deftest kubed-ext-pods-sorts-metrics-numerically ()
  "Metric columns sort by value, not by their printed text."
  (let* ((rows (list (list "a" (vector (kubed-ext--pods-cell "100m" 100)))
                     (list "b" (vector (kubed-ext--pods-cell "20m" 20)))))
         (sorter (kubed-ext--pods-sorter 0)))
    (should (equal (mapcar #'car (sort rows sorter)) '("b" "a")))))

(ert-deftest kubed-ext-pods-all-namespace-rows-are-unique ()
  "All-namespace rows are identified by namespace and name."
  (let ((row (kubed-ext--pods-row (kubed-ext-pods-test--pod)
                                  (make-hash-table :test 'equal) t)))
    (should (equal (car row) "monitoring/kminion-86947fd67f-z2rgq"))
    (should (equal (aref (cadr row) 0) "monitoring"))))

(ert-deftest kubed-ext-pods-mode-receives-row-bindings ()
  "Setup installs the shared row actions in the pod table, not only in lists."
  (kubed-ext-setup)
  (dolist (key '("?" "v" "G" "l" "!" "RET" "d" "f"))
    (should (commandp (keymap-lookup kubed-ext-pods-mode-map key))))
  (should (eq (keymap-lookup kubed-ext-pods-mode-map "?") #'kubed-ext-transient-menu))
  (should (eq (keymap-lookup kubed-ext-pods-mode-map "g") #'kubed-ext-pods-refresh)))

(ert-deftest kubed-ext-pods-row-actions-resolve-exact-pod ()
  "A selected row yields the pod scope that actions and predicates expect."
  (with-temp-buffer
    (kubed-ext-pods-mode)
    (setq kubed-ext--pods-scope '(:type "pods" :context "c" :namespace nil)
          kubed-list-type "pods" kubed-list-context "c" kubed-list-namespace nil
          kubed-ext--pods-objects (list (kubed-ext-pods-test--pod))
          kubed-ext--pods-usage (make-hash-table :test 'equal))
    (let ((kubed-ext--port-forwards nil))
      (kubed-ext--pods-render))
    (goto-char (point-min))
    (let ((scope (kubed-ext-selected-scope)))
      (should (equal (plist-get scope :type) "pods"))
      (should (equal (plist-get scope :name) "kminion-86947fd67f-z2rgq"))
      (should (equal (plist-get scope :namespace) "monitoring"))
      (should (equal (plist-get scope :context) "c")))
    ;; Pod-only menu entries must light up here.
    (should (kubed-ext--pod-selected-p))
    (should (kubed-ext--logs-selected-p))
    (should (kubed-ext--refreshable-p))))

(ert-deftest kubed-ext-pods-cells-carry-status-faces ()
  "Status, readiness, restarts and saturated usage are highlighted."
  (let ((usage (make-hash-table :test 'equal)) (kubed-ext--port-forwards nil))
    (puthash '("monitoring" . "kminion-86947fd67f-z2rgq") '("190m" "250Mi") usage)
    (let* ((pod (kubed-ext-pods-test--pod
                 '(containerStatuses . (((ready . nil) (restartCount . 3)
                                         (state . ((waiting . ((reason . "CrashLoopBackOff"))))))))))
           (cells (append (cadr (kubed-ext--pods-row pod usage nil)) nil)))
      (should (eq (get-text-property 0 'face (nth 2 cells)) 'kubed-ext-pods-warning))
      (should (eq (get-text-property 0 'face (nth 3 cells)) 'kubed-ext-pods-critical))
      (should (eq (get-text-property 0 'face (nth 4 cells)) 'kubed-ext-pods-warning))
      ;; 190m against a 200m limit is past the warning threshold.
      (should (eq (get-text-property 0 'face (nth 7 cells)) 'kubed-ext-pods-warning))))
  ;; A healthy pod stays unadorned apart from its success status.
  (let ((cells (append (cadr (kubed-ext--pods-row (kubed-ext-pods-test--pod)
                                                  (make-hash-table :test 'equal) nil))
                       nil)))
    (should (eq (get-text-property 0 'face (nth 3 cells)) 'kubed-ext-pods-ok))
    (should-not (get-text-property 0 'face (nth 2 cells)))))

(provide 'kubed-ext-pods-test)
;;; kubed-ext-pods-test.el ends here
