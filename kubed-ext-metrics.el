;;; kubed-ext-metrics.el --- Asynchronous pod and node metric tables -*- lexical-binding: t; -*-
;;; Commentary:
;; Metrics and resource budgets are fetched separately, never invented.
;;; Code:
(require 'kubed-ext-actions)
(require 'json)
(defcustom kubed-ext-metric-item-limit 10000
  "Positive maximum objects or usage rows accepted per metrics response."
  :type 'integer :group 'kubed-ext)
(defvar-local kubed-ext--node-budgets nil)
(defvar-local kubed-ext--top-scope nil)
(defvar-local kubed-ext--top-environment nil)
(defvar-local kubed-ext--top-generation 0)
(defvar-local kubed-ext--top-usage nil)
(defvar-local kubed-ext--top-objects nil)
(defvar-local kubed-ext--top-pods nil)
(defvar-local kubed-ext--top-errors nil)
(defvar-local kubed-ext--top-requests nil)

(defun kubed-ext--metric-value (quantity kind)
  "Convert Kubernetes QUANTITY to millicores or bytes for KIND, or nil."
  (when (and (stringp quantity)
             (string-match "\\`\\([0-9]+\\(?:\\.[0-9]+\\)?\\)\\([[:alpha:]]*\\)\\'" quantity))
    (let* ((number (string-to-number (match-string 1 quantity)))
           (suffix (match-string 2 quantity))
           (factor (if (eq kind 'cpu)
                       (cdr (assoc suffix '(("" . 1000) ("m" . 1) ("u" . 0.001) ("n" . 0.000001))))
                     (cdr (assoc suffix '(("" . 1) ("Ki" . 1024) ("Mi" . 1048576) ("Gi" . 1073741824)
                                           ("Ti" . 1099511627776) ("Pi" . 1125899906842624)
                                           ("Ei" . 1152921504606846976) ("k" . 1000) ("K" . 1000)
                                           ("M" . 1000000) ("G" . 1000000000) ("T" . 1000000000000)))))))
      (when factor (* number factor)))))
(defun kubed-ext--metric-quantity (quantity kind)
  "Display QUANTITY for KIND; missing/unparseable quantities remain unavailable."
  (if (kubed-ext--metric-value quantity kind) quantity "n/a"))
(defun kubed-ext--metric-format (value kind)
  "Format numeric VALUE in KIND units, or unavailable."
  (if (not (numberp value)) "n/a"
    (if (eq kind 'cpu) (format "%.0fm" value) (format "%.1fMi" (/ value 1048576.0)))))
(defun kubed-ext--metric-budget-value (containers budget kind)
  "Sum CONTAINERS' BUDGET for KIND, returning nil if any value is missing."
  (when containers
    (let ((values (mapcar (lambda (c) (kubed-ext--metric-value
                                     (alist-get kind (alist-get budget (alist-get 'resources c))) kind)) containers)))
      (when (cl-every #'numberp values) (apply #'+ values)))))
(defun kubed-ext--metric-budget (containers budget kind)
  "Display aggregate CONTAINERS BUDGET for KIND without fabricating defaults."
  (kubed-ext--metric-format (kubed-ext--metric-budget-value containers budget kind) kind))
(defun kubed-ext--pod-budget-value (pod budget kind)
  "Compute effective POD BUDGET for KIND including init containers and overhead.
Missing declared quantities remain unavailable.  Restartable init sidecars
contribute both to startup peaks and to steady-state requirements."
  (let* ((spec (alist-get 'spec pod))
         (regular (kubed-ext--metric-budget-value (alist-get 'containers spec) budget kind))
         (sidecars 0) (peak 0) (known (numberp regular)))
    (dolist (container (alist-get 'initContainers spec))
      (let ((value (kubed-ext--metric-budget-value (list container) budget kind)))
        (if (not value) (setq known nil)
          (if (equal (alist-get 'restartPolicy container) "Always")
              (setq sidecars (+ sidecars value) peak (max peak sidecars))
            (setq peak (max peak (+ sidecars value)))))))
    (when known
      (let* ((overhead (alist-get kind (alist-get 'overhead spec)))
             (extra (if overhead (kubed-ext--metric-value overhead kind) 0)))
        (when (numberp extra) (+ (max (+ regular sidecars) peak) extra))))))

(defun kubed-ext--pod-budget (pod budget kind)
  "Format effective POD BUDGET for KIND, preserving unavailable values."
  (kubed-ext--metric-format (kubed-ext--pod-budget-value pod budget kind) kind))

(defun kubed-ext--metric-items (text)
  "Parse Kubernetes list TEXT into non-executable JSON item alists."
  (let ((json-object-type 'alist) (json-array-type 'list)
        (json-null nil) (json-false nil))
    (let ((items (alist-get 'items (json-read-from-string text))))
      (unless (and (integerp kubed-ext-metric-item-limit) (> kubed-ext-metric-item-limit 0)
                   (<= (length items) kubed-ext-metric-item-limit))
        (error "Metrics dataset exceeds item limit; narrow query"))
      items)))
(defun kubed-ext--metric-id (pod)
  "Return namespace-qualified identity for POD object."
  (let ((m (alist-get 'metadata pod)))
    (cons (alist-get 'namespace m) (alist-get 'name m))))
(defun kubed-ext--top-parse (text scope)
  "Parse no-header kubectl top TEXT for SCOPE, validating each row."
  (let ((table (make-hash-table :test 'equal))
        (nodes (equal (plist-get scope :type) "nodes"))
        (all (not (plist-get scope :namespace))))
    (dolist (line (split-string text "\n" t))
      (when (>= (hash-table-count table) kubed-ext-metric-item-limit)
        (error "Metrics usage exceeds item limit; narrow query"))
      (let* ((cells (split-string line "[[:space:]]+" t))
             (expected (if nodes 5 (if all 4 3))))
        (unless (= (length cells) expected) (error "Malformed top row: %s" line))
        (let ((id (if nodes (nth 0 cells)
                    (cons (if all (nth 0 cells) (plist-get scope :namespace)) (nth (if all 1 0) cells))))
              (cpu (nth (if (and all (not nodes)) 2 1) cells))
              (memory (nth (if nodes 3 (if all 3 2)) cells)))
          (puthash id (list cpu memory) table)))) table))
(defun kubed-ext--node-budget (name budget kind)
  "Compute NAME node's scheduled pod BUDGET for KIND; incomplete data is n/a."
  (kubed-ext--metric-format (and kubed-ext--node-budgets
                                  (gethash (list name budget kind) kubed-ext--node-budgets)) kind))
(defun kubed-ext--aggregate-node-budgets (pods)
  "Aggregate PODS in one linear pass; missing quantities poison only their slot."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (pod pods)
      (let ((node (alist-get 'nodeName (alist-get 'spec pod))))
        (when node
          (dolist (budget '(requests limits))
            (dolist (kind '(cpu memory))
              (let* ((key (list node budget kind))
                     (old (gethash key table 0))
                     (value (kubed-ext--pod-budget-value pod budget kind)))
                (puthash key (and (numberp old) (numberp value) (+ old value)) table)))))))
    table))
(defun kubed-ext--top-render ()
  "Render currently available usage and budget data without changing generation."
  (setq kubed-ext--node-budgets (kubed-ext--aggregate-node-budgets kubed-ext--top-pods))
  (let ((nodes (equal (plist-get kubed-ext--top-scope :type) "nodes")) (rows nil)
        (seen (make-hash-table :test 'equal)))
    (dolist (object kubed-ext--top-objects)
      (let* ((id (if nodes (alist-get 'name (alist-get 'metadata object)) (kubed-ext--metric-id object)))
             (usage (and kubed-ext--top-usage (gethash id kubed-ext--top-usage))))
        (puthash id t seen)
        (push (list id (vector (if nodes id (or (cdr id) "n/a"))
                               (if nodes "—" (or (car id) "n/a"))
                               (kubed-ext--metric-quantity (nth 0 usage) 'cpu)
                               (if nodes (kubed-ext--node-budget id 'requests 'cpu)
                                 (kubed-ext--pod-budget object 'requests 'cpu))
                               (if nodes (kubed-ext--node-budget id 'limits 'cpu)
                                 (kubed-ext--pod-budget object 'limits 'cpu))
                               (kubed-ext--metric-quantity (nth 1 usage) 'memory)
                               (if nodes (kubed-ext--node-budget id 'requests 'memory)
                                 (kubed-ext--pod-budget object 'requests 'memory))
                               (if nodes (kubed-ext--node-budget id 'limits 'memory)
                                 (kubed-ext--pod-budget object 'limits 'memory)))) rows)))
    ;; Retain measured rows even when the resource/budget endpoint is denied.
    (when kubed-ext--top-usage
      (maphash (lambda (id usage)
                 (unless (gethash id seen)
                   (push (list id (vector (if nodes id (cdr id)) (if nodes "—" (car id))
                                          (kubed-ext--metric-quantity (nth 0 usage) 'cpu) "n/a" "n/a"
                                          (kubed-ext--metric-quantity (nth 1 usage) 'memory) "n/a" "n/a")) rows)))
               kubed-ext--top-usage))
    (setq tabulated-list-entries (nreverse rows)
          header-line-format (format "%s  %s [%s/%s] generation %d%s"
                                     (kubed-ext--scope-label kubed-ext--top-scope)
                                     (plist-get kubed-ext--top-scope :type) (plist-get kubed-ext--top-scope :context)
                                     (or (plist-get kubed-ext--top-scope :namespace) "all namespaces")
                                     kubed-ext--top-generation
                                     (if kubed-ext--top-errors (concat " — " (mapconcat #'identity kubed-ext--top-errors "; ")) "")))
    (tabulated-list-print t)))
(define-derived-mode kubed-ext-top-mode tabulated-list-mode "Kubed top"
  "Asynchronous CPU and memory usage, requests and limits."
  (setq tabulated-list-format [("Name" 30 t) ("Namespace" 22 t) ("CPU" 12 t)
                               ("CPU requests" 14 t) ("CPU limits" 14 t) ("Memory" 14 t)
                               ("Mem requests" 16 t) ("Mem limits" 16 t)])
  (tabulated-list-init-header)
  (kubed-ext--own-buffer #'kubed-ext--top-cancel))
(keymap-set kubed-ext-top-mode-map "g" #'kubed-ext-top-refresh)
(defun kubed-ext--top-cancel ()
  "Invalidate pending callbacks and cancel owned requests."
  (cl-incf kubed-ext--top-generation)
  (dolist (request kubed-ext--top-requests)
    (when (kubed-ext-request-p request) (kubed-ext-cancel-request request)))
  (setq kubed-ext--top-requests nil))
(defun kubed-ext--top-request (buffer generation argv slot parser)
  "Fetch ARGV for BUFFER/GENERATION, parsing into SLOT with PARSER."
  (let ((request
         (kubed-ext--async-kubectl
          argv
          (lambda (text)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (when (= generation kubed-ext--top-generation)
                  (condition-case error (set slot (funcall parser text))
                    (error (push (error-message-string error) kubed-ext--top-errors)))
                  (kubed-ext--top-render)))))
          (lambda (error)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (when (= generation kubed-ext--top-generation)
                  (push error kubed-ext--top-errors) (kubed-ext--top-render))))))))
    (push request kubed-ext--top-requests)))
(defun kubed-ext-top-refresh ()
  "Refresh the captured metric view asynchronously, rejecting stale responses."
  (interactive)
  (unless kubed-ext--top-scope (user-error "Not a metrics buffer"))
  (kubed-ext--top-cancel)
  (setq kubed-ext--top-errors nil kubed-ext--top-usage nil
        kubed-ext--top-objects nil kubed-ext--top-pods nil header-line-format "Metrics pending")
  (let* ((scope (copy-tree kubed-ext--top-scope)) (generation kubed-ext--top-generation)
         (buffer (current-buffer)) (type (plist-get scope :type))
         (nodes (equal type "nodes"))
         (args (append (kubed-ext--action-scope-args scope)
                       (when (and (not nodes) (not (plist-get scope :namespace))) '("--all-namespaces")))))
    (kubed-ext--with-environment kubed-ext--top-environment
      (let ((program (kubed-ext--kubectl-program)))
        (kubed-ext--top-request buffer generation
                               (append (list program "top" type "--no-headers") args)
                               'kubed-ext--top-usage (lambda (text) (kubed-ext--top-parse text scope)))
        (kubed-ext--top-request buffer generation (append (list program "get" type "-o" "json") args)
                               'kubed-ext--top-objects #'kubed-ext--metric-items)
        (when nodes
          (kubed-ext--top-request buffer generation
                                 (append (list program "get" "pods" "--all-namespaces" "-o" "json") args)
                                 'kubed-ext--top-pods #'kubed-ext--metric-items))))))
(defun kubed-ext--top-open (type context namespace)
  "Resolve CONTEXT once and open a TYPE metric table."
  (kubed-ext--resolved context namespace 'all
    (kubed-ext--top-open-resolved type context namespace)))
(defun kubed-ext--top-open-resolved (type context namespace)
  "Open a TYPE metric table in explicit CONTEXT/NAMESPACE."
  (let* ((environment (kubed-ext--capture-environment))
         (name (format "*kubed-top-%s %s [%s] %s*" type context (or namespace "all")
                       (substring (secure-hash 'sha1 (prin1-to-string environment)) 0 8)))
         (buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'kubed-ext-top-mode)
        (if (equal type "nodes") (kubed-ext-top-nodes-mode) (kubed-ext-top-pods-mode)))
      (kubed-ext--apply-environment-to-new-buffer environment)
      (setq kubed-ext--top-scope (list :type type :context context :namespace namespace :environment environment)
            kubed-ext--top-environment environment)
      (kubed-ext-top-refresh))
    (pop-to-buffer buffer) buffer))
(defun kubed-ext-top-pods (&optional context namespace)
  "Show asynchronous pod usage and resource budgets in CONTEXT/NAMESPACE."
  (interactive (let ((s (kubed-ext--action-selected-scope)))
                 (list (plist-get s :context) (plist-get s :namespace))))
  (kubed-ext--top-open "pods" context namespace))
(defun kubed-ext-top-nodes (&optional context)
  "Show asynchronous node usage and scheduled pod budgets in CONTEXT."
  (interactive (list (plist-get (kubed-ext--action-selected-scope) :context)))
  (kubed-ext--top-open "nodes" context nil))
(defun kubed-ext-top--set-status (text &optional face)
  "Set metrics header TEXT with optional FACE."
  (setq header-line-format (if face (propertize text 'face face) text)))
(defun kubed-ext-top-numeric-sorter (column)
  "Return a numeric row comparator for metric COLUMN."
  (lambda (a b)
    (< (or (kubed-ext--metric-value (aref (cadr a) column)
                                   (if (< column 5) 'cpu 'memory)) -1)
       (or (kubed-ext--metric-value (aref (cadr b) column)
                                   (if (< column 5) 'cpu 'memory)) -1))))

(define-derived-mode kubed-ext-top-pods-mode kubed-ext-top-mode "Kubed Pod Metrics"
  "Pod CPU/memory usage and effective requests/limits.")
(define-derived-mode kubed-ext-top-nodes-mode kubed-ext-top-mode "Kubed Node Metrics"
  "Node usage and scheduled pod CPU/memory budgets.")

(provide 'kubed-ext-metrics)
;;; kubed-ext-metrics.el ends here
