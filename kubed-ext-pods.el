;;; kubed-ext-pods.el --- Pod table with usage and port forwards -*- lexical-binding: t; -*-

;;; Commentary:

;; A pod list that joins three sources: `kubectl get pods -o json' for
;; identity, readiness and declared resources, `kubectl top pods' for current
;; usage, and the extension's own port-forward registry.  Usage is optional:
;; without metrics-server the table still renders, with usage columns marked
;; unavailable.

;;; Code:

(require 'tabulated-list)
(require 'kubed-ext-core)
(require 'kubed-ext-metrics)
(require 'kubed-ext-actions)

(defvar-local kubed-ext--pods-scope nil "Immutable scope of this pod table.")
(defvar-local kubed-ext--pods-generation 0 "Monotonic request generation.")
(defvar-local kubed-ext--pods-objects nil "Pod objects of the last response.")
(defvar-local kubed-ext--pods-usage nil "Pod identity to (CPU MEMORY) strings.")
(defvar-local kubed-ext--pods-requests nil "Owned inflight requests.")

(defface kubed-ext-pods-forwarded '((t :inherit success))
  "Port-forward indicator of a ready session."
  :group 'kubed-ext)
(defface kubed-ext-pods-ok '((t :inherit success))
  "Healthy pod status and unremarkable usage."
  :group 'kubed-ext)
(defface kubed-ext-pods-warning '((t :inherit warning))
  "Transitional pod status and usage approaching a declared budget."
  :group 'kubed-ext)
(defface kubed-ext-pods-critical '((t :inherit error))
  "Failing pod status and usage at or above a declared budget."
  :group 'kubed-ext)

(defcustom kubed-ext-pods-usage-warning 80
  "Percentage of a declared budget highlighted as approaching its limit."
  :type 'integer :group 'kubed-ext)

(defcustom kubed-ext-pods-usage-critical 100
  "Percentage of a declared budget highlighted as exhausted."
  :type 'integer :group 'kubed-ext)

(defun kubed-ext--pods-status-face (status)
  "Return the face for pod STATUS, or nil when it carries no signal."
  (cond ((member status '("Running" "Succeeded" "Completed")) 'kubed-ext-pods-ok)
        ((member status '("Pending" "ContainerCreating" "PodInitializing"
                          "Terminating" "Init" "Unknown"))
         'kubed-ext-pods-warning)
        ((null status) nil)
        ((string-match-p "BackOff\\|Error\\|Failed\\|Evicted\\|OOM\\|Unschedulable\\|Invalid"
                         status)
         'kubed-ext-pods-critical)
        (t 'kubed-ext-pods-warning)))

(defun kubed-ext--pods-usage-face (percentage)
  "Return the face for PERCENTAGE of a declared budget, or nil."
  (and (numberp percentage)
       (cond ((>= percentage kubed-ext-pods-usage-critical) 'kubed-ext-pods-critical)
             ((>= percentage kubed-ext-pods-usage-warning) 'kubed-ext-pods-warning))))

(defun kubed-ext--pods-age (timestamp)
  "Return age of TIMESTAMP as seconds and a compact label, as kubectl prints it."
  (if (not (stringp timestamp)) (cons nil "")
    (let* ((seconds (max 0 (floor (float-time (time-since (date-to-time timestamp))))))
           (minutes (/ seconds 60)) (hours (/ minutes 60)) (days (/ hours 24)))
      (cons seconds
            (cond ((< seconds 60) (format "%ds" seconds))
                  ((< minutes 60) (format "%dm" minutes))
                  ((< hours 24) (format "%dh%dm" hours (% minutes 60)))
                  ((< days 365) (format "%dd" days))
                  (t (format "%dy%dd" (/ days 365) (% days 365))))))))

(defun kubed-ext--pods-status (pod)
  "Return the effective status of POD, preferring a blocking container reason."
  (let* ((metadata (alist-get 'metadata pod))
         (status (alist-get 'status pod))
         (reason (seq-some (lambda (container)
                             (or (alist-get 'reason (alist-get 'waiting (alist-get 'state container)))
                                 (let ((terminated (alist-get 'terminated (alist-get 'state container))))
                                   (and terminated (alist-get 'reason terminated)))))
                           (alist-get 'containerStatuses status))))
    (cond ((alist-get 'deletionTimestamp metadata) "Terminating")
          ((member reason '("Completed" nil)) (or (alist-get 'phase status) "Unknown"))
          (t reason))))

(defun kubed-ext--pods-ready (pod)
  "Return the ready and total container counts of POD as a READY cell."
  (let ((containers (alist-get 'containerStatuses (alist-get 'status pod))))
    (format "%d/%d"
            (seq-count (lambda (c) (eq t (alist-get 'ready c))) containers)
            (length containers))))

(defun kubed-ext--pods-restarts (pod)
  "Return the total restart count of POD's containers."
  (apply #'+ (mapcar (lambda (c) (or (alist-get 'restartCount c) 0))
                     (alist-get 'containerStatuses (alist-get 'status pod)))))

(defun kubed-ext--pods-forward-state (namespace name)
  "Return the port-forward state of pod NAME in NAMESPACE, or nil."
  (seq-some (lambda (entry)
              (and (member (plist-get entry :type) '("pod" "pods"))
                   (equal (plist-get entry :name) name)
                   (equal (plist-get entry :namespace) namespace)
                   (or (plist-get entry :state) 'pending)))
            kubed-ext--port-forwards))

(defun kubed-ext--pods-ratio (usage budget)
  "Return USAGE as an integer percentage of BUDGET, or nil."
  (and (numberp usage) (numberp budget) (> budget 0)
       ;; Scale before dividing: integer division truncates to zero.
       (round (/ (* 100.0 usage) budget))))

(defun kubed-ext--pods-cell (label value &optional face)
  "Return LABEL carrying numeric VALUE for sorting, displayed in FACE."
  (let ((cell (if (numberp value) (propertize label 'kubed-ext-sort value) label)))
    (if face (propertize cell 'face face) cell)))

(defun kubed-ext--pods-sorter (column)
  "Return a comparator for COLUMN using the numeric value carried by its cells."
  (lambda (a b)
    (let ((x (get-text-property 0 'kubed-ext-sort (aref (cadr a) column)))
          (y (get-text-property 0 'kubed-ext-sort (aref (cadr b) column))))
      (if (and (numberp x) (numberp y)) (< x y)
        (string< (aref (cadr a) column) (aref (cadr b) column))))))

(defun kubed-ext--pods-row (pod usage all-namespaces)
  "Return the tabulated-list entry of POD, using USAGE for its metric cells.
ALL-NAMESPACES adds the namespace column."
  (let* ((metadata (alist-get 'metadata pod))
         (name (alist-get 'name metadata))
         (namespace (alist-get 'namespace metadata))
         (measured (gethash (cons namespace name) usage))
         (cpu (and measured (kubed-ext--metric-value (nth 0 measured) 'cpu)))
         (memory (and measured (kubed-ext--metric-value (nth 1 measured) 'memory)))
         (age (kubed-ext--pods-age (alist-get 'creationTimestamp metadata)))
         (forward (kubed-ext--pods-forward-state namespace name))
         (restarts (kubed-ext--pods-restarts pod)))
    (cl-flet ((ratio (used budget kind)
                (let ((value (kubed-ext--pods-ratio
                              used (kubed-ext--pod-budget-value pod budget kind))))
                  (kubed-ext--pods-cell (if value (number-to-string value) "n/a") value
                                        (kubed-ext--pods-usage-face value)))))
      (list (if all-namespaces (concat namespace "/" name) name)
            (vconcat
             (when all-namespaces (list namespace))
             (list name
                   (pcase forward
                     ('ready (propertize "●" 'face 'kubed-ext-pods-forwarded))
                     ('nil "")
                     (_ "◌"))
                   (let ((ready (kubed-ext--pods-ready pod)))
                     (if (string-match-p "\\`\\([0-9]+\\)/\\1\\'" ready) ready
                       (propertize ready 'face 'kubed-ext-pods-warning)))
                   (let ((status (kubed-ext--pods-status pod)))
                     (if-let* ((face (kubed-ext--pods-status-face status)))
                         (propertize status 'face face) status))
                   (kubed-ext--pods-cell (number-to-string restarts) restarts
                                         (and (> restarts 0) 'kubed-ext-pods-warning))
                   (kubed-ext--pods-cell (kubed-ext--metric-format cpu 'cpu) cpu)
                   (ratio cpu 'requests 'cpu)
                   (ratio cpu 'limits 'cpu)
                   (kubed-ext--pods-cell (kubed-ext--metric-format memory 'memory) memory)
                   (ratio memory 'requests 'memory)
                   (ratio memory 'limits 'memory)
                   (or (alist-get 'podIP (alist-get 'status pod)) "")
                   (or (alist-get 'nodeName (alist-get 'spec pod)) "")
                   (kubed-ext--pods-cell (cdr age) (car age))))))))

(defun kubed-ext--pods-format (all-namespaces)
  "Return `tabulated-list-format' for the pod table.
ALL-NAMESPACES adds the namespace column."
  (let ((columns `(("NAME" 46) ("PF" 2) ("READY" 5) ("STATUS" 16) ("RESTARTS" 8 right)
                   ("CPU" 5 right) ("%CPU/R" 6 right) ("%CPU/L" 6 right)
                   ("MEM" 8 right) ("%MEM/R" 6 right) ("%MEM/L" 6 right)
                   ("IP" 15) ("NODE" 34) ("AGE" 7))))
    (when all-namespaces (push '("NAMESPACE" 20) columns))
    (vconcat (cl-loop for (title width align) in columns
                      for column from 0
                      collect (append (list title width (kubed-ext--pods-sorter column))
                                      (when (eq align 'right) '(:right-align t)))))))

(defun kubed-ext--pods-render ()
  "Render the retained pod objects and usage of the current buffer."
  (let ((all-namespaces (not (plist-get kubed-ext--pods-scope :namespace)))
        (usage (or kubed-ext--pods-usage (make-hash-table :test 'equal)))
        (point-id (tabulated-list-get-id)))
    (setq tabulated-list-format (kubed-ext--pods-format all-namespaces))
    (unless (assoc (car tabulated-list-sort-key) (append tabulated-list-format nil))
      (setq tabulated-list-sort-key (cons "NAME" nil)))
    (setq tabulated-list-entries
          (mapcar (lambda (pod) (kubed-ext--pods-row pod usage all-namespaces))
                  kubed-ext--pods-objects))
    (setq-local mode-line-buffer-identification
                (list (kubed-ext--scope-label kubed-ext--pods-scope)))
    (setq-local mode-line-process (format " [%d pods]" (length tabulated-list-entries)))
    (tabulated-list-init-header)
    (tabulated-list-print t)
    (when point-id
      (goto-char (point-min))
      (while (and (not (eobp)) (not (equal point-id (tabulated-list-get-id))))
        (forward-line 1)))))

(defun kubed-ext--pods-cancel ()
  "Cancel this table's inflight requests and invalidate pending responses."
  (cl-incf kubed-ext--pods-generation)
  (mapc #'kubed-ext-cancel-request kubed-ext--pods-requests)
  (setq kubed-ext--pods-requests nil))

(define-derived-mode kubed-ext-pods-mode tabulated-list-mode "Kubed-Pods"
  "Pod table with usage, declared budgets and port-forward state."
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "NAME" nil))
  (kubed-ext--own-buffer #'kubed-ext--pods-cancel))

(keymap-set kubed-ext-pods-mode-map "g" #'kubed-ext-pods-refresh)

(defun kubed-ext--pods-request (buffer generation argv handler)
  "Run ARGV for BUFFER and pass its output to HANDLER when GENERATION is current."
  (push (kubed-ext--async-kubectl
         argv
         (lambda (output)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (when (= generation kubed-ext--pods-generation)
                 (condition-case err
                     (progn (funcall handler output) (kubed-ext--pods-render))
                   (error (setq mode-line-process
                                (propertize " [invalid response]" 'face 'error))
                          (message "Kubed pods: %s" (error-message-string err))))))))
         (lambda (failure)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (when (= generation kubed-ext--pods-generation)
                 (message "Kubed pods: %s" failure))))))
        kubed-ext--pods-requests))

(defun kubed-ext-pods-refresh ()
  "Refresh pods and usage, discarding responses of earlier refreshes."
  (interactive)
  (unless kubed-ext--pods-scope (user-error "Not a Kubed pod table"))
  (kubed-ext--pods-cancel)
  (let* ((scope kubed-ext--pods-scope)
         (buffer (current-buffer))
         (generation kubed-ext--pods-generation)
         (kubectl (or (plist-get (plist-get scope :environment) :kubectl)
                      (kubed-ext--kubectl-program)))
         (args (kubed-ext--scope-args
                (append scope (unless (plist-get scope :namespace) '(:all-namespaces t))))))
    (setq-local mode-line-process " [refreshing]")
    (kubed-ext--with-environment (plist-get scope :environment)
      (kubed-ext--pods-request
       buffer generation (append (list kubectl "get" "pods" "-o" "json") args)
       (lambda (output) (setq kubed-ext--pods-objects (kubed-ext--metric-items output))))
      ;; Usage is optional: without metrics-server the table still renders.
      (kubed-ext--pods-request
       buffer generation (append (list kubectl "top" "pods" "--no-headers") args)
       (lambda (output)
         (setq kubed-ext--pods-usage (kubed-ext--top-parse output scope)))))))

;;;###autoload
(defun kubed-ext-list-pods (&optional context namespace)
  "List pods of CONTEXT and NAMESPACE with usage and port-forward state.
A nil NAMESPACE lists every namespace."
  (interactive)
  (kubed-ext--resolved context namespace 'all
    (kubed-ext--list-pods-resolved context namespace)))

(defun kubed-ext--list-pods-resolved (context namespace)
  "Open the pod table of CONTEXT and NAMESPACE after scope resolution."
  (let* ((environment (or kubed-ext--environment (kubed-ext--capture-environment)))
         (buffer (get-buffer-create
                  (format "*Kubed pods [%s/%s]*" (or context "current")
                          (or namespace "all")))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'kubed-ext-pods-mode) (kubed-ext-pods-mode))
      (kubed-ext--apply-environment-to-new-buffer environment)
      (setq-local kubed-ext--pods-scope
                  (list :type "pods" :context context :namespace namespace
                        :environment environment)
                  kubed-list-type "pods" kubed-list-context context
                  kubed-list-namespace namespace)
      (kubed-ext-pods-refresh))
    (pop-to-buffer buffer)
    buffer))

(provide 'kubed-ext-pods)
;;; kubed-ext-pods.el ends here
