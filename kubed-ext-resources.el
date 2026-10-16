;;; kubed-ext-resources.el --- Scoped async resource views -*- lexical-binding: t; -*-
;;; Commentary:
;; Enhanced lists own their environment and per-buffer presentation state.
;; Discovery fetches only plural/group/kind/scope, never schemas or Secret data.
;; Workflow adapters: --current-scope returns the list scope; selected-scope
;; returns the row scope with exact :type/:name/:context/:namespace and captured
;; :environment.  All-namespace row IDs are namespace/name, not action names.
;;; Code:
(require 'tabulated-list)
(require 'bookmark)
(require 'json)
(require 'seq)
(require 'kubed-ext-core)
(cl-defstruct (kubed-ext-resource-row
               (:constructor kubed-ext-resource-row (type context namespace name cells)))
  type context namespace name cells)
(defvar-local kubed-ext--view-generation 0 "Monotonic request generation.")
(defvar-local kubed-ext--view-scope nil "Immutable list scope.")
(defvar-local kubed-ext--view-request nil "Owned list request.")
(defvar-local kubed-ext--view-rows nil "Retained unfiltered server cells.")
(defvar-local kubed-ext--view-headers nil "Server column names.")
(defvar-local kubed-ext--view-filter "" "Literal case-sensitive filter.")
(defvar-local kubed-ext--view-show-all nil "Explicitly expanded display.")
(defvar-local kubed-ext--view-marks nil "Marked row IDs.")
(defvar-local kubed-ext--view-identities nil "Row ID to exact resource scope.")
(defvar-local kubed-ext--view-pending nil "Hidden-view response awaiting rendering.")
(defvar-local kubed-ext--view-bookmark-row nil "Row ID awaiting bookmark restoration.")
(defvar-local kubed-ext--refresh-timer nil "Owned auto-refresh timer.")
(defvar kubed-ext--crd-registry (make-hash-table :test 'equal)
  "Environment/context/type-keyed compact metadata, never executable definitions.")
(defvar kubed-ext--crd-discovery-state (make-hash-table :test 'equal)
  "Environment/context-keyed discovery generations and requests.")
(defvar kubed-ext--discovery-generation 0 "Monotonic discovery serial.")
(defvar-local kubed-ext--view-parse-timer nil "Owned incremental parsing timer.")
(defvar-local kubed-ext--view-truncated nil "Response exceeds retained row limit.")
(defcustom kubed-ext-resource-retain-limit 5000
  "Positive maximum retained resource rows, including hidden rows.
Show-more never exceeds this bound; narrow namespace or selector for more."
  :type 'integer :group 'kubed-ext)
(defcustom kubed-ext-resource-parse-batch-size 500
  "Positive number of server rows processed per timer turn."
  :type 'integer :group 'kubed-ext)
(defcustom kubed-ext-resource-display-limit 1000
  "Positive maximum visible rows until explicit show-more."
  :type 'integer :group 'kubed-ext)
(defvaralias 'kubed-ext-auto-discover-crds 'kubed-ext-crd-discovery-enabled)
(defcustom kubed-ext-crd-discovery-enabled t
  "Discover compact CRD metadata asynchronously when opening enhanced views."
  :type 'boolean :group 'kubed-ext)
(defcustom kubed-ext-crd-cache-directory
  (expand-file-name "kubed-ext/crds/" user-emacs-directory)
  "Local directory for non-executable environment/context-keyed JSON metadata."
  :type 'directory :group 'kubed-ext)
(defcustom kubed-ext-crd-cache-max-bytes (* 4 1024 1024)
  "Positive maximum size of a compact JSON discovery cache."
  :type 'integer :group 'kubed-ext)
(defcustom kubed-ext-auto-refresh-interval 15
  "Positive interval in seconds for visible enhanced views."
  :type 'number :group 'kubed-ext)
(defface kubed-ext-resource-ready '((t :inherit success)) "Healthy status cells." :group 'kubed-ext)
(defface kubed-ext-resource-pending '((t :inherit warning)) "Pending status cells." :group 'kubed-ext)
(defface kubed-ext-resource-error '((t :inherit error)) "Failed status cells." :group 'kubed-ext)
(defconst kubed-ext--crd-discovery-jsonpath
  "jsonpath={range .items[*]}{.spec.names.plural}{\"\\t\"}{.spec.group}{\"\\t\"}{.spec.names.kind}{\"\\t\"}{.spec.scope}{\"\\n\"}{end}"
  "Compact CRD discovery output, deliberately excluding schemas.")

(defun kubed-ext--environment-key (&optional environment)
  "Return stable cache identity for ENVIRONMENT, defaulting to current buffer."
  (let ((snapshot (or environment kubed-ext--environment
                      (kubed-ext--capture-environment))))
    (list (plist-get snapshot :default-directory)
          (plist-get snapshot :kubectl) (plist-get snapshot :eenv))))

(defun kubed-ext--crd-context-key (context &optional environment)
  "Return ENVIRONMENT/CONTEXT discovery identity."
  (list (kubed-ext--environment-key environment) context))

(defun kubed-ext--crd-key (context type &optional environment)
  "Return ENVIRONMENT/CONTEXT/TYPE registry identity."
  (append (kubed-ext--crd-context-key context environment) (list type)))

(defun kubed-ext--valid-crd-p (metadata)
  "Return non-nil for validated compact CRD METADATA."
  (let ((type (plist-get metadata :type)) (plural (plist-get metadata :plural))
        (group (plist-get metadata :group)) (kind (plist-get metadata :kind)))
    (and (stringp type) (string-match-p "\\`[a-z0-9][a-z0-9.-]*\\'" type)
         (stringp plural) (string-match-p "\\`[a-z][a-z0-9-]*\\'" plural)
         (stringp group) (string-match-p "\\`[a-z0-9][a-z0-9.-]*\\'" group)
         (stringp kind) (string-match-p "\\`[A-Za-z][A-Za-z0-9]*\\'" kind)
         (equal type (concat plural "." group))
         (plist-member metadata :namespaced)
         (memq (plist-get metadata :namespaced) '(nil t)))))

(defun kubed-ext-register-crd (metadata)
  "Register validated compact METADATA in its environment and context.
Return the fully-qualified resource type, or nil for invalid metadata."
  (when (kubed-ext--valid-crd-p metadata)
    (let ((type (plist-get metadata :type)))
      (puthash (kubed-ext--crd-key (plist-get metadata :context) type
                                 (plist-get metadata :environment))
               (copy-tree metadata t) kubed-ext--crd-registry)
      type)))

(defun kubed-ext-crd-metadata (type &optional context environment)
  "Return compact TYPE metadata in CONTEXT and ENVIRONMENT."
  (gethash (kubed-ext--crd-key context type environment) kubed-ext--crd-registry))

(defun kubed-ext--crd-metadata-by-alias (base context)
  "Return registered metadata in CONTEXT whose plural or kind matches BASE.
One pass over the registry; alias lookup used to be quadratic in CRD count."
  (let ((key (kubed-ext--crd-context-key context)) found)
    (maphash (lambda (entry-key metadata)
               (when (and (not found) (equal (seq-take entry-key 2) key)
                          (or (equal base (plist-get metadata :plural))
                              (equal base (downcase (or (plist-get metadata :kind) "")))))
                 (setq found metadata)))
             kubed-ext--crd-registry)
    found))

(defun kubed-ext--resource-namespaced-p (type &optional context)
  "Return t, nil (cluster scope), or `unknown' for TYPE in CONTEXT.
Canonicalize built-in singular, plural, short and fully qualified aliases."
  (let* ((base (car (split-string (downcase type) "\\.")))
         (metadata (or (kubed-ext-crd-metadata type context)
                       (kubed-ext--crd-metadata-by-alias base context))))
    (cond (metadata (plist-get metadata :namespaced))
          ((member base '("node" "nodes" "no" "namespace" "namespaces" "ns"
                          "persistentvolume" "persistentvolumes" "pv"
                          "clusterrole" "clusterroles" "clusterrolebinding" "clusterrolebindings"
                          "storageclass" "storageclasses" "sc" "ingressclass" "ingressclasses"
                          "customresourcedefinition" "customresourcedefinitions" "crd" "crds"
                          "priorityclass" "priorityclasses" "runtimeclass" "runtimeclasses"
                          "csidriver" "csidrivers" "csinode" "csinodes"
                          "volumeattachment" "volumeattachments"
                          "mutatingwebhookconfiguration" "mutatingwebhookconfigurations"
                          "validatingwebhookconfiguration" "validatingwebhookconfigurations")) nil)
          ((member base '("pod" "pods" "po" "deployment" "deployments" "deploy"
                          "service" "services" "svc" "job" "jobs" "cronjob" "cronjobs"
                          "configmap" "configmaps" "cm" "secret" "secrets"
                          "ingress" "ingresses" "ing" "daemonset" "daemonsets" "ds"
                          "statefulset" "statefulsets" "sts" "replicaset" "replicasets" "rs"
                          "persistentvolumeclaim" "persistentvolumeclaims" "pvc"
                          "serviceaccount" "serviceaccounts" "sa" "role" "roles"
                          "rolebinding" "rolebindings" "event" "events" "ev"
                          "networkpolicy" "networkpolicies" "resourcequota" "resourcequotas"
                          "limitrange" "limitranges" "limits" "horizontalpodautoscaler" "horizontalpodautoscalers" "hpa"
                          "replicationcontroller" "replicationcontrollers" "rc" "netpol" "quota"
                          "endpoints" "ep" "endpointslice" "endpointslices" "pdb" "poddisruptionbudgets")) t)
          (t 'unknown))))

(defun kubed-ext--parse-crd-compact-list (output)
  "Parse validated tab-delimited compact discovery OUTPUT into metadata.
Malformed rows fail atomically; empty output means an empty registry."
  (let (entries seen)
    (dolist (line (split-string output "\n" t))
      (let* ((fields (split-string (string-remove-suffix "\r" line) "\t"))
             (plural (nth 0 fields)) (group (nth 1 fields))
             (kind (nth 2 fields)) (scope (nth 3 fields))
             (metadata (list :type (concat plural "." group)
                             :plural plural :group group :kind kind
                             :namespaced (equal scope "Namespaced"))))
        (unless (and (= (length fields) 4)
                     (member scope '("Namespaced" "Cluster"))
                     (kubed-ext--valid-crd-p metadata)
                     (not (member (plist-get metadata :type) seen)))
          (error "Malformed compact CRD discovery row"))
        (push (plist-get metadata :type) seen)
        (push metadata entries)))
    (nreverse entries)))

(defun kubed-ext--replace-crd-entries (context environment entries)
  "Atomically replace only CONTEXT/ENVIRONMENT registry with validated ENTRIES."
  (unless (cl-every #'kubed-ext--valid-crd-p entries)
    (error "Invalid CRD registry entries"))
  (let ((key (kubed-ext--crd-context-key context environment)) remove)
    (maphash (lambda (entry-key _value)
               (when (equal (seq-take entry-key 2) key) (push entry-key remove)))
             kubed-ext--crd-registry)
    (dolist (entry-key remove) (remhash entry-key kubed-ext--crd-registry))
    (dolist (entry entries)
      (kubed-ext-register-crd (append (list :context context :environment environment) entry)))))

(defun kubed-ext--crd-cache-file (context environment)
  "Return a local digest-keyed JSON cache filename for CONTEXT/ENVIRONMENT."
  (when (file-remote-p kubed-ext-crd-cache-directory)
    (error "CRD cache directory must be local"))
  (expand-file-name
   (concat (secure-hash 'sha256
                        (let ((print-length nil) (print-level nil))
                          (prin1-to-string (kubed-ext--crd-context-key context environment))))
           ".json") kubed-ext-crd-cache-directory))

(defun kubed-ext--write-crd-cache (context environment entries)
  "Atomically write compact ENTRIES for CONTEXT and ENVIRONMENT.
Disk failure never invalidates live discovery."
  (let (temporary)
    (condition-case err
        (let* ((file (kubed-ext--crd-cache-file context environment))
               (json-encoding-pretty-print nil)
               (data (json-encode
                      `((version . 1) (entries . ,(vconcat
                          (mapcar (lambda (entry)
                                    `((type . ,(plist-get entry :type))
                                      (plural . ,(plist-get entry :plural))
                                      (group . ,(plist-get entry :group))
                                      (kind . ,(plist-get entry :kind))
                                      (namespaced . ,(if (plist-get entry :namespaced) t :json-false))))
                                  entries)))))))
          (unless (and (integerp kubed-ext-crd-cache-max-bytes)
                       (> kubed-ext-crd-cache-max-bytes 0)
                       (<= (string-bytes data) kubed-ext-crd-cache-max-bytes))
            (error "CRD cache exceeds configured byte limit"))
          (make-directory (file-name-directory file) t)
          (setq temporary (make-temp-file (expand-file-name ".crds-" (file-name-directory file))))
          (set-file-modes temporary #o600)
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region data nil temporary nil 'silent))
          (rename-file temporary file t)
          (setq temporary nil))
      (error (message "Kubed-ext CRD cache write failed: %s" (error-message-string err))))
    (when (and temporary (file-exists-p temporary)) (delete-file temporary))))

(defun kubed-ext--load-crd-cache (context environment)
  "Load bounded validated JSON metadata for CONTEXT/ENVIRONMENT, never Lisp.
Return entry count (including zero), or nil when absent or invalid."
  (condition-case err
      (let ((file (kubed-ext--crd-cache-file context environment)))
        (when (file-readable-p file)
          (unless (and (integerp kubed-ext-crd-cache-max-bytes)
                       (> kubed-ext-crd-cache-max-bytes 0)
                       (<= (file-attribute-size (file-attributes file)) kubed-ext-crd-cache-max-bytes))
            (error "CRD cache exceeds configured byte limit"))
          (let* ((object (with-temp-buffer
                           (insert-file-contents file)
                           (json-parse-buffer :object-type 'alist :array-type 'list
                                              :false-object :false :null-object :null)))
                 (entries (mapcar
                           (lambda (entry)
                             (let ((scope (alist-get 'namespaced entry)))
                               (unless (memq scope '(t :false)) (error "Invalid cached scope"))
                               (list :type (alist-get 'type entry) :plural (alist-get 'plural entry)
                                     :group (alist-get 'group entry) :kind (alist-get 'kind entry)
                                     :namespaced (eq scope t))))
                           (alist-get 'entries object))))
            (unless (and (equal (alist-get 'version object) 1)
                         (listp (alist-get 'entries object))) (error "Invalid cache version"))
            (kubed-ext--replace-crd-entries context environment entries)
            (length entries))))
    (error (message "Kubed-ext ignored CRD cache: %s" (error-message-string err)) nil)))

(defvar kubed-ext--crd-cache-loaded (make-hash-table :test 'equal)
  "Context keys whose on-disk metadata has already been read this session.")

(defun kubed-ext--ensure-crd-cache (context environment)
  "Load the on-disk cache for CONTEXT/ENVIRONMENT at most once per session.
Opening a view used to reread and reparse the whole JSON cache every time."
  (let ((key (kubed-ext--crd-context-key context environment)))
    (unless (gethash key kubed-ext--crd-cache-loaded)
      (puthash key t kubed-ext--crd-cache-loaded)
      (kubed-ext--load-crd-cache context environment))))

(defun kubed-ext-discover-crds-async (&optional context force callback)
  "Discover compact CRDs for CONTEXT; return a request, or existing request.
FORCE supersedes inflight discovery.  CALLBACK receives the number registered
on success/cache hit.  RBAC failure leaves cached metadata and explicit FQ
resource lists usable.  No global Kubed resource definitions are generated."
  (when kubed-ext-crd-discovery-enabled
    (if context
        (kubed-ext--discover-crds-resolved context force callback)
      (kubed-ext--resolve-scope
       (list :context nil) 'all
       (lambda (resolved)
         (kubed-ext--discover-crds-resolved (plist-get resolved :context) force callback))))))
(defun kubed-ext--discover-crds-resolved (context force callback)
  "Discover compact metadata after explicit CONTEXT resolution.
FORCE supersedes inflight discovery; CALLBACK receives the registered count."
  (let* ((environment (or kubed-ext--environment (kubed-ext--capture-environment)))
           (key (kubed-ext--crd-context-key context environment))
           (state (gethash key kubed-ext--crd-discovery-state)))
      (cond
       ((and (not force) (eq (plist-get state :status) 'inflight))
        (when callback (plist-put state :callbacks (append (plist-get state :callbacks) (list callback))))
        (plist-get state :request))
       ((and (not force) (eq (plist-get state :status) 'done))
        (when callback (funcall callback (plist-get state :count))) nil)
       (t
        (unless force (kubed-ext--ensure-crd-cache context environment))
        (when (plist-get state :request) (kubed-ext-cancel-request (plist-get state :request)))
        (let* ((generation (cl-incf kubed-ext--discovery-generation))
               (new-state (list :status 'inflight :generation generation :callbacks (when callback (list callback)))))
          (puthash key new-state kubed-ext--crd-discovery-state)
          (condition-case err
              (let ((request
                     (kubed-ext--async-kubectl
                      (append (list (plist-get environment :kubectl) "get"
                                    "customresourcedefinitions.apiextensions.k8s.io"
                                    "-o" kubed-ext--crd-discovery-jsonpath)
                              (kubed-ext--scope-args (list :context context)))
                      (lambda (output)
                        (when (eq new-state (gethash key kubed-ext--crd-discovery-state))
                          (condition-case parse-error
                              (let ((entries (kubed-ext--parse-crd-compact-list output)))
                                (kubed-ext--replace-crd-entries context environment entries)
                                (kubed-ext--write-crd-cache context environment entries)
                                (plist-put new-state :status 'done)
                                (plist-put new-state :count (length entries))
                                (dolist (fn (plist-get new-state :callbacks))
                                  (condition-case callback-error (funcall fn (length entries))
                                    (error (message "Kubed CRD callback: %s" callback-error)))))
                            (error
                             (plist-put new-state :status 'failed)
                             (message "Kubed CRD discovery parse failed: %s" (error-message-string parse-error))))))
                      (lambda (failure)
                        (when (eq new-state (gethash key kubed-ext--crd-discovery-state))
                          (plist-put new-state :status 'failed)
                          (message "Kubed CRD discovery unavailable; enter fully-qualified resource: %s" failure))))))
                (plist-put new-state :request request)
                request)
            (error (plist-put new-state :status 'failed)
                   (message "Kubed CRD discovery could not start: %s" (error-message-string err)) nil)))))))

(defun kubed-ext-crd-resource-choices (&optional context)
  "Return sorted discovered FQ resource names for this environment/CONTEXT."
  (let ((key (kubed-ext--crd-context-key context)) choices)
    (maphash (lambda (entry-key metadata)
               (when (equal (seq-take entry-key 2) key) (push (plist-get metadata :type) choices)))
             kubed-ext--crd-registry)
    (sort choices #'string<)))
(defun kubed-ext-warm-crd-cache (&optional context)
  "Warm compact CRD cache asynchronously for CONTEXT."
  (interactive) (kubed-ext-discover-crds-async context))
(defun kubed-ext-refresh-crd-columns ()
  "Rediscover compact metadata and refresh current server-table columns."
  (interactive)
  (let ((scope (kubed-ext--current-scope)))
    (kubed-ext-discover-crds-async (plist-get scope :context) t)
    (when (derived-mode-p 'kubed-ext-resources-mode) (kubed-ext-refresh-resources))))

(defun kubed-ext--untabify-string (text)
  "Expand literal tabs in TEXT with fixed tab stops, in one pass.
Kubectl normally emits spaces, so this is a cheap no-op for ordinary output."
  (if (not (string-search "\t" text))
      text
    (with-temp-buffer
      (insert text)
      (untabify (point-min) (point-max))
      (buffer-string))))

(defun kubed-ext--table-columns (header)
  "Return ((START . NAME) ...) column boundaries for an untabified HEADER line.
Tabwriter separates columns by at least two spaces, so a single space belongs
to a header such as METADATA STATE.  Reject headers without NAME rather than
expose ambiguous action targets."
  (let (matches (start 0))
    (while (string-match "[^[:space:]]+\\(?: [^[:space:]]+\\)*" header start)
      (push (cons (match-beginning 0) (match-string 0 header)) matches)
      (setq start (match-end 0)))
    (setq matches (nreverse matches))
    (unless (cl-find "NAME" matches :key #'cdr :test #'equal)
      (error "Server table has no NAME column"))
    matches))

(defun kubed-ext--split-table-row (line columns)
  "Split untabified LINE into cells at COLUMNS boundaries."
  (let ((length (length line)))
    (cl-loop for tail on columns
             for from = (caar tail)
             for to = (if (cdr tail) (caadr tail) length)
             collect (if (< from length)
                         (string-trim (substring line from (min to length)))
                       ""))))

(defun kubed-ext--parse-server-table (text)
  "Parse TEXT by fixed header character boundaries, never row whitespace.
Kubectl's tabwriter aligns UTF-8 decoded character counts.  Empty interior
cells, Unicode and multiword values remain intact.  Expand tabs before slicing."
  (let* ((lines (split-string (kubed-ext--untabify-string
                               (if (string-search "\r" (or text ""))
                                   (replace-regexp-in-string "\r" "" text)
                                 (or text "")))
                              "\n" t))
         (header (car lines)))
    (cond ((or (null header) (string-match-p "\\`[[:space:]]*No resources found" header)) nil)
          (t
           (let ((columns (kubed-ext--table-columns header)))
             (cons (mapcar #'cdr columns)
                   (mapcar (lambda (line) (kubed-ext--split-table-row line columns))
                           (cdr lines))))))))

(defun kubed-ext--parse-table-async (text buffer generation callback)
  "Parse bounded TEXT in batches for BUFFER/GENERATION, calling CALLBACK.
Retain at most `kubed-ext-resource-retain-limit' rows and expose truncation.
Column boundaries are computed once from the header, and each batch slices
lines directly, so parsing stays linear in the response size.  Batches yield
to redisplay through a zero-delay timer instead of waiting for idle time.
For a namespace-qualified all-namespace response, discard other namespaces
before applying the cap and scan to completion so the truncation state concerns
matching rows rather than unrelated ones."
  (unless (and (integerp kubed-ext-resource-retain-limit)
               (> kubed-ext-resource-retain-limit 0)
               (integerp kubed-ext-resource-parse-batch-size)
               (> kubed-ext-resource-parse-batch-size 0))
    (user-error "Retained rows and parse batch size must be positive integers"))
  (let* ((text (kubed-ext--untabify-string
                (if (string-search "\r" (or text ""))
                    (replace-regexp-in-string "\r" "" text)
                  (or text ""))))
         (size (length text))
         (end (or (string-match "\n" text) size))
         (header (substring text 0 end))
         (cursor (min size (1+ end))) (count 0) rows headers columns
         namespace-index requested-namespace scan-for-matches truncated
         (blank-header (string-match-p "\\`[[:space:]]*\\'" header))
         (empty (string-match-p "\\`[[:space:]]*No resources found" header)))
    (cl-labels
        ((step ()
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (when (= generation kubed-ext--view-generation)
                 (condition-case err
                     (progn
                       (unless columns
                         (setq columns (kubed-ext--table-columns header)
                               headers (mapcar #'cdr columns)
                               namespace-index (cl-position "NAMESPACE" headers :test #'string-equal-ignore-case)
                               requested-namespace (plist-get kubed-ext--view-scope :namespace)
                               scan-for-matches (and (not (null namespace-index)) requested-namespace)))
                       (let ((batch 0))
                         (while (and (< cursor size)
                                     (< batch kubed-ext-resource-parse-batch-size)
                                     (or scan-for-matches (< count kubed-ext-resource-retain-limit)))
                           (let* ((newline (string-match "\n" text cursor))
                                  (line (substring text cursor (or newline size))))
                             (setq cursor (min size (1+ (or newline size))))
                             (unless (string-empty-p line)
                               (let ((row (kubed-ext--split-table-row line columns)))
                                 (when (or (not scan-for-matches)
                                           (equal (nth namespace-index row) requested-namespace))
                                   (if (< count kubed-ext-resource-retain-limit)
                                       (progn (push row rows) (cl-incf count))
                                     (setq truncated t))))))
                           (cl-incf batch)))
                       (if (and (< cursor size)
                                (or scan-for-matches (< count kubed-ext-resource-retain-limit)))
                           (setq kubed-ext--view-parse-timer (run-with-timer 0 nil #'step))
                         (setq kubed-ext--view-parse-timer nil
                               kubed-ext--view-truncated (or truncated
                                                            (and (not scan-for-matches)
                                                                 (< cursor size))))
                         (funcall callback headers (nreverse rows))))
                   (error
                    (setq kubed-ext--view-parse-timer nil
                          mode-line-process (propertize " [invalid table]" 'face 'error))
                    (message "Kubed table: %s" (error-message-string err)))))))))
      (if (or blank-header empty)
          ;; An empty response is not a parse failure; render an empty table.
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (when (= generation kubed-ext--view-generation)
                (setq kubed-ext--view-parse-timer nil kubed-ext--view-truncated nil)
                (funcall callback (or kubed-ext--view-headers '("NAME")) nil))))
        (setq kubed-ext--view-parse-timer (run-with-timer 0 nil #'step))))))

(defun kubed-ext--current-scope ()
  "Return list scope plist for enhanced or native Kubed lists, or nil.
Keys: :type, :context, :namespace, :all-namespaces, :selector, :environment.
Nil namespace implies all-namespace LIST scope only for namespaced types."
  (or kubed-ext--view-scope
      (when kubed-list-type
        (list :type kubed-list-type :context kubed-list-context
              :namespace kubed-list-namespace
              ;; Native Kubed nil namespace uses the context default, not -A.
              :all-namespaces (and (not kubed-list-namespace)
                                   (cl-position "NAMESPACE" (mapcar #'car (append tabulated-list-format nil))
                                                :test #'string-equal-ignore-case))
              :environment (kubed-ext--capture-environment)))))

(defun kubed-ext--cell-scope (headers cells scope)
  "Derive unambiguous row scope from HEADERS/CELLS and list SCOPE."
  (let* ((name-index (cl-position "NAME" headers :test #'string-equal-ignore-case))
         (namespace-index (cl-position "NAMESPACE" headers :test #'string-equal-ignore-case))
         (name (and name-index (nth name-index cells)))
         (namespace (if namespace-index (nth namespace-index cells) (plist-get scope :namespace))))
    (unless (and (stringp name) (not (string-empty-p name))
                 (or (not (plist-get scope :all-namespaces)) namespace-index)
                 (or (not namespace-index) (and namespace (not (string-empty-p namespace)))))
      (error "Server table contains missing resource identity"))
    (append (list :name name :namespace namespace :all-namespaces nil) scope)))

(defun kubed-ext--resource-row-id (scope &optional namespace-column)
  "Return stable display identity for exact row SCOPE.
Every namespaced resource uses namespace/name, including a scoped refresh whose
server table omits NAMESPACE.  Cluster-scoped resources retain their bare name.
NAMESPACE-COLUMN preserves cold all-namespace identity before CRD metadata has
settled, preventing duplicate bare names from colliding."
  (let ((name (plist-get scope :name)) (namespace (plist-get scope :namespace)))
    (if (and namespace (or namespace-column (eq (plist-get scope :scope-kind) t)))
        (concat namespace "/" name)
      name)))

(defun kubed-ext-selected-scope ()
  "Return exact selected row scope, or nil if point is not on a resource.
Actions use :name (never namespace/name ID), :namespace, :context, :type,
and :environment.  Native Kubed tables with namespace/name IDs are adapted."
  (let* ((id (tabulated-list-get-id)) (scope (kubed-ext--current-scope)))
    (when (and id scope)
      (or (and kubed-ext--view-identities (gethash id kubed-ext--view-identities))
          (let* ((entry (tabulated-list-get-entry))
                 (headers (mapcar #'car (append tabulated-list-format nil)))
                 (cells (mapcar (lambda (cell) (if (consp cell) (car cell) cell)) (append entry nil))))
            (if (cl-position "NAME" headers :test #'string-equal-ignore-case)
                (kubed-ext--cell-scope headers cells scope)
              (when (stringp id)
                (if (and (plist-get scope :all-namespaces) (string-match "\\`\\([^/]+\\)/\\(.+\\)\\'" id))
                    (append (list :namespace (match-string 1 id) :name (match-string 2 id) :all-namespaces nil) scope)
                  (unless (plist-get scope :all-namespaces)
                    (append (list :name id :all-namespaces nil) scope))))))))))

(defun kubed-ext--resource-cell-face (header cell)
  "Return CELL with a theme status face when HEADER is a status column."
  (if (member (upcase header) '("STATUS" "STATE" "READY"))
      (let ((face (cond ((string-match-p "Failed\\|Error\\|Crash\\|BackOff\\|Terminating" cell) 'kubed-ext-resource-error)
                        ((string-match-p "Pending\\|Unknown\\|Creating" cell) 'kubed-ext-resource-pending)
                        ((string-match-p "Running\\|Ready\\|Active\\|Complete\\|True" cell) 'kubed-ext-resource-ready))))
        (if face (propertize cell 'face face) cell)) cell))

(defun kubed-ext--restore-marks ()
  "Restore tags in the current table without disturbing point."
  (when kubed-ext--view-marks
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (tabulated-list-get-id)
          (tabulated-list-put-tag (if (gethash (tabulated-list-get-id) kubed-ext--view-marks)
                                     (if (derived-mode-p 'kubed-ext-resources-mode) ">" "D") " ")))
        (forward-line 1)))))

(defcustom kubed-ext-resource-column-max-width 48
  "Positive maximum width of a rendered server-table column.
Wider values are truncated in the display only; row scopes keep full values."
  :type 'integer :group 'kubed-ext)

(defun kubed-ext--table-format (headers rows)
  "Return a `tabulated-list-format' sized to HEADERS and ROWS.
Fixed-width columns misalign every row whose value overflows them, which is
common for custom resource tables, so widths follow the widest actual value."
  (let* ((widths (mapcar #'string-width headers))
         (limit (if (and (integerp kubed-ext-resource-column-max-width)
                         (> kubed-ext-resource-column-max-width 0))
                    kubed-ext-resource-column-max-width 48)))
    (dolist (row rows)
      (cl-loop for cell in row
               for tail on widths
               do (let ((width (string-width cell)))
                    (when (> width (car tail)) (setcar tail width)))))
    (vconcat
     (cl-loop for header in headers
              for width in widths
              collect (list header
                            ;; Later columns are truncated to their width by
                            ;; `tabulated-list-print-col'; the last one is not.
                            (max 3 (min width limit))
                            t)))))

(defcustom kubed-ext-resource-hidden-columns '("NOMINATED NODE" "READINESS GATES")
  "Server-table column names to omit from enhanced lists.
Matching is case-insensitive.  NAME and NAMESPACE are never hidden, because
row identity depends on them."
  :type '(repeat string) :group 'kubed-ext)

(defun kubed-ext--visible-columns (headers rows)
  "Return (HEADERS . ROWS) without `kubed-ext-resource-hidden-columns'.
Hiding happens before rendering only, so the retained server response is
unchanged and customising the list takes effect on the next refresh."
  (let ((keep (cl-loop for header in headers
                       for index from 0
                       unless (and (not (member (upcase header) '("NAME" "NAMESPACE")))
                                   (cl-member header kubed-ext-resource-hidden-columns
                                              :test #'string-equal-ignore-case))
                       collect index)))
    (if (= (length keep) (length headers))
        (cons headers rows)
      (cons (mapcar (lambda (index) (nth index headers)) keep)
            (mapcar (lambda (row) (mapcar (lambda (index) (nth index row)) keep)) rows)))))

(defun kubed-ext--render-resource-response (generation headers rows)
  "Render HEADERS/ROWS only for current GENERATION; preserve marks and sorting."
  (when (= generation kubed-ext--view-generation)
    (unless (and (integerp kubed-ext-resource-display-limit) (> kubed-ext-resource-display-limit 0))
      (user-error "Resource display limit must be a positive integer"))
    (pcase-let ((`(,visible-headers . ,visible-rows) (kubed-ext--visible-columns headers rows)))
      (setq headers visible-headers rows visible-rows))
    ;; Unknown resource scope is settled by the canonical server table, not
    ;; guessed before discovery.  -A on cluster resources is harmless.
    (when (and (eq (plist-get kubed-ext--view-scope :scope-kind) 'unknown) headers)
      (let ((namespaced (not (null (cl-position "NAMESPACE" headers :test #'string-equal-ignore-case)))))
        (setq kubed-ext--view-scope
              (plist-put (copy-sequence kubed-ext--view-scope) :all-namespaces
                         (and namespaced (not (plist-get kubed-ext--view-scope :namespace)))))
        (when (and namespaced (plist-get kubed-ext--view-scope :namespace))
          (let ((index (cl-position "NAMESPACE" headers :test #'string-equal-ignore-case))
                (namespace (plist-get kubed-ext--view-scope :namespace)))
            (setq rows (cl-remove-if-not (lambda (row) (equal (nth index row) namespace)) rows))))
        (unless namespaced
          (setq kubed-ext--view-scope (plist-put kubed-ext--view-scope :namespace nil)))
        (setq kubed-ext--view-scope (plist-put kubed-ext--view-scope :scope-kind namespaced))))
    (let ((identities (make-hash-table :test 'equal)) entries)
      ;; Validate all retained rows before changing the visible buffer.
      (dolist (cells rows)
        (unless (= (length cells) (length headers)) (error "Malformed server table cells"))
        (let* ((scope (kubed-ext--cell-scope headers cells kubed-ext--view-scope))
               (id (kubed-ext--resource-row-id
                    scope (cl-position "NAMESPACE" headers :test #'string-equal-ignore-case))))
          (when (gethash id identities) (error "Duplicate server table resource identity"))
          (puthash id scope identities)
          (when (or (string-empty-p kubed-ext--view-filter)
                    (string-match-p (regexp-quote kubed-ext--view-filter) (mapconcat #'identity cells " ")))
            (push (list id (vconcat (cl-mapcar #'kubed-ext--resource-cell-face headers cells))) entries))))
      (setq entries (nreverse entries))
      (setq-local kubed-ext--view-identities identities
                  kubed-ext--view-headers headers kubed-ext--view-rows rows)
      (setq tabulated-list-format (kubed-ext--table-format headers rows))
      (unless (assoc (car tabulated-list-sort-key) (append tabulated-list-format nil))
        (setq tabulated-list-sort-key nil))
      (let ((total (length entries)))
        (setq tabulated-list-entries (if kubed-ext--view-show-all entries
                                      (seq-take entries kubed-ext-resource-display-limit)))
        (setq-local mode-line-process
                    (if (> total (length tabulated-list-entries))
                        (propertize (format " [%d/%d rows; + show more]" (length tabulated-list-entries) total)
                                    'face 'warning)
                      (format " [%d rows]" total))))
      (when kubed-ext--view-truncated
        (setq mode-line-process (concat mode-line-process
                                        (format " [retained limit %d; narrow query]" kubed-ext-resource-retain-limit))))
      (tabulated-list-init-header)
      ;; The header line belongs to the column names: anything prepended to it
      ;; shifts every name away from the column it labels.  Scope lives in the
      ;; mode line instead, next to the row counts.
      (setq-local mode-line-buffer-identification
                  (list (kubed-ext--scope-label kubed-ext--view-scope)))
      (tabulated-list-print t)
      (kubed-ext--restore-marks)
      (when kubed-ext--view-bookmark-row
        (goto-char (point-min))
        (while (and (not (eobp))
                    (not (equal kubed-ext--view-bookmark-row (tabulated-list-get-id))))
          (forward-line 1))
        (when (eobp) (goto-char (point-min)))
        (setq kubed-ext--view-bookmark-row nil))
      t)))

(defun kubed-ext-resource-show-more ()
  "Explicitly render all retained rows, exceeding the default 1000-row bound."
  (interactive) (setq-local kubed-ext--view-show-all t)
  (kubed-ext--render-resource-response kubed-ext--view-generation kubed-ext--view-headers kubed-ext--view-rows))
(defun kubed-ext-resource-set-filter (filter)
  "Set literal FILTER and rerender without changing retained marks."
  (interactive "sLiteral filter: ")
  (setq-local kubed-ext--view-filter filter)
  (kubed-ext--render-resource-response kubed-ext--view-generation kubed-ext--view-headers kubed-ext--view-rows))
(defalias 'kubed-ext-set-filter #'kubed-ext-resource-set-filter)

(defun kubed-ext-marked-items ()
  "Return marked row IDs in stable order (namespace/name for all-namespace lists)."
  (if (derived-mode-p 'kubed-ext-resources-mode)
      (when kubed-ext--view-marks
        (sort (hash-table-keys kubed-ext--view-marks) #'string<))
    (let (ids)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when (and (memq (char-after) '(?D ?* ?>)) (tabulated-list-get-id))
            (push (tabulated-list-get-id) ids))
          (forward-line 1)))
      (nreverse ids))))
(defun kubed-ext--set-mark (marked)
  "Set selected row mark to MARKED, then advance point."
  (let ((id (tabulated-list-get-id)))
    (when id
      (unless kubed-ext--view-marks (setq-local kubed-ext--view-marks (make-hash-table :test 'equal)))
      (if marked (puthash id t kubed-ext--view-marks) (remhash id kubed-ext--view-marks))
      (tabulated-list-put-tag (if marked (if (derived-mode-p 'kubed-ext-resources-mode) ">" "D") " ") t))))
(defun kubed-ext-mark-item () "Mark selected resource and advance." (interactive) (kubed-ext--set-mark t))
(defun kubed-ext-unmark-item () "Unmark selected resource and advance." (interactive) (kubed-ext--set-mark nil))
(defun kubed-ext-resource-toggle-mark ()
  "Toggle selected resource mark and advance."
  (interactive) (kubed-ext--set-mark (not (and kubed-ext--view-marks (gethash (tabulated-list-get-id) kubed-ext--view-marks)))))
(defun kubed-ext-mark-all ()
  "Mark all currently visible resources; hidden rows are not implicitly marked."
  (interactive)
  (unless kubed-ext--view-marks (setq-local kubed-ext--view-marks (make-hash-table :test 'equal)))
  (dolist (entry tabulated-list-entries) (puthash (car entry) t kubed-ext--view-marks))
  (kubed-ext--restore-marks))
(defun kubed-ext-unmark-all ()
  "Clear all enhanced and native resource marks."
  (interactive) (when kubed-ext--view-marks (clrhash kubed-ext--view-marks))
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (tabulated-list-get-id) (tabulated-list-put-tag " "))
      (forward-line 1))))
(defun kubed-ext-jump-to-next-highlight ()
  "Jump to next literal filter match in the rendered table."
  (interactive)
  (unless (string-empty-p kubed-ext--view-filter)
    (forward-char (if (eobp) 0 1))
    (unless (search-forward kubed-ext--view-filter nil t)
      (goto-char (point-min)) (search-forward kubed-ext--view-filter nil t))))
(defun kubed-ext-jump-to-previous-highlight ()
  "Jump to previous literal filter match in the rendered table."
  (interactive)
  (unless (string-empty-p kubed-ext--view-filter)
    (unless (search-backward kubed-ext--view-filter nil t)
      (goto-char (point-max)) (search-backward kubed-ext--view-filter nil t))))

(defcustom kubed-ext-resource-wide-output t
  "Request `kubectl -o wide' columns in enhanced resource lists.
Wide output is a superset of the default server columns: pods gain NODE and
IP alongside AGE, and nothing is lost for types that print the same table.
Toggle per view with `kubed-ext-toggle-wide-output'."
  :type 'boolean :group 'kubed-ext)

(defvar-local kubed-ext--view-wide nil
  "Per-view wide-output state, initialised from `kubed-ext-resource-wide-output'.")

(defun kubed-ext--resource-list-argv (scope)
  "Build FULL server-table argv for SCOPE with explicit -A intent."
  (append (list (or (plist-get (plist-get scope :environment) :kubectl)
                   (kubed-ext--kubectl-program)) "get" (plist-get scope :type))
          (kubed-ext--scope-args
           (if (eq (plist-get scope :scope-kind) 'unknown)
               (append '(:namespace nil :all-namespaces t) scope) scope))
          (when (plist-get scope :selector) (list (concat "--selector=" (plist-get scope :selector))))
          (when (plist-get scope :wide) '("-o" "wide"))
          '("--server-print=true")))

(defun kubed-ext-toggle-wide-output ()
  "Toggle `kubectl -o wide' columns in this view and refresh it.
Pods then show NODE and IP; ingresses keep HOSTS, ADDRESS and PORTS."
  (interactive)
  (unless kubed-ext--view-scope (user-error "Not an enhanced Kubed resource view"))
  (setq-local kubed-ext--view-wide (not kubed-ext--view-wide)
              kubed-ext--view-scope (plist-put (copy-sequence kubed-ext--view-scope)
                                               :wide kubed-ext--view-wide))
  ;; Column sets differ between the two outputs, so drop a now-absent sort key.
  (setq tabulated-list-sort-key nil)
  (message "Wide columns %s" (if kubed-ext--view-wide "on" "off"))
  (kubed-ext-refresh-resources))

(defun kubed-ext--view-cleanup ()
  "Stop only this view's requests and timers and invalidate pending replies."
  (cl-incf kubed-ext--view-generation)
  (when (timerp kubed-ext--view-parse-timer) (cancel-timer kubed-ext--view-parse-timer))
  (setq kubed-ext--view-parse-timer nil)
  (when (timerp kubed-ext--refresh-timer) (cancel-timer kubed-ext--refresh-timer))
  (setq kubed-ext--refresh-timer nil kubed-ext--view-pending nil)
  (when kubed-ext--view-request (kubed-ext-cancel-request kubed-ext--view-request))
  (setq kubed-ext--view-request nil))

(defun kubed-ext--render-pending-view (&rest _ignored)
  "Render a pending response only after this view becomes visible."
  (when (and kubed-ext--view-pending (get-buffer-window (current-buffer) t))
    (pcase-let ((`(,generation ,headers ,rows) kubed-ext--view-pending))
      (setq kubed-ext--view-pending nil)
      (kubed-ext--render-resource-response generation headers rows))))

(define-derived-mode kubed-ext-resources-mode tabulated-list-mode "Kubed-Resources"
  "Async server-table view with exact scoped row identities."
  (setq tabulated-list-padding 2 tabulated-list-use-header-line t)
  (setq-local kubed-ext--view-marks (make-hash-table :test 'equal))
  (setq-local bookmark-make-record-function #'kubed-ext--bookmark-record)
  (add-hook 'kill-buffer-hook #'kubed-ext--view-cleanup nil t)
  (add-hook 'change-major-mode-hook #'kubed-ext--view-cleanup nil t)
  (add-hook 'window-buffer-change-functions #'kubed-ext--render-pending-view nil t))
(dolist (binding '(("m" . kubed-ext-mark-item) ("u" . kubed-ext-unmark-item)
                   ("M" . kubed-ext-mark-all) ("U" . kubed-ext-unmark-all)
                   ("g" . kubed-ext-refresh-resources) ("/" . kubed-ext-set-filter)
                   ("w" . kubed-ext-toggle-wide-output)
                   ("+" . kubed-ext-resource-show-more)))
  (define-key kubed-ext-resources-mode-map (kbd (car binding)) (cdr binding)))

(defun kubed-ext-list-resources (type &optional context namespace selector)
  "Resolve effective CONTEXT once, then open TYPE in NAMESPACE with SELECTOR."
  (interactive (list (completing-read "Resource (FQ name allowed): "
                                     (append '("pods" "deployments" "services" "jobs" "nodes")
                                             (kubed-ext-crd-resource-choices)) nil nil)))
  (kubed-ext--resolved context namespace 'all
    (kubed-ext--list-resources-resolved type context namespace selector)))

(defun kubed-ext--list-resources-resolved (type &optional context namespace selector)
  "Open an enhanced async TYPE list in CONTEXT/NAMESPACE with SELECTOR.
Nil NAMESPACE requests -A for namespaced types.  Buffer identity includes the
captured execution environment and every argument.  Reopening preserves
marks/filter/sort and monotonically advances generation, never resets it.
An explicit fully-qualified TYPE works without CRD discovery permission."
  (interactive (list (completing-read "Resource (FQ name allowed): "
                                     (append '("pods" "deployments" "services" "jobs" "nodes")
                                             (kubed-ext-crd-resource-choices)) nil nil)))
  (unless (and (stringp type) (string-match-p "\\`[a-zA-Z0-9][a-zA-Z0-9.-]*\\'" type))
    (user-error "Invalid resource type"))
  (let* ((environment (or kubed-ext--environment (kubed-ext--capture-environment)))
         (_cache (kubed-ext--ensure-crd-cache context environment))
         (scope-kind (kubed-ext--resource-namespaced-p type context))
         (namespace (and scope-kind namespace))
         (scope (list :type type :context context :namespace namespace :selector selector
                      :scope-kind scope-kind
                      :all-namespaces (and (not namespace) (not (null scope-kind)))
                      :environment environment))
         (identity (list (kubed-ext--environment-key environment) context namespace type selector))
         (buffer (get-buffer-create
                  (format "*Kubed %s [%s/%s] %s*" type (or context "current")
                          (or namespace "all") (substring (secure-hash 'sha256
                            (let ((print-length nil) (print-level nil)) (prin1-to-string identity))) 0 16)))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'kubed-ext-resources-mode) (kubed-ext-resources-mode))
      (kubed-ext--apply-environment-to-new-buffer environment)
      ;; Reopening a view keeps whatever wide setting it was toggled to.
      (setq-local kubed-ext--view-wide
                  (if (local-variable-p 'kubed-ext--view-wide)
                      kubed-ext--view-wide kubed-ext-resource-wide-output))
      (setq-local kubed-ext--view-scope (plist-put scope :wide kubed-ext--view-wide)
                  kubed-list-type type kubed-list-context context kubed-list-namespace namespace)
      (kubed-ext-discover-crds-async context)
      ;; Warm navigation completion while the list request is already in
      ;; flight, so the next context/namespace prompt has candidates.
      (kubed-ext-contexts-async environment)
      (kubed-ext-namespaces-async context environment)
      (kubed-ext-refresh-resources))
    (pop-to-buffer buffer)
    buffer))

(defun kubed-ext-refresh-resources ()
  "Refresh this enhanced view asynchronously, suppressing stale responses."
  (interactive)
  (unless kubed-ext--view-scope (user-error "Not an enhanced Kubed resource view"))
  (cl-incf kubed-ext--view-generation)
  (when (timerp kubed-ext--view-parse-timer) (cancel-timer kubed-ext--view-parse-timer))
  (setq kubed-ext--view-parse-timer nil)
  (when kubed-ext--view-request (kubed-ext-cancel-request kubed-ext--view-request))
  (let ((generation kubed-ext--view-generation) (buffer (current-buffer))
        (scope kubed-ext--view-scope))
    (setq-local mode-line-process " [refreshing]")
    (kubed-ext--with-environment (plist-get scope :environment)
      (setq-local kubed-ext--view-request
                  (kubed-ext--async-kubectl
                   (kubed-ext--resource-list-argv scope)
                   (lambda (output)
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (when (and (= generation kubed-ext--view-generation) (equal scope kubed-ext--view-scope))
                           (setq kubed-ext--view-request nil)
                           (condition-case err
                               (kubed-ext--parse-table-async
                                output buffer generation
                                (lambda (headers rows)
                                  (if (get-buffer-window buffer t)
                                      (kubed-ext--render-resource-response generation headers rows)
                                    (setq kubed-ext--view-pending (list generation headers rows)
                                          mode-line-process " [ready; hidden]"))))
                             (error (setq mode-line-process (propertize " [invalid table]" 'face 'error))
                                    (message "Kubed table: %s" (error-message-string err))))))))
                   (lambda (failure)
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (when (= generation kubed-ext--view-generation)
                           (setq kubed-ext--view-request nil
                                 mode-line-process (propertize " [request failed]" 'face 'error))
                           (message "Kubed resource list: %s" failure))))))))))

(defvar kubed-ext-auto-refresh-mode)

(defun kubed-ext--auto-refresh-tick (buffer)
  "Refresh visible BUFFER only if it has no inflight request."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and kubed-ext-auto-refresh-mode (get-buffer-window buffer t)
                 (or (not kubed-ext--view-request) (kubed-ext-request-finished-p kubed-ext--view-request)))
        (kubed-ext--render-pending-view)
        (condition-case err (kubed-ext-refresh-resources)
          (error (message "Kubed auto-refresh: %s" (error-message-string err))))))))
(define-minor-mode kubed-ext-auto-refresh-mode
  "Refresh visible enhanced views, deduplicating inflight requests.
Disabling cancels the timer and this view's pending request."
  :lighter " Auto"
  (when (timerp kubed-ext--refresh-timer) (cancel-timer kubed-ext--refresh-timer))
  (setq kubed-ext--refresh-timer nil)
  (if kubed-ext-auto-refresh-mode
      (progn
        (unless (and kubed-ext--view-scope (numberp kubed-ext-auto-refresh-interval)
                     (> kubed-ext-auto-refresh-interval 0)
                     (< kubed-ext-auto-refresh-interval most-positive-fixnum))
          (setq kubed-ext-auto-refresh-mode nil)
          (user-error "Auto-refresh needs an enhanced view and positive interval"))
        (setq kubed-ext--refresh-timer (run-at-time kubed-ext-auto-refresh-interval
                                                   kubed-ext-auto-refresh-interval
                                                   #'kubed-ext--auto-refresh-tick (current-buffer))))
    (kubed-ext--view-cleanup)))

(defvar kubed-ext--contexts-cache (make-hash-table :test 'equal)
  "Environment-keyed list of kubeconfig context names.")
(defvar kubed-ext--namespaces-cache (make-hash-table :test 'equal)
  "Environment/context-keyed list of namespace names.")
(defvar kubed-ext--completion-requests (make-hash-table :test 'equal)
  "Inflight completion-cache requests, keyed like their cache entry.")
(defvar kubed-ext-context-history nil "History for context prompts.")
(defvar kubed-ext-namespace-history nil "History for namespace prompts.")

(defun kubed-ext-resources-stop-all ()
  "Stop all enhanced-view timers, requests and stale discovery callbacks."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'kubed-ext-resources-mode)
        (setq kubed-ext-auto-refresh-mode nil) (kubed-ext--view-cleanup))))
  (maphash (lambda (_key state)
             (when (plist-get state :request) (kubed-ext-cancel-request (plist-get state :request))))
           kubed-ext--crd-discovery-state)
  (clrhash kubed-ext--crd-discovery-state)
  (maphash (lambda (_key request)
             (when (kubed-ext-request-p request) (kubed-ext-cancel-request request)))
           kubed-ext--completion-requests)
  (clrhash kubed-ext--completion-requests))

(defun kubed-ext--environment-digest (environment)
  "Return opaque identity for ENVIRONMENT, without persisting credentials."
  (secure-hash 'sha256 (let ((print-length nil) (print-level nil))
                        (prin1-to-string (kubed-ext--environment-key environment)))))

(defun kubed-ext--bookmark-record ()
  "Create a scope/filter/sort/row bookmark without saving environment secrets.
Reopening requires the same current environment or a matching live view."
  (let ((scope (copy-sequence kubed-ext--view-scope)))
    (setq scope (plist-put scope :environment nil))
    `((handler . kubed-ext--bookmark-jump) (scope . ,scope)
      (environment-id . ,(kubed-ext--environment-digest kubed-ext--environment))
      (filter . ,kubed-ext--view-filter) (sort . ,tabulated-list-sort-key)
      (row . ,(tabulated-list-get-id)))))
(defun kubed-ext--bookmark-jump (bookmark)
  "Restore enhanced scope and presentation from BOOKMARK in matching environment."
  (let* ((scope (alist-get 'scope bookmark))
         (identity (alist-get 'environment-id bookmark))
         (current (or kubed-ext--environment (kubed-ext--capture-environment)))
         (environment (and (equal identity (kubed-ext--environment-digest current)) current)))
    (unless environment
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (and kubed-ext--environment
                     (equal identity (kubed-ext--environment-digest kubed-ext--environment)))
            (setq environment kubed-ext--environment)))))
    (unless environment (user-error "Open bookmark from its original Kubernetes environment"))
    (kubed-ext--with-environment environment
      (let ((buffer (kubed-ext-list-resources (plist-get scope :type) (plist-get scope :context)
                                              (plist-get scope :namespace) (plist-get scope :selector))))
        (with-current-buffer buffer
          (setq kubed-ext--view-filter (or (alist-get 'filter bookmark) "")
                tabulated-list-sort-key (alist-get 'sort bookmark)
                kubed-ext--view-bookmark-row (alist-get 'row bookmark))
          (when tabulated-list-entries
            (kubed-ext--render-resource-response kubed-ext--view-generation
                                                kubed-ext--view-headers kubed-ext--view-rows)))))))

(defun kubed-ext--refresh-completion-cache (cache key argv &optional environment)
  "Refresh CACHE entry KEY from ARGV in ENVIRONMENT, at most once at a time.
Names arrive asynchronously, so an open prompt picks them up on the next
completion attempt.  Failure (including RBAC denial) leaves free-text entry
working and is reported only in the echo area."
  (let ((pending (list cache key)))
    (unless (gethash pending kubed-ext--completion-requests)
      (puthash pending t kubed-ext--completion-requests)
      (condition-case err
          (let ((request
                 (kubed-ext--with-environment
                     (or environment kubed-ext--environment (kubed-ext--capture-environment))
                   (kubed-ext--async-kubectl
                    argv
                    (lambda (output)
                      (remhash pending kubed-ext--completion-requests)
                      (puthash key (split-string output "\n" t "[[:space:]]+")
                               (symbol-value cache)))
                    (lambda (failure)
                      (remhash pending kubed-ext--completion-requests)
                      (message "Kubed completion unavailable; type the name: %s" failure))))))
            ;; A spawn failure runs the error callback before this point and
            ;; already cleared the entry; storing the request here would
            ;; resurrect it and block every later refresh of this key.
            (when (gethash pending kubed-ext--completion-requests)
              (puthash pending request kubed-ext--completion-requests)))
        (error (remhash pending kubed-ext--completion-requests)
               (message "Kubed completion could not start: %s" (error-message-string err)))))))

(defun kubed-ext-contexts-async (&optional environment)
  "Refresh the cached kubeconfig context names for ENVIRONMENT."
  (let ((environment (or environment kubed-ext--environment (kubed-ext--capture-environment))))
    (kubed-ext--refresh-completion-cache
     'kubed-ext--contexts-cache (kubed-ext--environment-key environment)
     (list (or (plist-get environment :kubectl) (kubed-ext--kubectl-program))
           "config" "get-contexts" "-o" "name")
     environment)))

(defun kubed-ext-namespaces-async (context &optional environment)
  "Refresh the cached namespace names of CONTEXT in ENVIRONMENT."
  (let ((environment (or environment kubed-ext--environment (kubed-ext--capture-environment))))
    (kubed-ext--refresh-completion-cache
     'kubed-ext--namespaces-cache (kubed-ext--crd-context-key context environment)
     (append (list (or (plist-get environment :kubectl) (kubed-ext--kubectl-program))
                   "get" "namespaces" "-o"
                   "jsonpath={range .items[*]}{.metadata.name}{\"\\n\"}{end}")
             (kubed-ext--scope-args (list :context context)))
     environment)))

(defun kubed-ext--cached-completion-table (cache key)
  "Return a completion table reading CACHE at KEY on every completion attempt.
Asynchronously discovered names therefore appear without reopening the prompt."
  (completion-table-dynamic
   (lambda (_string) (gethash key (symbol-value cache)))))

(defun kubed-ext-read-context (prompt &optional default environment)
  "Prompt with PROMPT for a context in ENVIRONMENT, defaulting to DEFAULT.
Unknown contexts are accepted after confirmation, so a kubeconfig that cannot
be listed never blocks navigation."
  (let ((environment (or environment kubed-ext--environment (kubed-ext--capture-environment))))
    (kubed-ext-contexts-async environment)
    (completing-read (format-prompt prompt default)
                     (kubed-ext--cached-completion-table
                      'kubed-ext--contexts-cache (kubed-ext--environment-key environment))
                     nil 'confirm nil 'kubed-ext-context-history default)))

(defun kubed-ext-read-namespace (prompt context &optional default environment)
  "Prompt with PROMPT for a namespace of CONTEXT, defaulting to DEFAULT.
Empty input means all namespaces.  Unknown namespaces are accepted after
confirmation, so listing namespaces is never required."
  (let ((environment (or environment kubed-ext--environment (kubed-ext--capture-environment))))
    (kubed-ext-namespaces-async context environment)
    (completing-read (format-prompt prompt default)
                     (kubed-ext--cached-completion-table
                      'kubed-ext--namespaces-cache
                      (kubed-ext--crd-context-key context environment))
                     nil 'confirm nil 'kubed-ext-namespace-history default)))

(declare-function kubed-ext-list-pods "kubed-ext-pods" (&optional context namespace))

(defun kubed-ext--reopen-scope (type context namespace selector)
  "Open TYPE in CONTEXT and NAMESPACE with SELECTOR, keeping the current view.
The pod table stays a pod table instead of falling back to the generic list."
  (if (derived-mode-p 'kubed-ext-pods-mode)
      (kubed-ext-list-pods context namespace)
    (kubed-ext-list-resources type context namespace selector)))

(defun kubed-ext-switch-context ()
  "Open the current resource view in another context, with completion."
  (interactive)
  (let* ((scope (kubed-ext--current-scope))
         (context (kubed-ext-read-context "Context" (plist-get scope :context)
                                          (plist-get scope :environment))))
    (kubed-ext--reopen-scope (or (plist-get scope :type) "pods") context
                             (plist-get scope :namespace) (plist-get scope :selector))))
(defun kubed-ext-switch-namespace ()
  "Open the current resource in another namespace, with completion.
Empty input lists all namespaces."
  (interactive)
  (let* ((scope (kubed-ext--current-scope))
         (namespace (kubed-ext-read-namespace
                     "Namespace (empty: all)" (plist-get scope :context)
                     (plist-get scope :namespace) (plist-get scope :environment))))
    (kubed-ext--reopen-scope (or (plist-get scope :type) "pods") (plist-get scope :context)
                             (unless (string-empty-p namespace) namespace)
                             (plist-get scope :selector))))
(defun kubed-ext-switch-resource ()
  "Open another resource type, allowing explicit FQ types after RBAC denial."
  (interactive)
  (let* ((scope (kubed-ext--current-scope))
         (type (completing-read "Resource (FQ allowed): "
                                 (append '("pods" "deployments" "services" "jobs" "nodes")
                                         (kubed-ext-crd-resource-choices (plist-get scope :context))) nil nil)))
    (kubed-ext-list-resources type (plist-get scope :context) (plist-get scope :namespace)
                              (plist-get scope :selector))))
(add-hook 'kubed-ext-shutdown-hook #'kubed-ext-resources-stop-all)
(defun kubed-ext-resources-unload-function ()
  "Stop resource-owned state and remove its shutdown registration."
  (kubed-ext-resources-stop-all)
  (remove-hook 'kubed-ext-shutdown-hook #'kubed-ext-resources-stop-all)
  nil)
(provide 'kubed-ext-resources)
;;; kubed-ext-resources.el ends here
