;;; kubed-ext-output-test.el --- Tests for highlighted output buffers -*- lexical-binding: t; -*-

;;; Commentary:

;; Mode selection must degrade safely: a tree-sitter mode is only used when
;; its grammar is installed, and output buffers stay read-only either way.

;;; Code:

(require 'ert)
(require 'kubed-ext-actions)

(defmacro kubed-ext-output-test--with-modes (grammar-ready &rest body)
  "Evaluate BODY with stub YAML and JSON modes.
GRAMMAR-READY decides whether the tree-sitter grammar is reported installed."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'kubed-ext-test-yaml-ts-mode) #'fundamental-mode)
             ((symbol-function 'kubed-ext-test-yaml-mode) #'fundamental-mode)
             ((symbol-function 'treesit-ready-p) (lambda (&rest _) ,grammar-ready)))
     (let ((kubed-ext-yaml-modes '(kubed-ext-test-yaml-ts-mode kubed-ext-test-yaml-mode))
           (kubed-ext-json-modes '(kubed-ext-test-yaml-ts-mode))
           (kubed-ext-highlight-output t)
           (kubed-ext-highlight-describe t))
       ,@body)))

(ert-deftest kubed-ext-output-prefers-tree-sitter-when-grammar-present ()
  "The first available mode wins when its grammar is installed."
  (kubed-ext-output-test--with-modes t
    (should (eq (kubed-ext--output-mode-for "yaml") 'kubed-ext-test-yaml-ts-mode))
    (should (eq (kubed-ext--output-mode-for "json") 'kubed-ext-test-yaml-ts-mode))))

(ert-deftest kubed-ext-output-skips-tree-sitter-without-grammar ()
  "A tree-sitter mode is skipped when its grammar is missing."
  (kubed-ext-output-test--with-modes nil
    (should (eq (kubed-ext--output-mode-for "yaml") 'kubed-ext-test-yaml-mode))
    ;; No non-tree-sitter JSON fallback is configured here.
    (should-not (kubed-ext--output-mode-for "json"))))

(ert-deftest kubed-ext-output-describe-is-opt-out ()
  "Describe output follows its own setting, independent of YAML output."
  (kubed-ext-output-test--with-modes t
    (should (eq (kubed-ext--output-mode-for "describe") 'kubed-ext-test-yaml-ts-mode))
    (let ((kubed-ext-highlight-describe nil))
      (should-not (kubed-ext--output-mode-for "describe"))
      ;; Disabling describe highlighting leaves resource output alone.
      (should (kubed-ext--output-mode-for "yaml")))))

(ert-deftest kubed-ext-output-highlighting-can-be-disabled ()
  "Disabling highlighting selects no mode at all."
  (kubed-ext-output-test--with-modes t
    (let ((kubed-ext-highlight-output nil))
      (should-not (kubed-ext--output-mode-for "yaml"))
      (should-not (kubed-ext--output-mode-for "describe")))))

(ert-deftest kubed-ext-output-unknown-format-is-plain ()
  "An action without a known output format stays plain text."
  (kubed-ext-output-test--with-modes t
    (should-not (kubed-ext--output-mode-for "rollout"))
    (should-not (kubed-ext--output-mode-for "exec"))))

(ert-deftest kubed-ext-output-buffer-is-read-only-and-quittable ()
  "A highlighted buffer is still read-only and still quits with `q'."
  (with-temp-buffer
    (kubed-ext-output-test--with-modes t
      (kubed-ext--apply-output-mode "yaml")
      (should (eq kubed-ext--output-mode 'kubed-ext-test-yaml-ts-mode))
      (should buffer-read-only)
      (should (eq (keymap-lookup (current-local-map) "q") #'quit-window)))))

(ert-deftest kubed-ext-output-falls-back-to-special-mode ()
  "Without a highlighting mode the buffer behaves as before."
  (with-temp-buffer
    (kubed-ext-output-test--with-modes t
      (let ((kubed-ext-highlight-output nil))
        (kubed-ext--apply-output-mode "yaml")
        (should-not kubed-ext--output-mode)
        (should (derived-mode-p 'special-mode))))))

(ert-deftest kubed-ext-output-render-keeps-chosen-mode ()
  "Rendering a response must not reset the buffer to `special-mode'."
  (let ((buffer (generate-new-buffer " *kubed output test*")))
    (unwind-protect
        (with-current-buffer buffer
          (kubed-ext-output-test--with-modes t
            (kubed-ext--apply-output-mode "yaml")
            (let ((mode major-mode))
              (kubed-ext--show-action-output buffer "apiVersion: v1\nkind: Pod\n")
              (should (eq major-mode mode))
              (should-not (derived-mode-p 'special-mode))
              (should (string-prefix-p "apiVersion: v1"
                                       (buffer-substring-no-properties
                                        (point-min) (point-max))))
              (should buffer-read-only))))
      (kill-buffer buffer))))

(provide 'kubed-ext-output-test)
;;; kubed-ext-output-test.el ends here
