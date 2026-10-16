EMACS ?= emacs
# kubed 0.7.1 is required: it provides the execution-environment API this
# package builds on.  Point KUBED at that checkout, not a newer tree.
KUBED ?= $(HOME)/.emacs.d/elpa/kubed-0.7.1
TIMEOUT ?= 300

EL := kubed-ext-core.el kubed-ext-resources.el kubed-ext-logs.el \
      kubed-ext-actions.el kubed-ext-metrics.el kubed-ext-terminals.el \
      kubed-ext-pods.el \
      kubed-ext.el
TESTS := $(wildcard tests/*.el)

# Every target runs in a disposable `emacs -Q --batch' under timeout(1): no
# target may touch a running Emacs, open a Tramp connection, or block.
BATCH = timeout $(TIMEOUT) $(EMACS) -Q --batch -L . -L $(KUBED)

.PHONY: all check compile test lint clean

all: check

check: compile test

compile: clean
	@$(BATCH) --eval '(setq byte-compile-error-on-warn t)' \
	          -f batch-byte-compile $(EL)
	@$(MAKE) --no-print-directory clean

test:
	@$(BATCH) -l ert $(patsubst %,-l %,$(EL)) $(patsubst %,-l %,$(TESTS)) \
	          -f ert-run-tests-batch-and-exit

lint:
	@$(BATCH) --eval '(progn (require (quote checkdoc)) \
	  (dolist (f (list $(patsubst %,"%",$(EL)))) \
	    (checkdoc-file f)) \
	  (with-current-buffer "*Warnings*" \
	    (when (> (buffer-size) 0) (princ (buffer-string)) (kill-emacs 1))))' \
	  2>&1 || true

clean:
	@rm -f *.elc tests/*.elc
