EMACS ?= emacs
BATCH = $(EMACS) --batch -Q -L .

# Indent settings for batch formatting (match project macro declarations)
INDENT_SETUP = --eval '(setq-default indent-tabs-mode nil)' \
  --eval '(put (quote org-drafts-with-change-to) (quote lisp-indent-function) 1)' \
  --eval '(put (quote org-drafts-with) (quote lisp-indent-function) 2)' \
  --eval '(put (quote pretty-hydra-define) (quote lisp-indent-function) (quote defun))'

.PHONY: all compile lint checkdoc format-check format test coverage \
	coverage-check benchmark benchmark-save benchmark-check clean

all: compile lint checkdoc format-check test

## Byte-compile with all warnings as errors
compile:
	$(BATCH) \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile org-drafts.el

## Run package-lint
lint:
	$(BATCH) \
	  --eval '(require (quote package-lint))' \
	  -f package-lint-batch-and-exit org-drafts.el

## Validate documentation strings
checkdoc:
	$(BATCH) \
	  --eval '(with-current-buffer (find-file-noselect "org-drafts.el") (let ((errs (checkdoc-file "org-drafts.el"))) (when (stringp errs) (message "%s" errs) (kill-emacs 1))))' \
	  --eval '(message "checkdoc: OK")'

## Check that code formatting matches Emacs indentation standards
format-check:
	$(BATCH) $(INDENT_SETUP) \
	  --eval '(find-file "org-drafts.el")' \
	  --eval '(let ((original (buffer-string))) (indent-region (point-min) (point-max)) (if (string= original (buffer-string)) (message "format-check: OK") (message "Formatting differs from Emacs standard indentation") (kill-emacs 1)))'

## Auto-format code to Emacs indentation standards
format:
	$(BATCH) $(INDENT_SETUP) \
	  --eval '(find-file "org-drafts.el")' \
	  --eval '(indent-region (point-min) (point-max))' \
	  --eval '(save-buffer)' \
	  --eval '(message "Formatted org-drafts.el")'

## Run ERT tests
test:
	$(BATCH) \
	  -l org-drafts-test.el \
	  -f ert-run-tests-batch-and-exit

## Run tests with code coverage via undercover
coverage:
	$(BATCH) \
	  --eval '(require (quote undercover))' \
	  --eval '(undercover "org-drafts.el" (:report-file "coverage/lcov.info") (:report-format (quote lcov)) (:send-report nil))' \
	  -l org-drafts-test.el \
	  -f ert-run-tests-batch-and-exit
	@echo "Coverage report: coverage/lcov.info"

## Check that code coverage meets threshold
coverage-check: coverage
	@if [ -f .coverage-threshold ]; then \
	  $(BATCH) -l org-drafts-test.el \
	    --eval '(org-drafts-test-coverage-check "coverage/lcov.info" (with-temp-buffer (insert-file-contents ".coverage-threshold") (string-to-number (buffer-string))))'; \
	else \
	  echo "No coverage threshold found. Create .coverage-threshold with a percentage (e.g. 70)"; \
	fi

## Run performance benchmarks
benchmark:
	$(BATCH) \
	  -l org-drafts-test.el \
	  --eval '(org-drafts-test-benchmark)'

## Save current benchmark as baseline
benchmark-save:
	$(BATCH) \
	  -l org-drafts-test.el \
	  --eval '(org-drafts-test-benchmark-save ".benchmark-baseline")'

## Check that benchmark results have not regressed >5%
benchmark-check:
	@if [ -f .benchmark-baseline ]; then \
	  $(BATCH) \
	    -l org-drafts-test.el \
	    --eval '(org-drafts-test-benchmark-check ".benchmark-baseline" 0.05)'; \
	else \
	  echo "No benchmark baseline found. Run: make benchmark > .benchmark-baseline"; \
	fi

clean:
	rm -f org-drafts.elc
	rm -rf coverage/
