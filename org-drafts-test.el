;;; org-drafts-test.el --- Tests for org-drafts -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>

;;; Commentary:

;; ERT tests for the org-drafts package.  Designed to run in batch mode
;; without requiring pretty-hydra, copy-as-format, or gptel.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)

;;; ---- Stubs for unavailable dependencies ----

;; Provide stubs for packages that are not available during batch testing.
;; These must be set up before loading org-drafts.

(unless (featurep 'copy-as-format)
  (defvar copy-as-format-format-alist nil
    "Stub alist for copy-as-format.")
  (provide 'copy-as-format))

(unless (featurep 'pretty-hydra)
  (defmacro pretty-hydra-define (name _body _heads-plist &rest _)
    "Stub macro that defines NAME/body as a no-op function."
    (declare (indent defun))
    (let ((func-name (intern (format "%s/body" name))))
      `(defun ,func-name ()
         ,(format "Stub hydra body for %s." name)
         (interactive)
         nil)))
  (provide 'pretty-hydra))

(require 'org-drafts)

;;; ---- Test helpers ----

(defvar org-drafts-test--finalize-called nil
  "Flag set to t when the mock org-capture-finalize is called.")

(defvar org-drafts-test--finalize-arg nil
  "Stores the argument passed to mock org-capture-finalize.")

(defvar org-drafts-test--hydra-called nil
  "Flag set to t when the mock hydra body is called.")

(defmacro org-drafts-test--with-org-buffer (content &rest body)
  "Execute BODY in a temp buffer with CONTENT set up as `org-mode'.
Point is left at `point-min'."
  (declare (indent 1) (debug (form body)))
  `(with-temp-buffer
     (org-mode)
     (insert ,content)
     (goto-char (point-min))
     ,@body))

(defmacro org-drafts-test--with-capture-buffer (content &rest body)
  "Execute BODY in a temp buffer simulating org-capture-mode.
CONTENT is inserted and `org-capture-mode' is set to t.
`org-capture-finalize' is mocked to record calls."
  (declare (indent 1) (debug (form body)))
  `(org-drafts-test--with-org-buffer ,content
     (let ((org-capture-mode t)
           (org-drafts-test--finalize-called nil)
           (org-drafts-test--finalize-arg nil))
       (cl-letf (((symbol-function 'org-capture-finalize)
                  (lambda (&optional arg)
                    (setq org-drafts-test--finalize-called t
                          org-drafts-test--finalize-arg arg))))
         ,@body))))

;;; ---- Tests for org-drafts-default-body-function ----

(ert-deftest org-drafts-test-default-body-function-basic ()
  "First body line should replace the timestamp in the heading."
  (org-drafts-test--with-org-buffer
      "* DRAFT [2025-07-14 Mon]\nBuy groceries\nExtra notes here\n"
    (let ((heading-pos (point-marker)))
      (forward-line)
      (let ((beg (point-marker))
            (end (point-max-marker)))
        (org-drafts-default-body-function heading-pos beg end)
        (goto-char (point-min))
        (should (string-match-p "^\\* DRAFT Buy groceries$"
                                (buffer-substring (point-min)
                                                  (line-end-position))))
        ;; The first body line ("Buy groceries") should be consumed.
        (forward-line)
        (should (looking-at-p "Extra notes here"))))))

(ert-deftest org-drafts-test-default-body-function-single-line ()
  "Body with only one line should become the heading title, leaving no body."
  (org-drafts-test--with-org-buffer
      "* DRAFT [2025-07-14 Mon]\nSingle line body\n"
    (let ((heading-pos (point-marker)))
      (forward-line)
      (let ((beg (point-marker))
            (end (point-max-marker)))
        (org-drafts-default-body-function heading-pos beg end)
        (goto-char (point-min))
        (should (string-match-p "^\\* DRAFT Single line body$"
                                (buffer-substring (point-min)
                                                  (line-end-position))))
        ;; No body lines remain.
        (forward-line)
        (should (eobp))))))

(ert-deftest org-drafts-test-default-body-function-trims-whitespace ()
  "Leading/trailing whitespace on first body line should be trimmed."
  (org-drafts-test--with-org-buffer
      "* DRAFT [2025-07-14 Mon]\n  Padded text  \nMore\n"
    (let ((heading-pos (point-marker)))
      (forward-line)
      (let ((beg (point-marker))
            (end (point-max-marker)))
        (org-drafts-default-body-function heading-pos beg end)
        (goto-char (point-min))
        (should (string-match-p "^\\* DRAFT Padded text$"
                                (buffer-substring (point-min)
                                                  (line-end-position))))))))

;;; ---- Tests for org-drafts-with ----

(ert-deftest org-drafts-test-with-calls-all-callbacks ()
  "org-drafts-with should call at-heading-func, body-func, and
at-capture-end-func when in capture mode."
  (let ((at-heading-called nil)
        (at-capture-end-called nil)
        (body-called nil)
        (body-args nil))
    (org-drafts-test--with-capture-buffer
        "* DRAFT [2025-07-14 Mon]\nSome body text\n"
      (cl-letf (((symbol-function 'org-capture-finalize)
                 (lambda (&optional _arg) nil)))
        (org-drafts-with
         (lambda () (setq at-heading-called t))
         (lambda () (setq at-capture-end-called t))
         (lambda (heading-pos beg end)
           (setq body-called t
                 body-args (list heading-pos beg end))
           nil)))
      (should at-heading-called)
      (should at-capture-end-called)
      (should body-called)
      ;; All three args should be markers.
      (should (markerp (nth 0 body-args)))
      (should (markerp (nth 1 body-args)))
      (should (markerp (nth 2 body-args))))))

(ert-deftest org-drafts-test-with-skips-properties-drawer ()
  "body-func beg marker should point past a PROPERTIES drawer."
  (let ((body-beg nil))
    (org-drafts-test--with-capture-buffer
        "* DRAFT [2025-07-14 Mon]\n:PROPERTIES:\n:CREATED: [2025-07-14 Mon]\n:END:\nActual body\n"
      (org-drafts-with
       #'ignore
       #'ignore
       (lambda (_heading-pos beg _end)
         (setq body-beg (marker-position beg))
         nil))
      (goto-char body-beg)
      (should (looking-at-p "Actual body")))))

(ert-deftest org-drafts-test-with-no-capture-mode ()
  "at-capture-end-func should NOT be called outside capture mode."
  (let ((at-capture-end-called nil))
    (org-drafts-test--with-org-buffer
        "* DRAFT [2025-07-14 Mon]\nBody text\n* Next heading\n"
      (let ((org-capture-mode nil))
        (org-drafts-with
         #'ignore
         (lambda () (setq at-capture-end-called t))
         (lambda (_h _b _e) nil)))
      (should-not at-capture-end-called))))

(ert-deftest org-drafts-test-with-end-marker-non-capture ()
  "Outside capture mode, end marker should stop at the next heading."
  (let ((body-end nil))
    (org-drafts-test--with-org-buffer
        "* DRAFT Test\nFirst body\n* Second heading\nOther content\n"
      (let ((org-capture-mode nil))
        (org-drafts-with
         #'ignore
         #'ignore
         (lambda (_h _b end)
           (setq body-end (marker-position end))
           nil)))
      (goto-char body-end)
      (should (looking-at-p "\\* Second heading")))))

(ert-deftest org-drafts-test-with-returns-body-func-value ()
  "org-drafts-with should return whatever body-func returns."
  (org-drafts-test--with-capture-buffer
      "* DRAFT Test\nBody\n"
    (let ((result (org-drafts-with
                   #'ignore
                   #'ignore
                   (lambda (_h _b _e) 42))))
      (should (equal result 42)))))

;;; ---- Tests for org-drafts-with-change-to ----

(ert-deftest org-drafts-test-with-change-to-replaces-draft ()
  "DRAFT keyword should be replaced with the specified keyword."
  (org-drafts-test--with-capture-buffer
      "* DRAFT [2025-07-14 Mon]\nBody text\n"
    (org-drafts-with-change-to "NOTE"
      (lambda (_h _b _e) nil))
    (goto-char (point-min))
    (should (looking-at-p "^\\* NOTE "))))

(ert-deftest org-drafts-test-with-change-to-replaces-scrap ()
  "SCRAP keyword should also be replaced."
  (org-drafts-test--with-capture-buffer
      "* SCRAP [2025-07-14 Mon]\nBody text\n"
    (org-drafts-with-change-to "TODO"
      (lambda (_h _b _e) nil))
    (goto-char (point-min))
    (should (looking-at-p "^\\* TODO "))))

(ert-deftest org-drafts-test-with-change-to-ignores-other-keywords ()
  "Non-DRAFT/SCRAP keywords should be left untouched."
  (org-drafts-test--with-capture-buffer
      "* TODO [2025-07-14 Mon]\nBody text\n"
    (org-drafts-with-change-to "NOTE"
      (lambda (_h _b _e) nil))
    (goto-char (point-min))
    (should (looking-at-p "^\\* TODO "))))

(ert-deftest org-drafts-test-with-change-to-calls-finalize ()
  "org-capture-finalize should be called in capture mode."
  (org-drafts-test--with-capture-buffer
      "* DRAFT Test\nBody\n"
    (org-drafts-with-change-to "SCRAP"
      (lambda (_h _b _e) nil))
    (should org-drafts-test--finalize-called)))

(ert-deftest org-drafts-test-with-change-to-body-receives-markers ()
  "body-func should receive valid markers for heading, beg, and end."
  (let ((received nil))
    (org-drafts-test--with-capture-buffer
        "* DRAFT Test\nThe body content\n"
      (org-drafts-with-change-to "SCRAP"
        (lambda (heading-pos beg end)
          (setq received (list (marker-position heading-pos)
                               (marker-position beg)
                               (marker-position end)))
          nil))
      (should received)
      ;; heading-pos should be at line 1.
      (should (= (nth 0 received) 1))
      ;; beg should be past the heading line.
      (should (> (nth 1 received) (nth 0 received)))
      ;; end should be at point-max in capture mode.
      (should (= (nth 2 received) (point-max))))))

(ert-deftest org-drafts-test-with-change-to-multi-star ()
  "Headings with multiple stars (deeper levels) should work."
  (org-drafts-test--with-org-buffer
      "** DRAFT Nested\nBody\n"
    (let ((org-capture-mode nil))
      (org-drafts-with-change-to "TODO"
        (lambda (_h _b _e) nil))
      (goto-char (point-min))
      (should (looking-at-p "^\\*\\* TODO ")))))

;;; ---- Tests for org-drafts-change ----

(ert-deftest org-drafts-test-change-to-note ()
  "org-drafts-change to NOTE should change keyword and apply body function."
  (org-drafts-test--with-capture-buffer
      "* DRAFT [2025-07-14 Mon]\nNew title text\nRest of body\n"
    (let ((org-drafts-task-body-function
           #'org-drafts-default-body-function))
      (org-drafts-change "NOTE"))
    (goto-char (point-min))
    (should (looking-at-p "^\\* NOTE New title text$"))))

(ert-deftest org-drafts-test-change-to-todo ()
  "org-drafts-change to TODO should change keyword and apply body function."
  (org-drafts-test--with-capture-buffer
      "* DRAFT [2025-07-14 Mon]\nDo the dishes\n"
    (let ((org-drafts-task-body-function
           #'org-drafts-default-body-function))
      (org-drafts-change "TODO"))
    (goto-char (point-min))
    (should (looking-at-p "^\\* TODO Do the dishes$"))))

(ert-deftest org-drafts-test-change-uses-custom-body-function ()
  "org-drafts-change should use the customizable body function."
  (let ((custom-called nil))
    (org-drafts-test--with-capture-buffer
        "* DRAFT [2025-07-14 Mon]\nBody\n"
      (let ((org-drafts-task-body-function
             (lambda (_h _b _e) (setq custom-called t))))
        (org-drafts-change "NOTE")))
    (should custom-called)))

;;; ---- Tests for org-drafts-copy-to-clipboard ----

(ert-deftest org-drafts-test-copy-to-clipboard-plain ()
  "Plain copy should put body text in the kill ring."
  (org-drafts-test--with-capture-buffer
      "* DRAFT Test\nHello world\n"
    (org-drafts-copy-to-clipboard)
    (should (equal "Hello world" (car kill-ring)))))

(ert-deftest org-drafts-test-copy-to-clipboard-multiline ()
  "Multi-line body should be copied entirely."
  (org-drafts-test--with-capture-buffer
      "* DRAFT Test\nLine one\nLine two\nLine three\n"
    (org-drafts-copy-to-clipboard)
    (should (equal "Line one\nLine two\nLine three" (car kill-ring)))))

(ert-deftest org-drafts-test-copy-to-clipboard-trims ()
  "Body text should be trimmed of leading/trailing whitespace."
  (org-drafts-test--with-capture-buffer
      "* DRAFT Test\n  Trimmed  \n"
    (org-drafts-copy-to-clipboard)
    (should (equal "Trimmed" (car kill-ring)))))

(ert-deftest org-drafts-test-copy-to-clipboard-changes-keyword ()
  "Copy should change DRAFT to SCRAP."
  (org-drafts-test--with-capture-buffer
      "* DRAFT Test\nContent\n"
    (org-drafts-copy-to-clipboard)
    (goto-char (point-min))
    (should (looking-at-p "^\\* SCRAP "))))

(ert-deftest org-drafts-test-copy-to-clipboard-with-properties ()
  "Copy should skip PROPERTIES drawer and copy only body."
  (org-drafts-test--with-capture-buffer
      "* DRAFT Test\n:PROPERTIES:\n:ID: abc123\n:END:\nActual content\n"
    (org-drafts-copy-to-clipboard)
    (should (equal "Actual content" (car kill-ring)))))

;;; ---- Tests for org-drafts-action ----

(ert-deftest org-drafts-test-action-draft-calls-hydra ()
  "When entry is DRAFT, org-drafts-action should call the hydra."
  (org-drafts-test--with-capture-buffer
      "* DRAFT Test\nBody\n"
    (let ((hydra-called nil))
      (cl-letf (((symbol-function 'org-drafts/body)
                 (lambda () (setq hydra-called t))))
        (org-drafts-action))
      (should hydra-called))))

(ert-deftest org-drafts-test-action-non-draft-calls-finalize ()
  "When entry is not DRAFT, org-drafts-action should call org-capture-finalize."
  (org-drafts-test--with-capture-buffer
      "* TODO Test\nBody\n"
    (org-drafts-action)
    (should org-drafts-test--finalize-called)))

(ert-deftest org-drafts-test-action-passes-prefix-arg ()
  "org-drafts-action should pass its ARG to org-capture-finalize."
  (org-drafts-test--with-capture-buffer
      "* TODO Test\nBody\n"
    (org-drafts-action '(4))
    (should org-drafts-test--finalize-called)
    (should (equal '(4) org-drafts-test--finalize-arg))))

(ert-deftest org-drafts-test-action-nested-draft ()
  "Deeply nested DRAFT headings should still trigger the hydra."
  (org-drafts-test--with-capture-buffer
      "*** DRAFT Deeply nested\nBody\n"
    (let ((hydra-called nil))
      (cl-letf (((symbol-function 'org-drafts/body)
                 (lambda () (setq hydra-called t))))
        (org-drafts-action))
      (should hydra-called))))

;;; ---- Tests for org-drafts-act-on-existing ----

(ert-deftest org-drafts-test-act-on-existing-draft ()
  "Should return t and call hydra for existing DRAFT entry."
  (org-drafts-test--with-org-buffer
      "* DRAFT Test\nBody\n"
    (let ((org-capture-mode nil)
          (org-todo-keywords '((sequence "TODO" "DRAFT" "SCRAP" "|" "DONE")))
          (hydra-called nil))
      ;; org-mode needs to know about these keywords.
      (org-set-regexps-and-options)
      (cl-letf (((symbol-function 'org-drafts/body)
                 (lambda () (setq hydra-called t))))
        (let ((result (org-drafts-act-on-existing)))
          (should result)
          (should hydra-called))))))

(ert-deftest org-drafts-test-act-on-existing-scrap ()
  "Should return t for existing SCRAP entry."
  (org-drafts-test--with-org-buffer
      "* SCRAP Test\nBody\n"
    (let ((org-capture-mode nil)
          (org-todo-keywords '((sequence "TODO" "DRAFT" "SCRAP" "|" "DONE")))
          (hydra-called nil))
      (org-set-regexps-and-options)
      (cl-letf (((symbol-function 'org-drafts/body)
                 (lambda () (setq hydra-called t))))
        (let ((result (org-drafts-act-on-existing)))
          (should result)
          (should hydra-called))))))

(ert-deftest org-drafts-test-act-on-existing-todo ()
  "Should return nil for TODO entry (not DRAFT/SCRAP)."
  (org-drafts-test--with-org-buffer
      "* TODO Test\nBody\n"
    (let ((org-capture-mode nil)
          (org-todo-keywords '((sequence "TODO" "DRAFT" "SCRAP" "|" "DONE"))))
      (org-set-regexps-and-options)
      (should-not (org-drafts-act-on-existing)))))

(ert-deftest org-drafts-test-act-on-existing-no-heading ()
  "Should return nil when not on a heading."
  (org-drafts-test--with-org-buffer
      "Just plain text, no heading\n"
    (let ((org-capture-mode nil))
      (should-not (org-drafts-act-on-existing)))))

;;; ---- Tests for org-drafts-install ----

(ert-deftest org-drafts-test-install-adds-hook ()
  "org-drafts-install should add act-on-existing to the hook."
  (let ((org-ctrl-c-ctrl-c-hook nil))
    (cl-letf (((symbol-function 'define-key)
               (lambda (_map _key _def) nil)))
      (org-drafts-install))
    (should (memq #'org-drafts-act-on-existing
                  org-ctrl-c-ctrl-c-hook))))

(ert-deftest org-drafts-test-install-binds-key ()
  "org-drafts-install should bind C-c C-c in org-capture-mode-map."
  (let ((bound-key nil)
        (bound-func nil))
    (cl-letf (((symbol-function 'define-key)
               (lambda (_map key def)
                 (setq bound-key key
                       bound-func def))))
      (org-drafts-install))
    (should (equal (kbd "C-c C-c") bound-key))
    (should (eq #'org-drafts-action bound-func))))

;;; ---- Tests for org-drafts-kagi ----

(ert-deftest org-drafts-test-kagi-builds-url ()
  "org-drafts-kagi should browse to a Kagi URL with the body as query."
  (let ((browsed-url nil))
    (org-drafts-test--with-capture-buffer
        "* DRAFT Test\nEmacs Lisp tutorial\n"
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (url) (setq browsed-url url))))
        (org-drafts-kagi)))
    (should (stringp browsed-url))
    (should (string-prefix-p "https://kagi.com/search?q=" browsed-url))
    (should (string-match-p "Emacs" browsed-url))))

;;; ---- Tests for org-drafts-claude ----

(ert-deftest org-drafts-test-claude-builds-url ()
  "org-drafts-claude should browse to a Claude URL with the body as query."
  (let ((browsed-url nil))
    (org-drafts-test--with-capture-buffer
        "* DRAFT Test\nExplain monads\n"
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (url) (setq browsed-url url))))
        (org-drafts-claude)))
    (should (stringp browsed-url))
    (should (string-prefix-p "https://claude.ai/new?q=" browsed-url))
    (should (string-match-p "monads" browsed-url))))

;;; ---- Tests for org-drafts-perplexity ----

(ert-deftest org-drafts-test-perplexity-builds-url ()
  "org-drafts-perplexity should browse to Perplexity with the body as query."
  (let ((browsed-url nil))
    (org-drafts-test--with-capture-buffer
        "* DRAFT Test\nWhat is a closure\n"
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (url) (setq browsed-url url))))
        (org-drafts-perplexity)))
    (should (stringp browsed-url))
    (should (string-prefix-p "https://www.perplexity.ai/search/" browsed-url))
    (should (string-match-p "closure" browsed-url))
    (should (string-match-p "copilot=true" browsed-url))))

;;; ---- Tests for org-drafts-email ----

(ert-deftest org-drafts-test-email-composes ()
  "org-drafts-email should call compose-mail and insert the body."
  (let ((composed nil)
        (mail-buf (generate-new-buffer " *test-mail*")))
    (unwind-protect
        (progn
          (org-drafts-test--with-capture-buffer
              "* DRAFT Test\nDear colleague\n"
            (cl-letf (((symbol-function 'compose-mail)
                       (lambda (&rest _)
                         (setq composed t)
                         (set-buffer mail-buf)))
                      ((symbol-function 'message-goto-body)
                       (lambda () nil)))
              (org-drafts-email)))
          (should composed)
          (with-current-buffer mail-buf
            (should (equal "Dear colleague" (buffer-string)))))
      (kill-buffer mail-buf))))

;;; ---- Edge case tests ----

(ert-deftest org-drafts-test-empty-body ()
  "Copy with an empty body should put empty string in kill ring."
  (org-drafts-test--with-capture-buffer
      "* DRAFT Test\n"
    (org-drafts-copy-to-clipboard)
    (should (equal "" (car kill-ring)))))

(ert-deftest org-drafts-test-with-change-to-preserves-heading-text ()
  "Text after the keyword should be preserved when changing keywords."
  (org-drafts-test--with-capture-buffer
      "* DRAFT Important meeting notes\nBody\n"
    (org-drafts-with-change-to "SCRAP"
      (lambda (_h _b _e) nil))
    (goto-char (point-min))
    (should (looking-at-p
             "^\\* SCRAP Important meeting notes$"))))

(ert-deftest org-drafts-test-body-with-org-markup ()
  "Body containing Org markup should be copied verbatim."
  (org-drafts-test--with-capture-buffer
      "* DRAFT Test\n- Item one\n- Item two\n  - Sub item\n"
    (org-drafts-copy-to-clipboard)
    (should (string-match-p "^- Item one" (car kill-ring)))
    (should (string-match-p "- Sub item" (car kill-ring)))))

;;; ---- Benchmark ----

(defun org-drafts-test-benchmark (&optional iterations)
  "Benchmark org-drafts-with-change-to for ITERATIONS (default 1000).
Prints timing results to standard output."
  (let* ((n (or iterations 1000))
         (result
          (benchmark-run n
            (with-temp-buffer
              (org-mode)
              (insert "* DRAFT [2025-07-14 Mon]\nBenchmark body text\n")
              (goto-char (point-min))
              (let ((org-capture-mode t))
                (cl-letf (((symbol-function 'org-capture-finalize)
                           (lambda (&optional _arg) nil)))
                  (org-drafts-with-change-to "SCRAP"
                    (lambda (_h _b _e) nil))))))))
    (message "org-drafts-with-change-to benchmark (%d iterations):" n)
    (message "  Total time:  %.4f seconds" (nth 0 result))
    (message "  GC count:    %d" (nth 1 result))
    (message "  GC time:     %.4f seconds" (nth 2 result))
    (message "  Per call:    %.6f seconds" (/ (nth 0 result) (float n)))
    result))

(defun org-drafts-test-benchmark-save (file)
  "Run benchmark and save per-call time to FILE."
  (let* ((result (org-drafts-test-benchmark))
         (per-call (/ (nth 0 result) 1000.0)))
    (with-temp-file file
      (insert (format "%.6f" per-call)))
    (message "Saved benchmark baseline: %.6f seconds/call" per-call)))

(defun org-drafts-test-benchmark-check (baseline-file threshold)
  "Check benchmark against BASELINE-FILE.
Fail if regression exceeds THRESHOLD (e.g. 0.05 for 5%)."
  (let* ((baseline (with-temp-buffer
                     (insert-file-contents baseline-file)
                     (string-to-number (buffer-string))))
         (result (org-drafts-test-benchmark))
         (current (/ (nth 0 result) 1000.0))
         (ratio (if (> baseline 0) (/ current baseline) 1.0)))
    (message "Baseline: %.6f  Current: %.6f  Ratio: %.2f"
             baseline current ratio)
    (when (> ratio (+ 1.0 threshold))
      (message "FAIL: Performance regressed by %.1f%% (threshold: %.1f%%)"
               (* (- ratio 1.0) 100.0) (* threshold 100.0))
      (kill-emacs 1))
    (message "PASS: Performance within threshold")))

(defun org-drafts-test-coverage-check (report-file threshold)
  "Check line coverage in REPORT-FILE meets THRESHOLD percent."
  (let ((lines-found 0)
        (lines-hit 0))
    (with-temp-buffer
      (insert-file-contents report-file)
      (goto-char (point-min))
      (while (re-search-forward "^DA:[0-9]+,\\([0-9]+\\)" nil t)
        (cl-incf lines-found)
        (when (> (string-to-number (match-string 1)) 0)
          (cl-incf lines-hit))))
    (let ((coverage (if (> lines-found 0)
                        (* 100.0 (/ (float lines-hit) lines-found))
                      0.0)))
      (message "Coverage: %.1f%% (threshold: %.1f%%)" coverage threshold)
      (when (< coverage threshold)
        (message "FAIL: Coverage %.1f%% below threshold %.1f%%"
                 coverage threshold)
        (kill-emacs 1))
      (message "PASS: Coverage meets threshold"))))

(provide 'org-drafts-test)

;;; org-drafts-test.el ends here
