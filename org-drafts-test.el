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

(ert-deftest org-drafts-test-with-change-to-runs-state-change-hook ()
  "`org-after-todo-state-change-hook' should run in capture mode."
  (let ((hook-ran nil))
    (org-drafts-test--with-capture-buffer
        "* DRAFT Test\nBody\n"
      (let ((org-after-todo-state-change-hook
             (list (lambda () (setq hook-ran t)))))
        (org-drafts-with-change-to "SCRAP"
          (lambda (_h _b _e) nil)))
      (should hook-ran)
      (should-not org-drafts-test--finalize-called))))

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

;;; ---- Tests for org-drafts-change-alt ----

(ert-deftest org-drafts-test-change-alt-uses-alt-body-function ()
  "org-drafts-change-alt should call `org-drafts-alt-task-body-function'."
  (let ((alt-called nil))
    (org-drafts-test--with-capture-buffer
        "* DRAFT [2025-07-14 Mon]\nBody\n"
      (let ((org-drafts-alt-task-body-function
             (lambda (_h _b _e) (setq alt-called t))))
        (org-drafts-change-alt "TODO")))
    (should alt-called)))

(ert-deftest org-drafts-test-change-alt-errors-when-unset ()
  "org-drafts-change-alt should error if no alt body function is set."
  (org-drafts-test--with-capture-buffer
      "* DRAFT Test\nBody\n"
    (let ((org-drafts-alt-task-body-function nil))
      (should-error (org-drafts-change-alt "TODO") :type 'user-error))))

(ert-deftest org-drafts-test-change-alt-changes-keyword ()
  "org-drafts-change-alt should change DRAFT to the new keyword."
  (org-drafts-test--with-capture-buffer
      "* DRAFT Test\nBody\n"
    (let ((org-drafts-alt-task-body-function (lambda (_h _b _e) nil)))
      (org-drafts-change-alt "NOTE"))
    (goto-char (point-min))
    (should (looking-at-p "^\\* NOTE "))))

;;; ---- Tests for org-drafts--collect-draft-markers ----

(ert-deftest org-drafts-test-collect-markers-empty ()
  "An empty buffer should produce no markers."
  (org-drafts-test--with-org-buffer ""
    (let ((markers (org-drafts--collect-draft-markers
                    (point-min) (point-max))))
      (should (null markers)))))

(ert-deftest org-drafts-test-collect-markers-only-drafts ()
  "Should collect a marker at the start of each DRAFT heading."
  (org-drafts-test--with-org-buffer
      "* DRAFT One\nBody one\n* DRAFT Two\nBody two\n* DRAFT Three\nBody three\n"
    (let ((markers (org-drafts--collect-draft-markers
                    (point-min) (point-max))))
      (unwind-protect
          (progn
            (should (= 3 (length markers)))
            (dolist (m markers)
              (should (markerp m))
              (goto-char m)
              (should (looking-at-p "^\\* DRAFT "))))
        (dolist (m markers) (set-marker m nil))))))

(ert-deftest org-drafts-test-collect-markers-skips-non-drafts ()
  "Non-DRAFT entries should not appear in the marker list."
  (org-drafts-test--with-org-buffer
      "* DRAFT First\n* TODO Second\n* DRAFT Third\n* NOTE Fourth\n* SCRAP Fifth\n"
    (let ((markers (org-drafts--collect-draft-markers
                    (point-min) (point-max))))
      (unwind-protect
          (progn
            (should (= 2 (length markers)))
            (goto-char (nth 0 markers))
            (should (looking-at-p "^\\* DRAFT First"))
            (goto-char (nth 1 markers))
            (should (looking-at-p "^\\* DRAFT Third")))
        (dolist (m markers) (set-marker m nil))))))

(ert-deftest org-drafts-test-collect-markers-respects-bounds ()
  "Only DRAFT entries between BEG and END should be collected."
  (org-drafts-test--with-org-buffer
      "* DRAFT First\nBody\n* DRAFT Second\nBody\n* DRAFT Third\nBody\n"
    (goto-char (point-min))
    (forward-line 2) ;; past first heading and its body
    (let* ((beg (point))
           (end (save-excursion (forward-line 2) (point)))
           (markers (org-drafts--collect-draft-markers beg end)))
      (unwind-protect
          (progn
            (should (= 1 (length markers)))
            (goto-char (car markers))
            (should (looking-at-p "^\\* DRAFT Second")))
        (dolist (m markers) (set-marker m nil))))))

(ert-deftest org-drafts-test-collect-markers-nested-levels ()
  "Markers should be collected for DRAFTs at any heading level."
  (org-drafts-test--with-org-buffer
      "* DRAFT Top\n** DRAFT Nested\n*** DRAFT Deeply\n"
    (let ((markers (org-drafts--collect-draft-markers
                    (point-min) (point-max))))
      (unwind-protect
          (should (= 3 (length markers)))
        (dolist (m markers) (set-marker m nil))))))

;;; ---- Tests for org-drafts-for-each-draft-in-region ----

(ert-deftest org-drafts-test-for-each-iterates ()
  "ACTION-FN should be called once per DRAFT entry."
  (org-drafts-test--with-org-buffer
      "* DRAFT One\nBody\n* DRAFT Two\nBody\n* DRAFT Three\nBody\n"
    (let ((count 0))
      (org-drafts-for-each-draft-in-region
       (point-min) (point-max)
       (lambda () (cl-incf count)))
      (should (= 3 count)))))

(ert-deftest org-drafts-test-for-each-passes-point-at-heading ()
  "ACTION-FN should be invoked with point at the heading start."
  (org-drafts-test--with-org-buffer
      "* DRAFT Alpha\n* DRAFT Beta\n"
    (let (titles)
      (org-drafts-for-each-draft-in-region
       (point-min) (point-max)
       (lambda ()
         (push (buffer-substring (point) (line-end-position)) titles)))
      (setq titles (nreverse titles))
      (should (equal titles '("* DRAFT Alpha" "* DRAFT Beta"))))))

(ert-deftest org-drafts-test-for-each-skips-non-drafts ()
  "Non-DRAFT entries should not be visited."
  (org-drafts-test--with-org-buffer
      "* TODO First\n* DRAFT Second\n* NOTE Third\n"
    (let (visited)
      (org-drafts-for-each-draft-in-region
       (point-min) (point-max)
       (lambda ()
         (push (buffer-substring (point) (line-end-position)) visited)))
      (should (equal visited '("* DRAFT Second"))))))

(ert-deftest org-drafts-test-for-each-empty-region ()
  "Iteration over a region with no DRAFTs should be a no-op."
  (org-drafts-test--with-org-buffer
      "* TODO Just a todo\n* NOTE Just a note\n"
    (let ((called nil))
      (org-drafts-for-each-draft-in-region
       (point-min) (point-max)
       (lambda () (setq called t)))
      (should-not called))))

(ert-deftest org-drafts-test-for-each-converts-all ()
  "Calling org-drafts-change inside the action should rewrite each heading."
  (org-drafts-test--with-org-buffer
      "* DRAFT One\nBody\n* DRAFT Two\nMore\n* DRAFT Three\nLast\n"
    (let ((org-capture-mode nil)
          (org-drafts-task-body-function (lambda (_h _b _e) nil)))
      (org-drafts-for-each-draft-in-region
       (point-min) (point-max)
       (lambda () (org-drafts-change "TODO"))))
    (goto-char (point-min))
    (let ((todo-count 0))
      (while (re-search-forward "^\\* TODO " nil t)
        (cl-incf todo-count))
      (should (= 3 todo-count)))
    (goto-char (point-min))
    (should-not (re-search-forward "^\\* DRAFT " nil t))))

(ert-deftest org-drafts-test-for-each-releases-markers ()
  "Markers returned by collection should be released after iteration."
  (org-drafts-test--with-org-buffer
      "* DRAFT One\n* DRAFT Two\n"
    (let (captured)
      (cl-letf* ((orig-collect
                  (symbol-function 'org-drafts--collect-draft-markers))
                 ((symbol-function 'org-drafts--collect-draft-markers)
                  (lambda (b e)
                    (let ((ms (funcall orig-collect b e)))
                      (setq captured ms)
                      ms))))
        (org-drafts-for-each-draft-in-region
         (point-min) (point-max)
         #'ignore))
      (should captured)
      (dolist (m captured)
        (should-not (marker-position m))
        (should-not (marker-buffer m))))))

(ert-deftest org-drafts-test-for-each-releases-markers-on-error ()
  "Markers should still be released when ACTION-FN signals an error."
  (org-drafts-test--with-org-buffer
      "* DRAFT One\n* DRAFT Two\n"
    (let (captured)
      (cl-letf* ((orig-collect
                  (symbol-function 'org-drafts--collect-draft-markers))
                 ((symbol-function 'org-drafts--collect-draft-markers)
                  (lambda (b e)
                    (let ((ms (funcall orig-collect b e)))
                      (setq captured ms)
                      ms))))
        (should-error
         (org-drafts-for-each-draft-in-region
          (point-min) (point-max)
          (lambda () (error "boom")))))
      (should captured)
      (dolist (m captured)
        (should-not (marker-position m))
        (should-not (marker-buffer m))))))

;;; ---- Tests for org-drafts--dispatch ----

(ert-deftest org-drafts-test-dispatch-without-region ()
  "Without `org-drafts--region-bounds', BODY runs once at point."
  (let ((count 0))
    (let ((org-drafts--region-bounds nil))
      (org-drafts--dispatch (cl-incf count)))
    (should (= 1 count))))

(ert-deftest org-drafts-test-dispatch-with-region ()
  "With `org-drafts--region-bounds' set, BODY runs once per DRAFT in region."
  (org-drafts-test--with-org-buffer
      "* DRAFT A\n* DRAFT B\n* TODO C\n* DRAFT D\n"
    (let ((count 0)
          (org-drafts--region-bounds
           (cons (copy-marker (point-min))
                 (copy-marker (point-max)))))
      (unwind-protect
          (progn
            (org-drafts--dispatch (cl-incf count))
            (should (= 3 count)))
        (set-marker (car org-drafts--region-bounds) nil)
        (set-marker (cdr org-drafts--region-bounds) nil)))))

;;; ---- Tests for org-drafts--release-region-bounds ----

(ert-deftest org-drafts-test-release-clears-and-releases ()
  "Releasing should null the variable and the markers."
  (let* ((m1 (copy-marker (point-min)))
         (m2 (copy-marker (point-max)))
         (org-drafts--region-bounds (cons m1 m2)))
    (org-drafts--release-region-bounds)
    (should-not org-drafts--region-bounds)
    (should-not (marker-position m1))
    (should-not (marker-position m2))))

(ert-deftest org-drafts-test-release-noop-when-nil ()
  "Releasing when nothing is bound is a no-op."
  (let ((org-drafts--region-bounds nil))
    (org-drafts--release-region-bounds)
    (should-not org-drafts--region-bounds)))

;;; ---- Tests for org-drafts-act-on-region ----

(ert-deftest org-drafts-test-act-on-region-binds-bounds ()
  "Hydra body should see `org-drafts--region-bounds' set to the region.
Cleanup is the responsibility of the hydra's :after-exit hook in real
use; this test stubs the hydra body and inspects the variable mid-flight."
  (org-drafts-test--with-org-buffer
      "* DRAFT One\n* DRAFT Two\n"
    (let ((seen-bounds nil)
          (org-drafts--region-bounds nil))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'org-drafts/body)
                       (lambda ()
                         (setq seen-bounds
                               (and org-drafts--region-bounds
                                    (cons (marker-position
                                           (car org-drafts--region-bounds))
                                          (marker-position
                                           (cdr org-drafts--region-bounds))))))))
              (let ((transient-mark-mode t))
                (set-mark (point-min))
                (goto-char (point-max))
                (org-drafts-act-on-region (point-min) (point-max))))
            (should seen-bounds)
            (should (= (car seen-bounds) (point-min)))
            (should (= (cdr seen-bounds) (point-max))))
        (org-drafts--release-region-bounds)))))

(ert-deftest org-drafts-test-act-on-region-deactivates-mark ()
  "The mark should be deactivated before the hydra opens."
  (org-drafts-test--with-org-buffer
      "* DRAFT One\n* DRAFT Two\n"
    (let ((mark-active-during nil)
          (org-drafts--region-bounds nil))
      (unwind-protect
          (cl-letf (((symbol-function 'org-drafts/body)
                     (lambda ()
                       (setq mark-active-during (region-active-p)))))
            (let ((transient-mark-mode t))
              (set-mark (point-min))
              (goto-char (point-max))
              (org-drafts-act-on-region (point-min) (point-max)))
            (should-not mark-active-during))
        (org-drafts--release-region-bounds)))))

(ert-deftest org-drafts-test-act-on-region-releases-stale-bounds ()
  "A second call should release any pre-existing bounds."
  (org-drafts-test--with-org-buffer
      "* DRAFT One\n* DRAFT Two\n"
    (let* ((stale-m1 (copy-marker (point-min)))
           (stale-m2 (copy-marker (point-max)))
           (org-drafts--region-bounds (cons stale-m1 stale-m2)))
      (unwind-protect
          (cl-letf (((symbol-function 'org-drafts/body) #'ignore))
            (let ((transient-mark-mode t))
              (set-mark (point-min))
              (goto-char (point-max))
              (org-drafts-act-on-region (point-min) (point-max)))
            (should-not (marker-position stale-m1))
            (should-not (marker-position stale-m2))
            (should org-drafts--region-bounds))
        (org-drafts--release-region-bounds)))))

(ert-deftest org-drafts-test-act-on-region-no-region-errors ()
  "Calling without an active region should signal an error.
The (interactive \"r\") form itself raises \"The mark is not set\" when
the mark is unset, so this test only requires that *some* error is
signaled rather than asserting a specific type."
  (org-drafts-test--with-org-buffer "* DRAFT Test\n"
    (deactivate-mark)
    (should-error (call-interactively #'org-drafts-act-on-region))))

(ert-deftest org-drafts-test-act-on-region-converts-all ()
  "End-to-end: all DRAFTs in the region should change to TODO."
  (org-drafts-test--with-org-buffer
      "* DRAFT One\nBody1\n* DRAFT Two\nBody2\n* DRAFT Three\nBody3\n"
    (let ((org-capture-mode nil)
          (org-drafts-task-body-function (lambda (_h _b _e) nil))
          (org-drafts--region-bounds nil))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'org-drafts/body)
                       (lambda ()
                         (org-drafts--dispatch
                           (org-drafts-change "TODO")))))
              (let ((transient-mark-mode t))
                (set-mark (point-min))
                (goto-char (point-max))
                (org-drafts-act-on-region (point-min) (point-max))))
            (goto-char (point-min))
            (should-not (re-search-forward "^\\* DRAFT " nil t))
            (goto-char (point-min))
            (let ((todo-count 0))
              (while (re-search-forward "^\\* TODO " nil t)
                (cl-incf todo-count))
              (should (= 3 todo-count))))
        (org-drafts--release-region-bounds)))))

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
