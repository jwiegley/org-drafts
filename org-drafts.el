;;; org-drafts.el --- Manage drafts using Org-capture -*- lexical-binding: t -*-

;; Copyright (C) 2011 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Created: 14 Jul 2025
;; Version: 1.0
;; Package-Requires: ((emacs "29.1") (org "9.0") (copy-as-format "0.0.8") (pretty-hydra "0.2.2"))
;; Keywords: outlines convenience
;; URL: https://github.com/jwiegley/dot-emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; I use the following Org-capture template:
;;
;;   ("d" "DRAFT" entry
;;     "path-to-inbox-file.org"
;;     "* DRAFT %U\n%?"
;;     :prepend t):
;;
;; With the following use-package declaration:
;;
;;   (use-package org-drafts
;;     :after (org)
;;     :bind* ("M-M" . (lambda () (interactive) (org-capture nil "d")))
;;     :config
;;     (org-drafts-install))

;;; Code:

(require 'cl-lib)
(require 'org-macs)
(require 'org-capture)
(require 'copy-as-format)
;;(require 'ox-slack)
(require 'pretty-hydra)

(declare-function gptel "gptel")
(declare-function gptel-request "gptel")
(declare-function gptel-send "gptel")
(declare-function org-slack-export-to-clipboard-as-slack "ox-slack")
(declare-function message-goto-body "message")

(defgroup org-drafts nil
  "Capture drafts that begin in the Org-capture buffer."
  :group 'org)

(defcustom org-drafts-task-body-function
  #'org-drafts-default-body-function
  "Function to use for processing TODO/NOTE bodies.
Bound to keys n and t in the org-drafts hydra."
  :type 'function
  :group 'org-drafts)

(defcustom org-drafts-alt-task-body-function nil
  "Alternative body function for TODO/NOTE bodies.
When non-nil, the org-drafts hydra exposes capital N and T keys that
call `org-drafts-change-alt' using this function instead of
`org-drafts-task-body-function'.  This lets both behaviors stay
available simultaneously: for example, the default behavior of moving
the first body line into the heading title (bound to lowercase n and
t) and an alternate behavior such as LLM-based title synthesis (bound
to capital N and T)."
  :type '(choice (const :tag "Disabled" nil) function)
  :group 'org-drafts)

(defcustom org-drafts-after-state-change-function nil
  "Hook which is run after the state of a TODO item was changed."
  :type 'hook
  :group 'org-drafts)

(defvar org-drafts--text
  "Store the text content of the current Org draft.")

(defun org-drafts-default-body-function (heading-pos beg end)
  "Default body function for tasks is to make the body be the title.
HEADING-POS is a marker set to the beginning of the heading line.
BEG and END are markers covering the range of the body text."
  (with-restriction heading-pos end
    (save-excursion
      (goto-char (point-min))
      (goto-char (line-end-position))
      (backward-kill-sexp)
      (insert (string-trim
               (buffer-substring beg
                                 (save-excursion
                                   (goto-char beg)
                                   (line-end-position)))))
      (goto-char beg)
      (delete-region (point)
                     (min (1+ (line-end-position)) (point-max))))))

(defun org-drafts-with (at-heading-func at-capture-end-func body-func)
  "Execute BODY-FUNC with the draft content, modifying it based on heading.
AT-HEADING-FUNC is called at the heading position.
AT-CAPTURE-END-FUNC is called at the end of the capture process.
This function handles the text content of the draft between the heading
and the next heading or end of buffer."
  (save-excursion
    (org-back-to-heading-or-point-min)
    (let ((heading-pos (point-marker)))
      (funcall at-heading-func)
      (forward-line)
      (when (looking-at-p ":PROPERTIES:")
        (re-search-forward ":END:")
        (forward-line))
      (prog1
          (funcall body-func
                   heading-pos
                   (point-marker)
                   (if org-capture-mode
                       (point-max-marker)
                     (save-excursion
                       (org-next-visible-heading 1)
                       (point-marker))))
        (when org-capture-mode
          (funcall at-capture-end-func))))))

(defun org-drafts-with-change-to (keyword body-func)
  "Process draft content changing the heading to KEYWORD.
BODY-FUNC is called to process the content.
This function is used to change a draft's heading keyword and process
its content."
  (declare (indent 1))
  (org-drafts-with
      (lambda () (when (looking-at "^\\*+ \\(DRAFT\\|SCRAP\\) ")
                   (replace-match keyword t t nil 1)
                   (run-hooks 'org-drafts-after-state-change-function)))
      (lambda ()
        (run-hooks 'org-after-todo-state-change-hook))
    body-func))

(defsubst org-drafts-change (keyword)
  "Call `org-drafts-with-change-to' but with no body function.
This results in only the KEYWORD being changed."
  (org-drafts-with-change-to keyword org-drafts-task-body-function))

(defun org-drafts-change-alt (keyword)
  "Change the draft to KEYWORD using `org-drafts-alt-task-body-function'.
Signals a `user-error' if no alternate body function has been
configured."
  (unless org-drafts-alt-task-body-function
    (user-error "`org-drafts-alt-task-body-function' is not set"))
  (org-drafts-with-change-to keyword org-drafts-alt-task-body-function))

(defvar org-drafts--region-bounds nil
  "If non-nil, a (BEG . END) cons of buffer positions or markers.
When set, the org-drafts hydra commands operate on every DRAFT entry
between BEG and END instead of just the entry at point.  Set by
`org-drafts-act-on-region' before invoking the hydra and cleared by
`org-drafts--release-region-bounds' from the hydra's :after-exit hook.")

(defun org-drafts--release-region-bounds ()
  "Release any markers in `org-drafts--region-bounds' and clear it.
Intended to be called from the hydra's :after-exit hook so that
`org-drafts-act-on-region' can leak no markers regardless of how the
hydra exits."
  (when org-drafts--region-bounds
    (let ((b (car org-drafts--region-bounds))
          (e (cdr org-drafts--region-bounds)))
      (setq org-drafts--region-bounds nil)
      (when (markerp b) (set-marker b nil))
      (when (markerp e) (set-marker e nil)))))

(defun org-drafts--collect-draft-markers (beg end)
  "Return a list of markers at the start of each DRAFT heading.
The markers cover the buffer range from BEG to END.  Markers are
returned in the order in which the DRAFT headings appear.  Heading
detection is case-sensitive so that lowercase \"draft\" inside text is
not matched."
  (save-excursion
    (let ((case-fold-search nil)
          acc)
      (goto-char beg)
      (while (re-search-forward "^\\*+ DRAFT " end t)
        (when (save-match-data
                (save-excursion
                  (beginning-of-line)
                  (org-at-heading-p)))
          (beginning-of-line)
          (push (copy-marker (point)) acc)
          (forward-line 1)))
      (nreverse acc))))

(defun org-drafts-for-each-draft-in-region (beg end action-fn)
  "Run ACTION-FN at each DRAFT entry heading between BEG and END.
ACTION-FN is a zero-argument function invoked with point at the start
of the heading line of each DRAFT entry.  Markers for every DRAFT
heading are collected before iteration begins, so modifications made by
ACTION-FN do not affect which entries are visited.  The markers are
released after iteration completes, even if ACTION-FN signals an error."
  (let ((markers (org-drafts--collect-draft-markers beg end)))
    (unwind-protect
        (dolist (m markers)
          (when (marker-position m)
            (save-excursion
              (goto-char m)
              (funcall action-fn))))
      (dolist (m markers)
        (set-marker m nil)))))

(defmacro org-drafts--dispatch (&rest body)
  "Evaluate BODY at point, or once at each DRAFT in a stored region.
When `org-drafts--region-bounds' is non-nil, BODY is evaluated at each
DRAFT entry heading between its car and cdr.  Otherwise BODY is
evaluated once at point."
  (declare (indent 0) (debug (body)))
  `(if org-drafts--region-bounds
       (org-drafts-for-each-draft-in-region
        (car org-drafts--region-bounds)
        (cdr org-drafts--region-bounds)
        (lambda () ,@body))
     (progn ,@body)))

(defun org-drafts-copy-to-clipboard (&optional format)
  "Copy draft content to clipboard, changing heading to SCRAP.
FORMAT is an optional format to use for the copy operation.
This function copies the draft content to the clipboard, optionally
formatting it."
  (interactive)
  (org-drafts-with-change-to "SCRAP"
    (lambda (_heading-pos beg end)
      (with-restriction beg end
        (let ((str (string-trim (buffer-string))))
          (goto-char (point-max))
          (while (and (bolp) (not (bobp)))
            (delete-char -1))
          (if (string= "ox-slack" format)
              (org-slack-export-to-clipboard-as-slack)
            (kill-new
             (if format
                 (funcall
                  (cadr (assoc format copy-as-format-format-alist))
                  str
                  (use-region-p))
               str))))))))

(defun org-drafts-gptel ()
  "Send draft to GPTel chat buffer.
Changes heading to SCRAP before sending.
Creates a new GPTel chat buffer with the draft content and sends it."
  (interactive)
  (require 'gptel)
  (org-drafts-with-change-to "SCRAP"
    (lambda (_heading-pos beg end)
      (with-restriction beg end
        (let ((str (string-trim (buffer-string))))
          (with-current-buffer (gptel "chat_buffer_name" nil str)
            (pop-to-buffer (current-buffer))
            (gptel-send)))))))

(defun org-drafts-kagi ()
  "Submit draft contents as a search query to Kagi.
Changes heading to SCRAP before sending query."
  (interactive)
  (org-drafts-with-change-to "SCRAP"
    (lambda (_heading-pos beg end)
      (with-restriction beg end
        (let ((str (string-trim (buffer-string))))
          (with-restriction beg end
            (browse-url (concat "https://kagi.com/search?q="
                                (url-hexify-string str)))))))))

(defun org-drafts-claude ()
  "Submit draft contents as a search query to Perplexity.ai.
Changes heading to SCRAP before sending query."
  (interactive)
  (org-drafts-with-change-to "SCRAP"
    (lambda (_heading-pos beg end)
      (with-restriction beg end
        (let ((str (string-trim (buffer-string))))
          (browse-url (concat "https://claude.ai/new?q="
                              (url-hexify-string str))))))))

(defun org-drafts-perplexity ()
  "Submit draft contents as a search query to Perplexity.ai.
Changes heading to SCRAP before sending query."
  (interactive)
  (org-drafts-with-change-to "SCRAP"
    (lambda (_heading-pos beg end)
      (with-restriction beg end
        (let ((str (string-trim (buffer-string))))
          (browse-url (concat "https://www.perplexity.ai/search/?q="
                              (url-hexify-string str)
                              "&copilot=true")))))))

(defun org-drafts-email ()
  "Create a new email with the draft content.
Changes heading to SCRAP before creating email.
Uses Gnus mail user agent to compose a new email with the draft content."
  (interactive)
  (org-drafts-with-change-to "SCRAP"
    (lambda (_heading-pos beg end)
      (with-restriction beg end
        (let ((str (string-trim (buffer-string))))
          (let ((mail-user-agent 'gnus-user-agent))
            (compose-mail)
            (message-goto-body)
            (insert str)))))))

(defun org-drafts-rewrite ()
  "Rewrite draft content using an LLM via gptel.
Prompts for rewrite instructions, sends the draft body to gptel for
rewriting, then saves the original content, prompt, and rewritten
result in the draft entry.  The rewritten text is pushed to the
kill ring and displayed in a temporary buffer."
  (interactive)
  (require 'gptel)
  (let ((prompt (read-string "Rewrite prompt: ")))
    (when (string-empty-p prompt)
      (user-error "Rewrite prompt cannot be empty"))
    (let ((capture-buf (current-buffer))
          (in-capture org-capture-mode))
      (org-drafts-with
          #'ignore
          #'ignore
        (lambda (heading-pos beg end)
          (let ((original (string-trim
                           (buffer-substring-no-properties beg end)))
                (level (save-excursion
                         (goto-char heading-pos)
                         (org-current-level))))
            (when (string-empty-p original)
              (user-error "Draft body is empty"))
            (message "Rewriting draft with gptel...")
            (gptel-request
             (format "Rewrite the following text according to these \
instructions.  Output ONLY the rewritten text, with no preamble or \
commentary.\n\nInstructions: %s\n\nText to rewrite:\n%s"
                     prompt original)
             :callback
             (lambda (response info)
               (if (not (stringp response))
                   (message "gptel rewrite failed: %s"
                            (plist-get info :status))
                 (let ((result (string-trim response))
                       (sub (make-string (1+ level) ?*)))
                   (kill-new result)
                   (with-current-buffer
                       (get-buffer-create "*Org Draft Rewrite*")
                     (erase-buffer)
                     (insert result)
                     (goto-char (point-min))
                     (display-buffer (current-buffer)))
                   (when (buffer-live-p capture-buf)
                     (with-current-buffer capture-buf
                       (save-excursion
                         (delete-region beg end)
                         (goto-char beg)
                         (insert original "\n"
                                 sub " Rewrite Prompt\n"
                                 prompt "\n"
                                 sub " Rewritten\n"
                                 result "\n"))
                       (when in-capture
                         (org-capture-finalize))))
                   (message "Draft rewritten and saved. \
Result copied to kill ring.")))))))))))

;; pretty-hydra generates wide hint tables as docstrings.
(eval-when-compile
  (setq byte-compile-docstring-max-column 200))

(pretty-hydra-define org-drafts
  (:color teal :quit-key "q"
          :after-exit (org-drafts--release-region-bounds))
  ("Org"
   (("n"   (org-drafts--dispatch (org-drafts-change "NOTE")) "NOTE")
    ("t"   (org-drafts--dispatch (org-drafts-change "TODO")) "TODO")
    ("p"   (org-drafts--dispatch (org-drafts-change "PROMPT")) "PROMPT")
    ("q"   (org-drafts--dispatch (org-drafts-change "QUOTE")) "QUOTE")
    ("N"   (org-drafts--dispatch (org-drafts-change-alt "NOTE"))
     "NOTE (alt)")
    ("T"   (org-drafts--dispatch (org-drafts-change-alt "TODO"))
     "TODO (alt)")
    ("P"   (org-drafts--dispatch (org-drafts-change-alt "PROMPT"))
     "PROMPT (alt)")
    ("Q"   (org-drafts--dispatch (org-drafts-change-alt "QUOTE"))
     "QUOTE (alt)")
    ("d"   org-capture-finalize "DRAFT")
    ("S"   (org-drafts--dispatch (org-drafts-change "SCRAP")) "SCRAP")
    ("C-c" org-capture-finalize "DRAFT"))
   "Utils"
   (("c"   (org-drafts--dispatch (org-drafts-copy-to-clipboard)) "Copy")
    ("M"   (org-drafts--dispatch (org-drafts-copy-to-clipboard "markdown"))
     "Md (code)")
    ("s"   (org-drafts--dispatch (org-drafts-copy-to-clipboard "ox-slack"))
     "Slack"))
   "Other"
   (("g"   (org-drafts--dispatch (org-drafts-gptel)) "GPTel")
    ("C"   (org-drafts--dispatch (org-drafts-claude)) "Claude")
    ("C-s" (org-drafts--dispatch (org-drafts-claude)) "Claude")
    ("m"   (org-drafts--dispatch (org-drafts-email)) "Email")
    ("r"   (org-drafts--dispatch (org-drafts-rewrite)) "Rewrite"))
   "Quick actions for handling Org drafts."))

(defun org-drafts-action (&optional arg)
  "Handle the finalization of a draft.
If the entry is a DRAFT, activate the hydra menu.
Otherwise, finalize the capture normally with optional ARG."
  (interactive "P")
  (if (save-excursion
        (goto-char (point-min))
        (looking-at-p "^\\*+ DRAFT "))
      (org-drafts/body)
    (org-capture-finalize arg)))

(defun org-drafts-act-on-existing ()
  "Activate hydra menu for existing DRAFT or SCRAP entries.
This function is added to `org-ctrl-c-ctrl-c-hook' to provide hydra menu
activation when editing existing DRAFT or SCRAP entries."
  (when (ignore-errors
          (member (org-get-todo-state) '("DRAFT" "SCRAP")))
    (org-drafts/body)
    t))

;;;###autoload
(defun org-drafts-act-on-region (beg end)
  "Activate the org-drafts hydra to act on every DRAFT in the region.
BEG and END are the bounds of the active region.  The selected hydra
action is applied once to each DRAFT entry whose heading line lies
between BEG and END.

The hydra is non-blocking: `pretty-hydra-define' installs a transient
keymap and returns immediately, so cleanup happens later from the
hydra's :after-exit hook (`org-drafts--release-region-bounds').  Any
stale region bounds from a prior invocation are released before the new
ones are stored, so leaks from abnormal exits do not accumulate.

Interactively, BEG and END default to the active region; signals an
error if no region is active."
  (interactive "r")
  (unless (use-region-p)
    (user-error "No active region"))
  (org-drafts--release-region-bounds)
  (deactivate-mark)
  (setq org-drafts--region-bounds
        (cons (copy-marker beg) (copy-marker end)))
  (org-drafts/body))

(defun org-drafts-install ()
  "Install Org-drafts key bindings and hooks.
Sets up the necessary keymap and hooks for Org-drafts functionality."
  (add-hook 'org-ctrl-c-ctrl-c-hook #'org-drafts-act-on-existing)
  (define-key org-capture-mode-map (kbd "C-c C-c") #'org-drafts-action))

(provide 'org-drafts)

;;; org-drafts.el ends here
