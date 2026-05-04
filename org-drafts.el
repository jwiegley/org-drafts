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
  (:color teal :quit-key "q")
  ("Org"
   (("n"   (org-drafts-change "NOTE") "NOTE")
    ("t"   (org-drafts-change "TODO") "TODO")
    ("N"   (org-drafts-change-alt "NOTE") "NOTE (alt)")
    ("T"   (org-drafts-change-alt "TODO") "TODO (alt)")
    ("d"   org-capture-finalize "DRAFT")
    ("S"   (org-drafts-change "SCRAP") "SCRAP")
    ("C-c" org-capture-finalize "DRAFT"))
   "Utils"
   (("c"   org-drafts-copy-to-clipboard "Copy")
    ("M"   (org-drafts-copy-to-clipboard "markdown") "Md (code)")
    ("s"   (org-drafts-copy-to-clipboard "ox-slack") "Slack"))
   "Other"
   (("k"   org-drafts-kagi "Kagi")
    ("g"   org-drafts-gptel "GPTel")
    ("p"   org-drafts-perplexity "Perplexity")
    ("C"   org-drafts-claude "Claude")
    ("C-s" org-drafts-claude "Claude")
    ("m"   org-drafts-email "Email")
    ("r"   org-drafts-rewrite "Rewrite"))
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

(defun org-drafts-install ()
  "Install Org-drafts key bindings and hooks.
Sets up the necessary keymap and hooks for Org-drafts functionality."
  (add-hook 'org-ctrl-c-ctrl-c-hook #'org-drafts-act-on-existing)
  (define-key org-capture-mode-map (kbd "C-c C-c") #'org-drafts-action))

(provide 'org-drafts)

;;; org-drafts.el ends here
