;;; org-drafts --- Manage drafts using Org-capture -*- lexical-binding: t -*-

;; Copyright (C) 2011 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Created: 14 Jul 2025
;; Version: 1.0
;; Keywords: org capture task todo context
;; X-URL: https://github.com/jwiegley/dot-emacs

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
(require 'ox-slack)
(require 'pretty-hydra)

(defgroup org-drafts nil
  "Capture drafts that begin in the Org-capture buffer."
  :group 'org)

(cl-defun org-drafts-change (keyword)
  "Change the heading keyword of a draft entry to KEYWORD.
This function changes the heading keyword of the current Org-capture buffer
and finalizes the capture if in capture mode."
  (interactive)
  (goto-char (point-min))
  (re-search-forward "^\\*+ \\(DRAFT\\|SCRAP\\) ")
  (replace-match keyword t t nil 1)
  (when org-capture-mode
    (org-capture-finalize current-prefix-arg)))

(defvar org-drafts--text
  "Store the text content of the current Org draft.")

(defun org-drafts-with (at-heading-func at-capture-end-func body-func)
  "Execute BODY-FUNC with the draft content, modifying it based on heading.
AT-HEADING-FUNC is called at the heading position.
AT-CAPTURE-END-FUNC is called at the end of the capture process.
This function handles the text content of the draft between the heading
and the next heading or end of buffer."
  (save-excursion
    (org-back-to-heading-or-point-min)
    (funcall at-heading-func)
    (forward-line)
    (when (looking-at-p ":PROPERTIES:")
      (re-search-forward ":END:")
      (forward-line))
    (save-restriction
      (narrow-to-region (point)
                        (if org-capture-mode
                            (point-max)
                          (save-excursion
                            (org-next-visible-heading 1)
                            (point))))
      (prog1
          (funcall body-func (string-trim (buffer-string)))
        (when org-capture-mode
          (funcall at-capture-end-func))))))

(defun org-drafts-with-change-to (keyword body-func)
  "Process draft content changing the heading to KEYWORD.
BODY-FUNC is called to process the content.
This function is used to change a draft's heading keyword and process
its content."
  (declare (indent 1))
  (org-drafts-with
    (lambda ()
      (when (looking-at "^\\*+ \\(DRAFT\\|SCRAP\\) ")
        (replace-match keyword t t nil 1)))
    (lambda ()
      (org-capture-finalize current-prefix-arg))
    body-func))

(defsubst org-drafts-change (keyword)
  "Call `org-drafts-with-change-to' but with no body function.
This results in only the KEYWORD being changed."
  (org-drafts-with-change-to keyword #'ignore))

(defun org-drafts-copy-to-clipboard (&optional format)
  "Copy draft content to clipboard, changing heading to SCRAP.
FORMAT is an optional format to use for the copy operation.
This function copies the draft content to the clipboard, optionally
formatting it."
  (interactive)
  (org-drafts-with-change-to "SCRAP"
    (lambda (str)
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
           str))))))

(defun org-drafts-gptel ()
  "Send draft to GPTel chat buffer.
Changes heading to SCRAP before sending.
Creates a new GPTel chat buffer with the draft content and sends it."
  (interactive)
  (org-drafts-with-change-to "SCRAP"
    (lambda (str)
      (with-current-buffer (gptel "chat_buffer_name" nil str)
        (pop-to-buffer (current-buffer))
        (gptel-send)))))

(defun org-drafts-kagi ()
  "Submit draft contents as a search query to Kagi.
Changes heading to SCRAP before sending query."
  (interactive)
  (org-drafts-with-change-to "SCRAP"
    (lambda (str) (browse-url (concat "https://kagi.com/search?q=" str)))))

(defun org-drafts-perplexity ()
  "Submit draft contents as a search query to Perplexity.ai.
Changes heading to SCRAP before sending query."
  (interactive)
  (org-drafts-with-change-to "SCRAP"
    (lambda (str) (browse-url (concat "https://www.perplexity.ai/search/?q=" str
                                 "&copilot=true")))))

(defun org-drafts-email ()
  "Create a new email with the draft content.
Changes heading to SCRAP before creating email.
Uses Gnus mail user agent to compose a new email with the draft content."
  (interactive)
  (org-drafts-with-change-to "SCRAP"
    (lambda (str)
      (let ((mail-user-agent 'gnus-user-agent))
        (compose-mail)
        (message-goto-body)
        (insert str)))))

(pretty-hydra-define org-drafts
  (:color teal :quit-key "q")
  ("Org"
   (("N"   (org-drafts-change "NOTE") "NOTE")
    ("T"   (org-drafts-change "TODO") "TODO")
    ("d"   org-capture-finalize "DRAFT")
    ("C-c" org-capture-finalize "DRAFT"))
   "Utils"
   (("c"   org-drafts-copy-to-clipboard "Copy")
    ("W"   (org-drafts-copy-to-clipboard "whatsapp") "WhatsApp")
    ("G"   (org-drafts-copy-to-clipboard "github") "GitHub")
    ("M"   (org-drafts-copy-to-clipboard "markdown") "Markdown")
    ("T"   (org-drafts-copy-to-clipboard "telegram") "Telegram")
    ("s"   (org-drafts-copy-to-clipboard "ox-slack") "Slack")
    ("S"   (org-drafts-copy-to-clipboard "slack") "Slack (code)"))
   "Other"
   (("k"   org-drafts-kagi "Kagi")
    ("g"   org-drafts-gptel "GPTel")
    ("p"   org-drafts-perplexity "Perplexity")
    ("C-s" org-drafts-perplexity "Perplexity")
    ("m"   org-drafts-email "Email"))
  "This hydra menu provides quick actions for handling Org drafts"))

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
