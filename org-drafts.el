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

(require 'cl-lib)
(require 'org-macs)
(require 'org-capture)
(require 'pretty-hydra)

(defgroup org-drafts nil
  "Capture drafts that begin in the Org-capture buffer."
  :group 'org)

(cl-defun org-drafts-change (keyword)
  (interactive)
  (goto-char (point-min))
  (re-search-forward "^\\*+ \\(DRAFT\\|SCRAP\\) ")
  (replace-match keyword t t nil 1)
  (when org-capture-mode
    (org-capture-finalize current-prefix-arg)))

(defvar org-drafts--text)

(defun org-drafts-with (at-heading-func at-capture-end-func body-func)
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
  (declare (indent 1))
  (org-drafts-with
    (lambda ()
      (when (looking-at "^\\*+ \\(DRAFT\\|SCRAP\\) ")
        (replace-match keyword t t nil 1)))
    (lambda ()
      (org-capture-finalize current-prefix-arg))
    body-func))

(defsubst org-drafts-change (keyword)
  (org-drafts-with-change-to keyword #'ignore))

(defun org-drafts-copy-to-clipboard (&optional format)
  (interactive)
  (org-drafts-with-change-to "SCRAP"
    (lambda (str)
      (kill-new
       (if format
           (funcall
            (cadr (assoc format copy-as-format-format-alist))
            str
            (use-region-p))
         str)))))

(defun org-drafts-gptel ()
  (interactive)
  (org-drafts-with-change-to "SCRAP"
    (lambda (str)
      (with-current-buffer (gptel "chat_buffer_name" nil str)
        (pop-to-buffer (current-buffer))
        (gptel-send)))))

(defun org-drafts-kagi ()
  (interactive)
  (org-drafts-with-change-to "SCRAP"
    (lambda (str) (browse-url (concat "https://kagi.com/search?q=" str)))))

(defun org-drafts-perplexity ()
  (interactive)
  (org-drafts-with-change-to "SCRAP"
    (lambda (str) (browse-url (concat "https://www.perplexity.ai/search/?q=" str
                                 "&copilot=true")))))

(pretty-hydra-define
  org-drafts
  (:color teal :quit-key "q")
  ("Org"
   (("N"   (org-drafts-change "NOTE") "NOTE")
    ("T"   (org-drafts-change "TODO") "TODO")
    ("d"   org-capture-finalize "DRAFT")
    ("C-c" org-capture-finalize "DRAFT"))
   "Utils"
   (("c"   org-drafts-copy-to-clipboard "Copy")
    ("w"   (org-drafts-copy-to-clipboard "whatsapp") "Copy")
    ("g"   (org-drafts-copy-to-clipboard "github") "Copy")
    ("m"   (org-drafts-copy-to-clipboard "markdown") "Copy")
    ("t"   (org-drafts-copy-to-clipboard "telegram") "Copy")
    ("s"   (org-drafts-copy-to-clipboard "slack") "Copy")
    ("k"   org-drafts-kagi "Kagi"))
   "AI"
   (("g"   org-drafts-gptel "GPTel")
    ("p"   org-drafts-perplexity "Perplexity")
    ("C-s" org-drafts-perplexity "Perplexity"))))

(defun org-drafts-action (&optional arg)
  (interactive "P")
  (if (save-excursion
        (goto-char (point-min))
        (looking-at-p "^\\*+ DRAFT "))
      (org-drafts/body)
    (org-capture-finalize arg)))

(defun org-drafts-act-on-existing ()
  (when (member (org-get-todo-state) '("DRAFT" "SCRAP"))
    (org-drafts/body)
    t))

(defun org-drafts-install ()
  (add-hook 'org-ctrl-c-ctrl-c-hook #'org-drafts-act-on-existing)
  (define-key org-capture-mode-map (kbd "C-c C-c") #'org-drafts-action))

(provide 'org-drafts)

;;; org-drafts.el ends here
