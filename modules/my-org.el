;;;; my-org.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;; Org is useful. I am committing to use it more.

;;; Code:
(use-package org
  :ensure nil
  :defer t
  :config
  (setq org-directory (file-truename "~/org")
        org-agenda-files (directory-files-recursively org-directory "\\.org$")
        org-default-notes-file (concat org-directory "/notes.org")
        org-return-follows-link t
        ;; This results in prettier content without unnecessary spaces in files
        ;; https://www.reddit.com/r/emacs/comments/97naje/comment/e4a6qqt
        org-indent-indentation-per-level 0
        org-startup-indented t
        ;; Add toggling states and different states
        ;; e.g <SPC> m t t => TODO
        ;; e.g <SPC> m t d => Done
        ;; These key binds will conflict with meow, where <SPC> m becomes M-
        ;; This is achieved with org-todo and having a (letter) at the end of the keyword.
        ;; (setq org-todo-keywords
        ;;      '((sequence "TODO" "WAIT" "|" "DONE" "CANCELLED")))
        org-todo-keywords '((sequence
                             "TODO(t)"  ; A task that needs doing & is ready to do
                             "WAIT(w)"  ; Something external is holding up this task
                             "HOLD(h)"  ; This task is paused/on hold because of me
                             "IDEA(i)"  ; An unconfirmed and unapproved task or notion
                             "|"
                             "DONE(d)"  ; Task successfully completed
                             "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
                            (sequence
                             "[ ](T)"   ; A task that needs doing
                             "[-](S)"   ; Task is in progress
                             "[?](W)"   ; Task is being held up or paused
                             "|"
                             "[X](D)")) ; Task was completed
        ;; Ensure that a heading and it's contents are automatically aligned
        org-adapt-indentation t
        org-log-done 'time

        org-capture-templates '(("t" "Task" entry (file+headline org-default-notes-file "Tasks") "* TODO %?\n  %T")
                                ("a" "Emacs Annoyance" entry (file+headline "~/org/emacs.org" "Annoyances") "* TODO %?\n  %T")
                                ("i" "Emacs Improvement" entry (file+headline "~/org/emacs.org" "Sharp tools") "* TODO %?\n  %T")
                                ("r" "Reading" entry (file "~/org/reading.org") "* %?\n")
                                ("w" "Log work from home" plain
                                 (file+olp+datetree "~/org/wfh-diary.org")
                                 ":LOGBOOK:\nCLOCK: [%<%F %a> 09:00]--[%<%F %a> 16:30] =>  7:30\n:END:\n" :time-prompt t :immediate-finish t)
                                ("W" "Log work from home for today" plain
                                 (file+olp+datetree "~/org/wfh-diary.org")
                                 ":LOGBOOK:\nCLOCK: [%<%F %a> 09:00]--[%<%F %a> 16:30] =>  7:30\n:END:\n" :time-prompt nil :immediate-finish t))
        )
  (add-hook 'org-mode-hook 'turn-on-auto-fill))

;; Org Roam doesn't work for me.
;; (use-package org-roam
;;   :ensure t
;;   :defer t
;;   :custom
;;   (org-roam-directory org-directory)
;;   :config
;;   ;; If you're using a vertical completion framework, you might want a more informative completion interface
;;   (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
;;   (org-roam-db-autosync-mode)
;;   ;; If using org-roam-protocol
;;   (require 'org-roam-protocol))

;; Evil Key Binds for Org Mode
;; https://github.com/Somelauw/evil-org-mode
;;(use-package evil-org
;;  :ensure t
;;  :defer t
;;  :after org
;;  :hook (org-mode . (lambda () evil-org-mode))
;;  :config
;;  (require 'evil-org-agenda)
;;  (evil-org-agenda-set-keys))

;;; Package:
(provide 'my-org)
;;; my-org.el ends here
