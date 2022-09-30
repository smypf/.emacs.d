;;;; my-org.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;; Org is useful. I am committing to use it more.

;;; Code:

(evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)

(use-package org
  :ensure nil
  :config
  (setq org-directory (file-truename "~/org")
	org-agenda-files (directory-files-recursively org-directory "\\.org$")
	org-default-notes-file (concat org-directory "/notes.org")
	;; Ensure that a heading and it's contents are automatically aligned
	org-adapt-indentation t)
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  :general (
	    :prefix leader
	    :states 'normal
	    :keymaps 'override
	    "N" 'org-capture
	    "na" 'org-agenda
	    "nt" 'org-todo))
;; Add toggling states and different states
;; e.g <SPC> m t t => TODO
;; e.g <SPC> m t d => Done
;; This is achieved with org-todo and having a (letter) at the end of the keyword.
;; (setq org-todo-keywords
;;      '((sequence "TODO" "WAIT" "|" "DONE" "CANCELLED")))

(setq org-todo-keywords
      '((sequence
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
	 "[X](D)")))  ; Task was completed


(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory org-directory)
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix leader
 "nl" 'org-roam-buffer-toggle
 "nf" 'org-roam-node-find
 "ng" 'org-roam-graph
 "ni" 'org-roam-node-insert
 "nc" 'org-roam-capture
 ;; Dailies
 "nj" 'org-roam-dailies-capture-today)

;; Evil Key Binds for Org Mode
;; https://github.com/Somelauw/evil-org-mode
(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;;; Package:
(provide 'my-org)
;;; my-org.el ends here