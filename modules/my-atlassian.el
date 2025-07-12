;;;; my-atlassian.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;;

;;; Code:

;; https://atlassian.slack.com/archives/CFJJ2E96C/p1721828999932499
(dir-locals-set-class-variables
 'huge-git-repository
 '((nil . ((magit-refresh-buffers . nil)))
   (magit-status-mode
	. ((eval . (magit-disable-section-inserter 'magit-insert-tags-header))
	   ;;(eval . (magit-disable-section-inserter 'magit-insert-status-headers))
	   ;; This was 6+ seconds for this one!!! (Something has changed)
	   ;;(eval . (magit-disable-section-inserter 'magit-insert-untracked-files))
	   (eval . (magit-disable-section-inserter 'magit-insert-unpulled-from-upstream))
	   (eval . (magit-disable-section-inserter 'magit-insert-unpushed-to-upstream-or-recent))
	   (eval . (magit-disable-section-inserter 'magit-insert-unpushed-to-pushremote))
	   (eval . (magit-disable-section-inserter 'magit-insert-unpulled-from-pushremote))
	   ))))

(dir-locals-set-directory-class
 (expand-file-name "~/atlassian/afm") 'huge-git-repository)

(defun smypf/enable-indent-tabs-mode-in-atlassian-folder ()
  "Enable indent-tabs-mode only for files in ~/atlassian folder."
  (let ((file-name (buffer-file-name)))
	(when (and file-name
			   (string-prefix-p (expand-file-name "~/atlassian/afm") file-name)
			   (string-match-p "\\.tsx?$" file-name))
	  (not (string-match-p "node_modules" file-name))
	  (not (string-match-p "\\.d\\.ts$" file-name))
	  (not (string-match-p "\\.rs$" file-name)))
	;; Only enable these modes for TypeScript files
	(setq-local indent-tabs-mode t)
	(setq-local apheleia-mode t)))

(use-package emacs
  :ensure nil
  :hook
  (prog-mode . smypf/enable-indent-tabs-mode-in-atlassian-folder))

(use-package jira
  :config
  (setq jira-base-url "https://product-fabric.atlassian.net") ;; Jira instance URL
  (setq jira-username "syee@atlassian.com") ;; Jira username (usually, an email)
  ;; API token for Jira
  ;; See https://support.atlassian.com/atlassian-account/docs/manage-api-tokens-for-your-atlassian-account/
  (setq jira-token-is-personal-access-token nil)
  (setq jira-api-version 3) ;; Version 2 is also allowed
  )

;;; Package:
(provide 'my-atlassian)
;;; my-atlassian.el ends here
