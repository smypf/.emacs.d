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
       (eval . (magit-disable-section-inserter 'magit-insert-status-headers))
	   ;; 6+ seconds for this one!!!
       (eval . (magit-disable-section-inserter 'magit-insert-untracked-files))
       (eval . (magit-disable-section-inserter 'magit-insert-unpulled-from-upstream))
       (eval . (magit-disable-section-inserter 'magit-insert-unpushed-to-upstream-or-recent))
       ))))

(dir-locals-set-directory-class
 (expand-file-name "~/atlassian/atlassian-frontend-monorepo") 'huge-git-repository)

(use-package emacs
  :ensure nil
  :hook
  (prog-mode . indent-tabs-mode))



;;; Package:
(provide 'my-atlassian)
;;; my-atlassian.el ends here
