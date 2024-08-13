;;;; my-projectile.el --- Managing Projects -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;;

;;; Code:
;; Removed recentf-mode as it is not being used
;; (use-package emacs
;;   :ensure nil
;;   :init
;;   (recentf-mode))

(use-package projectile
  :defer t
  :commands smypf-set-frame-name projectile-project-name
  :init
  (defun smypf-set-frame-name ()
    (interactive)
    (if (string= (projectile-project-name) "-")
        (setq frame-title-format (concat "Emacs"))
    (setq frame-title-format (concat "Emacs - %b - " (projectile-project-name)))))
  (smypf-set-frame-name)
  (add-hook 'projectile-find-file-hook 'smypf-set-frame-name)

  (defun smypf-open-test-in-new-window ()
    (interactive)
    (smypf-nav-split-and-follow-right)
    (projectile-toggle-between-implementation-and-test))

  :bind
  (("C-c p t" . projectile-toggle-between-implementation-and-test)
   ("C-c p T" . smypf-open-test-in-new-window))
  :config
  (projectile-mode)

  ;; Speeds up loading files.
  (setq projectile-enable-caching t
        ;; Maybe turn this on to speed up finding files
        projectile-git-use-fd nil)

  ;; Change the name of the frame.
  ;; This is useful when alt-tabbing

  ;; These have been removed. 'alien indexing is much faster
  ;; (setq projectile-sort-order 'recently-active)
  ;; (setq projectile-indexing-method 'native)
  ;; Add npm projects
  ;; This allows for usage of `projectile-toggle-between-implementation-and-test`
  (projectile-register-project-type 'npm '("package.json")
                                    :project-file "package.json"
                                    :configure "npm ci"
                                    :compilation-dir "."
                                    :compile "npm run build"
                                    :test "npm test "
                                    :run "npm start"
                                    :test-suffix ".test"))

;;; Package:
(provide 'my-projectile)
;;; my-projectile.el ends here
