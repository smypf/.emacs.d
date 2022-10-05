;;;; my-projectile.el --- Managing Projects -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;; 

;;; Code:

(use-package projectile
  :defer t
  :config
  (projectile-mode)
  (setq projectile-sort-order 'recently-active)
  ;; Add npm projects
  ;; This allows for usage of `projectile-toggle-between-implementation-and-test`
  (projectile-register-project-type 'npm '("package.json")
                                  :project-file "package.json"
				  :configure "npm ci"
				  :compile "npm run build"
				  :test "npm test "
				  :run "npm start"
				  :test-suffix ".spec"))

(use-package emacs
  :ensure nil
  :config
  (recentf-mode))




;;; Package:
(provide 'my-projectile)
;;; my-projectile.el ends here
