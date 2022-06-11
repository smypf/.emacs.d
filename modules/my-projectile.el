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
  ;; Add npm projects
  ;; This allows for usage of `projectile-toggle-between-implementation-and-test`
  (projectile-mode)
  (projectile-register-project-type 'npm '("package.json")
                                  :project-file "package.json"
				  :configure "npm ci"
				  :compile "npm run build"
				  :test "npm test "
				  :run "npm start"
				  :test-suffix ".spec"))

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix leader
 "SPC" 'projectile-find-file
 "pA" 'projectile-add-known-project
 "pC" 'projectile-compile-project
 "pT" 'projectile-test-project
 "pi" 'projectile-invalidate-cache
 "pp" 'projectile-switch-project
 "pt" 'projectile-toggle-between-implementation-and-test)



;;; Package:
(provide 'my-projectile)
;;; my-projectile.el ends here
