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

;; Change the name of the frame.
;; This is useful when alt-tabbing
(defun smypf/set-frame-name ()
  (interactive)
  (if (string= (projectile-project-name) "-")
      (setq frame-title-format (concat "Emacs"))
    (setq frame-title-format (concat "Emacs - %b - " (projectile-project-name)))))

(defun smypf/set-compilation-search-path ()
  (interactive)
  (add-to-list compilation-search-path (projectile-project-root)))

(defun smypf/open-test-in-new-window ()
  (interactive)
  (smypf/nav-split-and-follow-right)
  (projectile-toggle-between-implementation-and-test))

(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))

(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)


(use-package projectile
  :defer t
  :commands smypf/set-frame-name projectile-project-name
  :custom
  (
   ;; create test files
   (projectile-create-missing-test-files t)
   ;; Speeds up loading files.
   (projectile-enable-caching t)
   ;; Maybe turn this on to speed up finding files
   (projectile-git-use-fd t))

  :hook
  ((projectile-find-file . smypf/set-frame-name))
  :bind
  (("C-c p t" . projectile-toggle-between-implementation-and-test)
   ("C-c p T" . smypf/open-test-in-new-window))
  :config
  ;; TODO fix this to not use config
  (projectile-mode)
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
