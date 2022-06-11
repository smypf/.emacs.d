;;;; my-dired.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;; 

;;; Code:

;; Open dired by pressing the '-' (hyphen) button
(general-define-key
 :states 'normal
 "-" 'dired-jump)

;; Kill the dired buffer when pressing 'q'
(evil-define-key 'normal dired-mode-map "q" 'kill-buffer-and-window)

;; Prevent new buffers from being created when navigating directories
(setq dired-kill-when-opening-new-dired-buffer t)

;; Hide the "." and ".." directories
;; https://stackoverflow.com/a/43632653
(add-hook 'dired-mode-hook 'dired-omit-mode)

;; use gls to ensure that folders are sorted at the top
(if (eq system-type 'darwin)
    (setq insert-directory-program "gls" dired-use-ls-dired t))
(setq dired-listing-switches "-alGh --group-directories-first"
      dired-omit-files
      (rx (or (seq bol (? ".") "#")
	      (seq bol "." eol)
	      (seq bol ".." eol))))



;;; Package:
(provide 'my-dired)
;;; my-dired.el ends here
