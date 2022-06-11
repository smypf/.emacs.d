;;;; visual-customisations.el --- Change the look and feel  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;; Change how the Emacs interface looks based on my preferences

;;; Code:

;; Disable the menu bar which is not useful in the terminal
(menu-bar-mode -1)

;; Disable audio bells which are annoying
(setq visible-bell 1)

;; Doom modeline is a nice package for showing meta information about the current buffer
(use-package doom-modeline
  :init (doom-modeline-mode)
  :config
  (setq doom-modeline-major-mode-icon nil
	doom-modeline-vcs-max-length 40
	doom-modeline-workspace-name nil
	doom-modeline-buffer-encoding nil
	doom-modeline-persp-name nil
	doom-modeline-persp-icon t
	doom-modeline-buffer-file-name-style 'relative-to-project))


;; Setup my theme to be used
(unless (package-installed-p 'eink-theme)
  (package-install-file (expand-file-name "eink-theme.el" user-emacs-directory)))

;; Load the theme
(load-theme 'eink t)

;; Add line numbers globally
(global-display-line-numbers-mode)

;; Use hl-line-mode everywhere
;; This makes it easier to see which line the cursor is on
(global-hl-line-mode)

;;; Package:
(provide 'visual-customisations)
;;; rational-compile.el ends here
