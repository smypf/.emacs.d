;;;; my-visual-customisations.el --- Change the look and feel  -*- lexical-binding: t; -*-

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

;; Prevent the cursor jumping to the middle of the page when scrolling to the bottom of the screen
;; https://stackoverflow.com/a/25777730
(setq scroll-conservatively 101)

;; Add a margin at the top and bottom of the page to give more context
(setq scroll-margin 5)


;; Change highlighting while searching
;; TODO after pressing `n` search terms are not highlighted. I don't like this.
;; Perhaps change the `/` button to `consult-line`
(setq lazy-highlight-cleanup nil
      lazy-highlight-max-at-a-time nil
      lazy-highlight-initial-delay 0
      isearch-allow-scroll t)

;; 120 is a good number
(setq-default fill-column 120)

;; Set the initial buffer to the scratch buffer
(setq inhibit-startup-message t
      ;; Set a different message
      initial-scratch-message ";; Stay focussed\n\n")

;; Allow for pressing `y` and `n` instead of having to type yesRET or noRET
(defalias 'yes-or-no-p 'y-or-n-p)


;;; Package:
(provide 'my-visual-customisations)
;;; my-visual-customisations.el ends here
