;;;; my-visual-customisations.el --- Change the look and feel  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;; Change how the Emacs interface looks based on my preferences

;;; Code:

;; Disable the menu bar which is not useful in the terminal
(unless window-system
  (menu-bar-mode -1))


;; Disable audio bells which are annoying
;; (setq visible-bell 1)
;; https://www.emacswiki.org/emacs/AlarmBell
(setq ring-bell-function 'ignore)


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
(unless (package-installed-p 'pleasant-monochromish-theme)
  (package-install-file (expand-file-name "pleasant-monochromish-theme.el" user-emacs-directory)))

;; Load the theme
(load-theme 'pleasant-monochromish t)

;; Add line numbers globally
(global-display-line-numbers-mode)

;; Use hl-line-mode everywhere
;; This makes it easier to see which line the cursor is on
(global-hl-line-mode)


;; This isn't working for some reason
;; (defvar my-visual-fill-toggle nil)
;; (defun my-visual-fill-toggle ()
;;   (interactive)
;;   (if (eq my-visual-fill-toggle nil)
;;       (progn (setq my-visual-fill-toggle t)
;;        (visual-fill-column-mode 1))
;;     (progn (visual-fill-column-mode -1)
;;      (setq my-visual-fill-toggle t))))


(use-package visual-fill-column)
;; :hook (visual-fill-column-mode-hook . visual-line-mode))

;; Break lines (word wrap) at word boundaries rather than the last character in the line
;; (global-visual-line-mode t)
;; (add-hook 'prog-mode 'visual-line-mode)

;; Prevent the cursor jumping to the middle of the page when scrolling to the bottom of the screen
;; https://stackoverflow.com/a/25777730
(setq scroll-conservatively 101)

;; Add a margin at the top and bottom of the page to give more context
(setq scroll-margin 5)


;; Change highlighting while searching
;; Perhaps change the `/` button to `consult-line`
;; This was disabled in favour of 'evil-search. See modules/my-evil.el
; (setq lazy-highlight-cleanup nil
;       lazy-highlight-max-at-a-time nil
;       lazy-highlight-initial-delay 0
;       isearch-allow-scroll t)

;; 120 is a good number
(setq-default fill-column 120)

;; Set the initial buffer to the scratch buffer
(setq inhibit-startup-message t
      ;; Set a different message
      initial-scratch-message ";; Stay focussed\n\n")

;; Allow for pressing `y` and `n` instead of having to type yesRET or noRET
(defalias 'yes-or-no-p 'y-or-n-p)


;; Change the vertical border character
;; https://stackoverflow.com/a/18211568
;; Set symbol for the border
(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?│))

;; Remove the `\` character from lines that wrap
(set-display-table-slot standard-display-table 'wrap ?\ )

;; Reverse colors for the border to have nicer line
;; These are done in the pleasant-monochromish theme
;; (set-face-inverse-video-p 'vertical-border nil)
;; (set-face-background 'vertical-border (face-background 'default))


;;; Package:
(provide 'my-visual-customisations)
;;; my-visual-customisations.el ends here

