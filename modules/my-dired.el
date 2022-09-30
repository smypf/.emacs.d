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

(use-package dirvish
  :defer t
  :init
  (dirvish-override-dired-mode)
  :custom
  ;; (dirvish-header-line-format '(:left (path) :right (free-space)))
  (dirvish-mode-line-format
   '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
  ;(dirvish-attributes '(file-size collapse subtree-state vc-state git-msg))
  (dirvish-attributes '(collapse subtree-state vc-state))
  ;; Maybe the icons are too big to your eyes
  ;; (dirvish-all-the-icons-height 0.8)
  ;; In case you want the details at startup like `dired'
  ;; (dirvish-hide-details nil)
  :config
  ;(evil-set-initial-state 'dirvish-mode 'emacs)
  ;; (dirvish-peek-mode) ; Preview files in minibuffer
  ;; Dired options are respected except a few exceptions, see *In relation to Dired* section above
  (setq dired-dwim-target t)
  (setq delete-by-moving-to-trash t)
  ;; Enable mouse drag-and-drop files to other applications
  (setq dired-mouse-drag-files t)                   ; added in Emacs 29
  (setq mouse-drag-and-drop-region-cross-program t) ; added in Emacs 29
  ;; See *Parsing switches*
  (setq dired-listing-switches
        "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
  :bind
  ;; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   ;; Dirvish has all the keybindings in `dired-mode-map' already
   :map dirvish-mode-map
   ("h" . dired-up-directory)
   ("j" . dired-next-line)
   ("k" . dired-previous-line)
   ("l" . dired-find-file)
   ("i" . wdired-change-to-wdired-mode)
   ("?" . dirvish-dispatch)
   ("." . dired-omit-mode)))
   ;;("a"   . dirvish-quick-access)
   ;;("f"   . dirvish-file-info-menu)
   ;;("y"   . dirvish-yank-menu)
   ;;("N"   . dirvish-narrow)
   ;;("^"   . dirvish-history-last)
   ;;("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ;;("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ;;("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ;;("TAB" . dirvish-subtree-toggle)
   ;;("M-f" . dirvish-history-go-forward)
   ;;("M-b" . dirvish-history-go-backward)
   ;;("M-l" . dirvish-ls-switches-menu)
   ;;("M-m" . dirvish-mark-menu)
   ;;("M-t" . dirvish-layout-toggle)
   ;;("M-s" . dirvish-setup-menu)
   ;;("M-e" . dirvish-emerge-menu)
   ;;("M-j" . dirvish-fd-jump)))

(use-package dirvish-extras
  :defer t
  :after dirvish
  :ensure nil)

;;; Package:
(provide 'my-dired)
;;; my-dired.el ends here
