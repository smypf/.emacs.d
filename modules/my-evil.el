;;;; my-evil.el --- Keybinds which make sense (to me) -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;; Vim keybindings in Emacs

;;; Code:

;; Setup evil for vim style keybinds
(use-package evil
  :init
  (setq evil-want-integration t
	evil-want-keybinding nil
	evil-vsplit-window-right t
	evil-split-window-below nil

	;; Persist highlighting
	;; https://www.reddit.com/r/emacs/comments/6lythd/comment/djxowfs
	evil-search-module 'evil-search 

	evil-undo-system 'undo-redo

	;; Search for the word under the cursor instead of the symbol
	;; https://github.com/emacs-evil/evil/pull/1431/commits/84347427a729b7cc325be05ea2996ec1ad3efda3
	evil-symbol-word-search t

	;; Setting `split-height-threshold` to nil and `split-width-threshold` to 1 forces vertical splits
	;; This was specifically done for ensuring that magit panes are not opened in horizontal splits
	;; These values are copied from the Doom Emacs repository
	;; https://github.com/doomemacs/doomemacs/blob/61a7c541655038615e3f846a87db2e7d5883d35a/core/core-ui.el#L290
	split-height-threshold nil
	split-width-threshold 160)
  (evil-mode))

(use-package evil-surround
  :after evil)


;; Evil Collection is used for setting up vim keybindings in other buffers
(use-package evil-collection
  :after evil
  :config
  ;; Initialise only the packages I'm interested in
  (evil-collection-init '(magit dired consult vertico)))

;; For some reason this doesn't work
;; (evil-ex-define-cmd "\"w" 'evil-write)
(evil-ex-define-cmd "W" 'evil-write)

;; Keybinds for manipulating window panes
(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix leader
 "w" 'evil-window-map)
(define-key evil-window-map (kbd "<right>") 'evil-window-right)
(define-key evil-window-map (kbd "<left>") 'evil-window-left)
(define-key evil-window-map (kbd "<up>") 'evil-window-up)
(define-key evil-window-map (kbd "<down>") 'evil-window-down)
(define-key evil-window-map (kbd "S-<right>") 'evil-window-move-far-right)
(define-key evil-window-map (kbd "S-<left>") 'evil-window-move-far-left)
(define-key evil-window-map (kbd "S-<up>") 'evil-window-move-very-top)
(define-key evil-window-map (kbd "S-<down>") 'evil-window-move-very-bottom)

;; Keybinds for manipulating buffers
(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix leader
 "bp" 'evil-prev-buffer
 "bn" 'evil-next-buffer
 "bb" 'switch-to-buffer)

;; TODO move this
;; Keybinds for searching
(defun search-thing-at-point ()
  (interactive)
  (consult-ripgrep (projectile-project-root) (thing-at-point 'symbol)))

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix leader
 "/" 'consult-ripgrep
 "?" 'search-thing-at-point
 "s" 'consult-line)

;;; Package:
(provide 'my-evil)
;;; my-evil.el ends here

