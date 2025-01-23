;;;; my-meow.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;;

;;; Code:
(use-package emacs
  :after magit
  :config
  ;; bind SPC to Meow Keypad in magit status
  (define-key magit-status-mode-map (kbd "SPC") #'meow-keypad))


(defun smypf-meow-quit ()
  "A better way of handling close panes.

Close the window if there is more than one buffer.
If there is only one buffer go to the *scratch* buffer."
  (interactive)

  (if (bound-and-true-p magit-blame-read-only-mode)
      (magit-blame-quit)
    (if (> (count-windows) 1)
        (delete-window)
      (if (> (length (tab-bar-tabs)) 1)
          (tab-bar-close-tab)
        (switch-to-buffer "*scratch*")))))

(defun meow-qwerty-setup ()
  ;; https://emacs.stackexchange.com/questions/45401/why-cant-i-bind-my-function-to-a-key-or-call-it-with-m-x
  (interactive)
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   ;; '("/" . meow-keypad-describe-key) ;; I don't use this
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("C-r" . undo-redo)
   '("~" . pop-global-mark)
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   ;; TODO '-' doesn't work in the dired buffers. Need to figure out what is overriding the bind
   ;; '("-" . dired-jump)
   '("-" . negative-argument)
   '("=" . indent-region)
   '(";" . meow-reverse)
   '(":" . execute-extended-command)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . smypf-meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)
   '("/" . consult-line)))

(defun meow-colemak-setup ()
  (interactive)
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak-dh)
  (meow-motion-overwrite-define-key
   ;; Use e to move up, n to move down.
   ;; Since special modes usually use n to move down, we only overwrite e here.
   '("e" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   '("?" . meow-cheatsheet)
   ;; To execute the originally e in MOTION state, use SPC e.
   '("e" . "H-e")
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument))
  (meow-normal-define-key
   '("C-r" . undo-redo)
   '("0" . meow-expand-0)
   '("1" . meow-expand-1)
   '("2" . meow-expand-2)
   '("3" . meow-expand-3)
   '("4" . meow-expand-4)
   '("5" . meow-expand-5)
   '("6" . meow-expand-6)
   '("7" . meow-expand-7)
   '("8" . meow-expand-8)
   '("9" . meow-expand-9)
   '("-" . negative-argument)
   '("=" . indent-region)
   '(";" . meow-reverse)
   '(":" . execute-extended-command)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("/" . meow-visit)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)

   '("D" . meow-backward-delete)

   '("e" . meow-prev)
   '("E" . meow-prev-expand)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)

   '("h" . meow-next)
   '("H" . meow-next-expand)
   ;; '("h" . meow-left)
   ;; '("H" . meow-left-expand)

   '("i" . meow-right)
   '("I" . meow-right-expand)
   '("j" . meow-join)
   '("k" . meow-kill) ;; is this important enough that it should be located in the home row?

   '("l" . meow-left) ;; changed from meow-line to meow-left
   '("L" . meow-left-expand) ;; changed from meow-goto-line to meow-left-expand
   ;; '("l" . meow-line) ;; changed from meow-line to meow-left
   ;; '("L" . meow-goto-line) ;; changed from meow-goto-line to meow-left-expand

   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)

   '("n" . meow-search)
   ;; '("n" . meow-next)
   ;; '("N" . meow-next-expand)

   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . smypf-meow-quit)
   '("r" . meow-replace)
   '("s" . meow-insert)
   '("S" . meow-open-above)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-search)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("X" . meow-backward-delete)
   '("y" . meow-save)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

(use-package meow
  :ensure t
  :init
  (meow-global-mode 1)
  :config
  ;; (meow-setup-line-number)
  ;; from https://github.com/meow-edit/meow/issues/84
  ;; Must set before enable `meow-global-mode`
  ;;(setq meow-use-cursor-position-hack t
  ;;    meow-use-enhanced-selection-effect t)
  (meow-qwerty-setup))

;; TODO
;; Add additional QOL keys. See my-evil for a list of current bindings
;; Window Management
;; Determine how to navigate to different splits
;; (other-window 1)
;; Leader Binds
;; Manipulating buffers
;; Searching
;; Opening Git
;; SPC G
;; Find file (Projectile)
;; SPC SPC

(defun back-window()
  "Go back a window."
  (interactive)
  (other-window -1))

;; TODO Move these binds to specific module files rather than in the Meow module
(meow-leader-define-key
 ;; Window Manipulation
 ;; At this point you should probably learn how Emacs does it natively
 ;; '("wv" . split-window-right)
 ;; '("wl" . other-window)
 ;; '("wh" . back-window)

 ;; Open Magit
 '("G" . magit)

 ;; Searching
 ;;'("/" . consult-ripgrep) - replaced with deadgrep
 '("?" . search-thing-at-point)
 '("'" . eval-expression)
 '("i" . consult-imenu)
 '("I" . consult-imenu-multi)
 '("s" . consult-line)

 ;; Projectile
 '("SPC" . projectile-find-file)
 '("C" . projectile-compile-project)
 '("R" . recompile)
 ;;'("T" . projectile-test-project)
 '("pp" . projectile-switch-project)
 '("pt" . projectile-toggle-between-implementation-and-test)

 ;; Coding
 '("cd" . xref-find-definitions)
 '("cD" . find-references-at-point)
 '("cl" . xref-list-references)
 '("cr" . eglot-rename)
 '("ca" . eglot-code-actions)
 '("ce" . consult-flycheck)
 '("cs" . consult-eglot-symbols)
 '("cf" . backward-forward-next-location)
 '("cb" . backward-forward-previous-location)

 ;; Completion
 '("v" . vertico-repeat)

 ;; easier M-x
 '(":" . execute-extended-command)
 '("o" . execute-extended-command)
 ;; end

 ;; buffers
 ;; Something needs to be done to filter out irrelevant buffers
 ;;'("bn" . next-buffer)
 ;;'("bp" . previous-buffer)
 '("bb" . consult-buffer)
 '("bk" . kill-buffer)
 '("B" . consult-buffer)
                                        ;'("," . consult-buffer)

 '("X" . org-capture)
 )

;; (meow-define-keys
;;     ;; state
;;     'normal
;;   '("z c" . hs-hide-block)
;;   '("z o" . hs-show-block)
;;   '("z C" . hs-hide-all)
;;   '("z O" . hs-show-all))

;; Disable meow for various mode which rely on having inbuilt keymaps
;; https://github.com/meow-edit/meow/issues/317#issuecomment-1233622579
;; TODO add keybinds to the magit-blame-read-only-mode-map
(meow-define-state disable "dummy state")
(add-to-list 'meow-mode-state-list '(magit-mode . disable))
(add-to-list 'meow-mode-state-list '(git-rebase-mode . disable))
(add-to-list 'meow-mode-state-list '(git-commit-mode . insert))
(add-to-list 'meow-mode-state-list '(ediff-mode . disable))
(add-to-list 'meow-mode-state-list '(vterm-mode . disable))
(add-to-list 'meow-mode-state-list '(howm-menu-mode . disable))
(add-to-list 'meow-mode-state-list '(howm-mode . disable))
(add-to-list 'meow-mode-state-list '(hm . disable))
(add-to-list 'meow-mode-state-list '(private-comments-edit-mode . insert))
(add-to-list 'meow-mode-state-list '(org-capture-mode . insert))

(add-hook 'howm-menu-hook 'meow-disable-mode)

(use-package meow-tree-sitter
  :after meow
  :defer t
  :init
  (meow-tree-sitter-register-defaults))

;; TODO
;; C-d Down a page
;; C-b Up a page

;;; Package:
(provide 'my-meow)
;;; my-meow.el ends here
