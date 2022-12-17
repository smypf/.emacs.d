;;;; my-utility.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;;

;;; Code:

;; No Littering prevents backup files from being created in the same location as the file being worked on
(unless (package-installed-p 'no-littering)
  (require 'no-littering))

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
      ;; Prevent creation of "#...#" lock files
      ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Interlocking.html#Interlocking
      create-lockfiles nil)

;; Set up which-key. This shows what options are availabe for key sequences
(use-package which-key
  :defer t
  :init
  (which-key-mode)
  :config
  ;; Add more spacing between options
  (setq which-key-add-column-padding 10))

;; Copy Pasting
(use-package xclip
  :ensure t
  :init (xclip-mode)
  :config
  (setq select-enable-clipboard nil))

(use-package helpful
  :defer t
  :config
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  (global-set-key (kbd "C-h f") #'helpful-callable)

  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)

  ;; Lookup the current symbol at point. C-c C-d is a common keybinding
  ;; for this in lisp modes.
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  ;;
  ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
  ;; already links to the manual, if a function is referenced there.
  (global-set-key (kbd "C-h F") #'helpful-function)

  ;; Look up *C*ommands.
  ;;
  ;; By default, C-h C is bound to describe `describe-coding-system'. I
  ;; don't find this very useful, but it's frequently useful to only
  ;; look at interactive functions.
  (global-set-key (kbd "C-h C") #'helpful-command))

(use-package popper
  :ensure t ; or :straight t
  ;; These binds aren't working since '`' is used for other things.
  ;; They should be changed to something else.
  ;; Additionally it should be checked how meow bindings can be used.
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          helpful-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

;;(use-package shackle
;;  :ensure t
;;  :defer t
;;  :init
;;  (shackle-mode)
;;  :config
;;  (setq shackle-rules '((compilation-mode :noselect t :align 'right))
;;      shackle-default-rule '(:select t :align 'right)))


(use-package shackle
  :if (not (bound-and-true-p disable-pkg-shackle))
  :config
  (progn
    (setq shackle-default-alignment 'right)
    (setq shackle-rules
          '(("*Help*" :select nil)
            ("*info*" :select nil :other nil)
            ("*Messages*" :select nil :other nil)
            (compilation-mode :select nil :other nil))))

    (shackle-mode 1))

;; This was causing things to break.
;; TODO Fix this functionality so that Shackle does it instead
;; From https://www.masteringemacs.org/article/demystifying-emacs-window-manager
;; This probably conflicts with Shackle and the rules for shackle should be re-written
;;(setq window-sides-slots '(0 0 1 0))

;;(add-to-list 'display-buffer-alist
;;          `(,(rx (| "*compilation*" "*grep*"))
;;            display-buffer-in-side-window
;;            (side . right)
;;            (slot . 0)
;;            (window-parameters . ((no-delete-other-windows . t)))
;;            (window-width . 100)))

;; Automatically scroll compilation buffer output
(setq compilation-scroll-output t)

;;; Package:
(provide 'my-utility)
;;; my-utility.el ends here
