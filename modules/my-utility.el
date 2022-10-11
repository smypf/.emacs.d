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


;;; Package:
(provide 'my-utility)
;;; my-utility.el ends here
