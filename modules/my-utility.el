;;;; my-utility.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;;

;;; Code:

;; No Littering prevents backup files from being created in the same location as the file being worked on
(use-package no-littering)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
      ;; Prevent creation of "#...#" lock files
      ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Interlocking.html#Interlocking
      create-lockfiles nil)

;; Set up which-key. This shows what options are availabe for key sequences
;; Which key is playing up. Meow provides this functionality without explicitly enabling this functionality
(use-package which-key
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
  :bind (
         ;; Note that the built-in `describe-function' includes both functions
         ;; and macros. `helpful-function' is functions only, so we provide
         ;; `helpful-callable' as a drop-in replacement.
         ("C-h f" . helpful-callable)
         ;; Lookup the current symbol at point. C-c C-d is a common keybinding
         ;; for this in lisp modes.
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
         ;; already links to the manual, if a function is referenced there.
         ("C-c C-d" . helpful-at-point)
         ("C-h F" . helpful-function)
         ;; Look up *C*ommands.
         ;;
         ;; By default, C-h C is bound to describe `describe-coding-system'. I
         ;; don't find this very useful, but it's frequently useful to only
         ;; look at interactive functions.
         ("C-h C" . helpful-command)))

(use-package popper
  :ensure t ; or :straight t
  ;; These binds aren't working since '`' is used for other things.
  ;; They should be changed to something else.
  ;; Additionally it should be checked how meow bindings can be used.
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          helpful-mode
          flycheck-error-list-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

(use-package shackle
  :if (not (bound-and-true-p disable-pkg-shackle))
  :config
  (progn
    (setq shackle-default-alignment 'right)
    (setq shackle-rules
          '(("*Help*" :select nil)
            ("*Warnings*" :select nil :other nil :align below)
            (helpful-mode :select nil :other nil)
            ("*info*" :select nil :other nil)
            ("*Messages*" :select nil :other nil)
            ;; Including this line causes the height to be 50% of the window, regardless of the :size value
            ;;(flycheck-error-list-mode :select nil :size .1 :align below)
            (compilation-mode :select nil :other nil)))
    )

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

;; Enable recentf-mode which stores recently opened files
(use-package recentf
  :ensure nil
  :init
  (recentf-mode t)
  (setq recentf-max-saved-items 50)
  :bind ("C-c r" . recentf))

;; Windmove is an alternative means to navigate window panes

(use-package windmove
  :ensure nil
  :config
  (windmove-mode)
  (windmove-default-keybindings 'super))

(use-package binky-mode
  :hook (after-init-hook . binky-mode)
  :bind ("C-M-m" . binky-binky))

(use-package balanced-windows
  :config
  (balanced-windows-mode))

;; CMD + Mouse 1 open address
(global-set-key [s-mouse-1] 'goto-address-at-mouse)

(use-package emacs
  :ensure nil
  :init
  (defun xah-next-user-buffer ()
    "Switch to the next user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://xahlee.info/emacs/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
    (interactive)
    (next-buffer)
    (let ((i 0))
      (while (< i 20)
        (if (not (xah-user-buffer-q))
            (progn (next-buffer)
                   (setq i (1+ i)))
          (progn (setq i 100))))))

  (defun xah-previous-user-buffer ()
    "Switch to the previous user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://xahlee.info/emacs/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
    (interactive)
    (previous-buffer)
    (let ((i 0))
      (while (< i 20)
        (if (not (xah-user-buffer-q))
            (progn (previous-buffer)
                   (setq i (1+ i)))
          (progn (setq i 100))))))

  (defun xah-user-buffer-q ()
    "Return t if current buffer is a user buffer, else nil.
Typically, if buffer name starts with *, it's not considered a user buffer.
This function is used by buffer switching command and close buffer command,
so that next buffer shown is a user buffer.
You can override this function to get your idea of “user buffer”.
version 2016-06-18"
    (interactive)
    (if (string-equal "*" (substring (buffer-name) 0 1))
        nil
      (if (string-equal major-mode "dired-mode")
          nil
        t
        )))

  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c b n" . xah-next-user-buffer)
         ("C-c b p" . xah-previous-user-buffer)))

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind ([remap ispell-word] . jinx-correct))

;; These are stolen from https://macowners.club/posts/custom-functions-5-navigation/
(use-package emacs
  :ensure nil
  :config
  (defun smypf-nav-split-and-follow-right ()
    "Split and focus a new window to the right."
    (interactive)
    (split-window-right)
    (other-window 1))
  (defun smypf-nav-find-file-right ()
    "Open file with `projectile-find-file' in a split window to the right."
    (interactive)
    (split-window-right)
    (other-window 1)
    (projectile-find-file))
  :commands (smypf-nav-find-file-right smypf-nav-split-and-follow-right)
  :bind (([remap split-window-right] . smypf-nav-split-and-follow-right)
         ("C-c n" . smypf-nav-find-file-right)))

(use-package howm
  :defer t
  :init
  (require 'howm)
  :config
  ;; Refer to https://leahneukirchen.org/blog/archive/2022/03/note-taking-in-emacs-with-howm.html for some more information about why this is used
  (setq howm-home-directory (file-truename "~/org/howm"))
  (setq howm-directory (symbol-value 'howm-home-directory))
  (setq howm-keyword-file (expand-file-name ".howm-keys" howm-home-directory))
  (setq howm-history-file (expand-file-name ".howm-history" howm-home-directory))
  (setq howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.org")
  (setq howm-view-use-grep t)
  (setq howm-view-grep-command "rg")
  (setq howm-view-grep-option "-nH --no-heading --color never")
  (setq howm-view-grep-extended-option nil)
  (setq howm-view-grep-fixed-option "-F")
  (setq howm-view-grep-expr-option nil)
  (setq howm-view-grep-file-stdin-option nil))

;;; Package:
(provide 'my-utility)
;;; my-utility.el ends here
