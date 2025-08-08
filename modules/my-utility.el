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
  ;; Disabled as this was causing issues with the display
  (which-key-setup-minibuffer)
  (setq which-key-add-column-padding 10))

(use-package rg
  :ensure t
  :defer t)

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


;; Replacing shackle with popper functions. They were not working well together.
;; See my-popper.el
;; ;; shackle needs to come before popper
;; (use-package shackle
;;   :if (not (bound-and-true-p disable-pkg-shackle))
;;   :hook
;;   ((after-init . shackle-mode))
;;   :custom
;;   (shackle-default-alignment 'right)
;;   (shackle-rules
;;    '(("*Help*" :select nil)
;;      ("*Warnings*" :select nil :other nil :align below)
;;      ("*Warnings*" :select nil :other nil :align below)
;;      (howm-view-summary-mode :other nil :align right)
;;      ("*HTTP Response*" :select nil :other nil)
;;      ("*HTTP Response*" :select nil :other nil)
;;      ;; ("\\*eldoc.*" :align below :size 10 :select nil :regexp t)
;;      ("*eldoc*" :align above :size 10 :select nil)
;;      (helpful-mode :select t :other nil)
;;      ("*info*" :select nil :other nil)
;;      (rg-mode :select nil :other nil)
;;      ("*Messages*" :select nil :other nil)
;;      ;; Including this line causes the height to be 50% of the window, regardless of the :size value
;;      ;;(flycheck-error-list-mode :select nil :size .1 :align below)
;;      (compilation-mode :select nil :other nil :align right))))

;; (use-package popper
;;   :defer t
;;   :ensure t ; or :straight t
;;   ;; These binds aren't working since '`' is used for other things.
;;   ;; They should be changed to something else.
;;   ;; Additionally it should be checked how meow bindings can be used.
;;   :bind (("C-c w p p"   . popper-toggle)
;;          ("C-c w p n"   . popper-cycle))
;;   :init
;;   (setq popper-reference-buffers
;;     '("\\*Messages\\*"
;;       "\\*Warnings\\*"
;;       "\\*HTTP Response\\*"
;;       "Output\\*$"
;;       "\\*Async Shell Command\\*"
;;       help-mode
;;       helpful-mode
;;       flycheck-error-list-mode
;;       rg-mode
;;       devdocs-mode
;;       "\\*eldoc\\*"
;;       "\\*eldoc for .*?\\*"
;;       compilation-mode))
;;   (popper-mode +1)
;;   (popper-echo-mode +1) ; For echo area hints

;;   :config
;;   ;; ensure that shackle is responsible for the height of the buffer
;;   (setq popper-display-control nil)
;;   (defvar popper-repeat-map
;;     (let ((map (make-sparse-keymap)))
;;       (define-key map "p" 'popper-toggle)
;;       (define-key map "n" 'popper-cycle)
;;       map))

;;   (put 'popper-toggle 'repeat-map 'popper-repeat-map)
;;   (put 'popper-cycle 'repeat-map 'popper-repeat-map))

;; ;; This was causing things to break.
;; ;; TODO Fix this functionality so that Shackle does it instead
;; ;; From https://www.masteringemacs.org/article/demystifying-emacs-window-manager
;; ;; This probably conflicts with Shackle and the rules for shackle should be re-written
;; ;;(setq window-sides-slots '(0 0 1 0))

;; ;;(add-to-list 'display-buffer-alist
;; ;;          `(,(rx (| "*compilation*" "*grep*"))
;; ;;            display-buffer-in-side-window
;; ;;            (side . right)
;; ;;            (slot . 0)
;; ;;            (window-parameters . ((no-delete-other-windows . t)))
;; ;;            (window-width . 100)))

;; Enable recentf-mode which stores recently opened files
(use-package recentf
  :defer t
  :ensure nil
  :init
  (recentf-mode t)
  (setq recentf-max-saved-items 50)
  :bind ("C-c f r" . recentf))

;; Windmove is an alternative means to navigate window panes
;; (use-package windmove
;;   :defer t
;;   :ensure nil
;;   :config
;;   (windmove-mode)
;;   (windmove-default-keybindings 'super))



;; (use-package binky
;;   :defer t
;;   :hook (after-init-hook . binky-mode)
;;   :bind ("C-M-m" . binky-binky))
;; (use-package gumshoe
;;   :diminish
;;   :init
;;   (global-gumshoe-mode 1)
;;   :config
;;   :bind (("C-M-," . gumshoe-win-backtrack)
;;          :map global-gumshoe-backtracking-mode-map
;;          ("C-M-," . gumshoe-win-backtrack-back)
;;          ("C-M-." .  gumshoe-win-backtrack-forward)
;;          ("C-M-/" .  gumshoe-peruse-globally)
;;          ))

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
'user buffer' is determined by `xah-user-buffer-q'.
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
'user buffer' is determined by `xah-user-buffer-q'.
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
You can override this function to get your idea of 'user buffer'.
version 2016-06-18"
    (interactive)
    (if (string-equal major-mode "deadgrep-mode")
        t
      (if (string-equal "*" (substring (buffer-name) 0 1))
          nil
        (if (string-equal major-mode "dired-mode")
            nil
          (if (string-equal major-mode "magit-process-mode")
              nil
            t
            )))))

  ;; Modified buffer navigation functions
  ;; These functions allow you to quickly jump between buffers that have unsaved changes.
  ;; They wrap around the buffer list, so you can cycle through all modified buffers.
  ;; If no other buffers have unsaved changes, a message is displayed.

  (defun smypf/next-modified-buffer ()
    "Switch to the next buffer with unsaved changes.
Skips buffers that are not modified or are special buffers."
    (interactive)
    (let ((buffers (buffer-list))
          (current-buffer (current-buffer))
          (found nil)
          (started-from-current nil))
      ;; Find the current buffer in the list
      (while (and buffers (not found))
        (if (eq (car buffers) current-buffer)
            (setq found t)
          (setq buffers (cdr buffers))))
      ;; Start from the next buffer
      (setq buffers (cdr buffers))
      ;; If we reached the end, wrap around to the beginning
      (unless buffers
        (setq buffers (buffer-list)))
      ;; Find the next modified buffer
      (let ((i 0)
            (max-iterations (length (buffer-list))))
        (while (and (< i max-iterations)
                    (or (not (and (buffer-modified-p (car buffers))
                                  (with-current-buffer (car buffers) (xah-user-buffer-q))))
                        (and started-from-current (eq (car buffers) current-buffer))))
          (setq buffers (cdr buffers))
          (setq i (1+ i))
          ;; Wrap around if we reach the end
          (unless buffers
            (setq buffers (buffer-list))
            (setq started-from-current t)))
        (if (and buffers
                 (buffer-modified-p (car buffers))
                 (with-current-buffer (car buffers) (xah-user-buffer-q))
                 (not (eq (car buffers) current-buffer)))
            (switch-to-buffer (car buffers))
          (message "No other buffers with unsaved changes")))))

  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c b n" . xah-next-user-buffer)
         ("C-c b p" . xah-previous-user-buffer)
         ("C-c b m" . smypf/next-modified-buffer)))

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind ([remap ispell-word] . jinx-correct))


;; Scale the text size globally rather than on a per-frame basis
(use-package emacs
  :ensure nil
  :bind (
         ("s-=" . 'global-text-scale-adjust)
         ("s--" . 'global-text-scale-adjust)
         ("s-0" . 'global-text-scale-adjust)))

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
                                        ;("C-c n" . smypf-nav-find-file-right)
         ))

(use-package smart-delete
  :defer t
  :bind (("C-c t d" . smart-delete-mode)))
;; Removed automatic enabling of smart-delete-mode as it got in the way
;;:hook (prog-mode . smart-delete-mode)

(use-package visual-regexp
  :defer t
  :bind (("C-c %" . vr/replace)
         ([remap query-replace] . vr/replace)))


;; Doesn't work for me. It's too old maybe
;; (use-package visual-regexp-steroids
;;   :after visual-regexp
;;   :defer nil)

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

(use-package emacs
  :ensure nil
  :config
  ;; https://www.reddit.com/r/emacs/comments/r7l3ar/comment/hn3kuwh
  (defun smypf/scroll-down-half-page ()
    "scroll down half a page while keeping the cursor centered"
    (interactive)
    (let ((ln (line-number-at-pos (point)))
          (lmax (line-number-at-pos (point-max))))
      (cond ((= ln 1) (move-to-window-line nil))
            ((= ln lmax) (recenter (window-end)))
            (t (progn
                 (move-to-window-line -1)
                 (recenter))))))

  (defun smypf/scroll-up-half-page ()
    "scroll up half a page while keeping the cursor centered"
    (interactive)
    (let ((ln (line-number-at-pos (point)))
          (lmax (line-number-at-pos (point-max))))
      (cond ((= ln 1) nil)
            ((= ln lmax) (move-to-window-line nil))
            (t (progn
                 (move-to-window-line 0)
                 (recenter))))))

  ;; Add to repeat map
  ;; https://www.masteringemacs.org/article/mastering-key-bindings-emacs
  (defvar scroll-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map "e" 'smypf/scroll-up-half-page)
      (define-key map "d" 'smypf/scroll-down-half-page)
      map))

  (put 'smypf/scroll-up-half-page 'repeat-map 'scroll-repeat-map)
  (put 'smypf/scroll-down-half-page 'repeat-map 'scroll-repeat-map)

  :bind (
         ("C-c d" . smypf/scroll-down-half-page)
         ("C-c e" . smypf/scroll-up-half-page)
         )
  :commands (smypf/scroll-down-half-page smypf/scroll-up-half-page))


(use-package rainbow-mode
  :defer t)

;; (use-package compile-angel
;;   :ensure t
;;   :demand t
;;   :custom
;;   (compile-angel-verbose nil)
;;   :config
;;   (compile-angel-on-load-mode)
;;   (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode))

;; Global Mark Ring
;; (use-package emacs
;;   :ensure nil
;;   :config
;;   (defun push-mark-no-activate ()
;;    "Pushes `point' to `mark-ring' and does not activate the region
;;    Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
;;    (interactive)
;;    (push-mark (point) t nil)
;;    (message "Pushed mark to ring"))

;;   (global-set-key (kbd "C-`") 'push-mark-no-activate)

;;   (defun jump-to-mark ()
;;    "Jumps to the local mark, respecting the `mark-ring' order.
;;   This is the same as using \\[set-mark-command] with the prefix argument."
;;    (interactive)
;;    (set-mark-command 1))
;;   (global-set-key (kbd "M-`") 'jump-to-mark))

;; (use-package emacs
;;   :ensure nil
;;   :config
;;   (global-set-key (kbd "C-M-,") 'point-stack-pop)
;;   (global-set-key (kbd "C-M-/") 'point-stack-push)
;;   (global-set-key (kbd "C-M-.") 'point-stack-forward-stack-pop)

;; The recommended usage is to save locations automatically on navigation:
;; (point-stack-setup-advices))


;;; Commentary:
;; Original function at the time of this writing is at:
;; https://github.com/emacs-mirror/emacs/blob/3907c884f03cf5f2a09696bda015b1060c7111ba/lisp/simple.el#L10980
;;
(defun ct/kill-buffer--possibly-save--advice (original-function buffer &rest args)
  "Ask user in the minibuffer whether to save before killing.

Replaces `kill-buffer--possibly-save' as advice, so
ORIGINAL-FUNCTION is unused and never delegated to. Its first
parameter is the buffer, which is the `car' or ARGS."
  (let ((response
         (car
          (read-multiple-choice
           (format "Buffer %s modified."
                   (buffer-name))
           '((?s "Save and kill buffer" "save the buffer and then kill it")
             (?d "Discard and kill buffer without saving" "kill buffer without saving")
             (?c "Cancel" "Exit without doing anything"))
           nil nil (and (not use-short-answers)
                        (not (use-dialog-box-p)))))))
    (cond ((= response ?s)
           (with-current-buffer buffer (save-buffer))
           t)
          ((= response ?d)
           t)
          ((= response ?c)
           nil)
          )))

(advice-add 'kill-buffer--possibly-save :around #'ct/kill-buffer--possibly-save--advice)

(use-package highlight
  :defer t)


(use-package repeat-fu
  :commands (repeat-fu-mode repeat-fu-execute)

  :config
  (setq repeat-fu-preset 'meow)

  :hook
  ((meow-mode)
   .
   (lambda ()
     (when (and (not (minibufferp)) (not (derived-mode-p 'special-mode)))
       (repeat-fu-mode)
       (define-key meow-normal-state-keymap (kbd "C-'") 'repeat-fu-execute)
       (define-key meow-insert-state-keymap (kbd "C-'") 'repeat-fu-execute)))))
;;;




;;; Package:
(provide 'my-utility)
;;; my-utility.el ends here
