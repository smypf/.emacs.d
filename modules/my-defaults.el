;;;; my-defaults.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:
;; Set Emacs defaults on a system wide basis

;;; Code:

(use-package emacs
  :ensure nil
  :config
  ;; Ensure spaces are used, and indentation width is 4 characters
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)

  (cond
   ((file-exists-p "/opt/homebrew/bin/fish")
	(setq shell-file-name "/opt/homebrew/bin/fish")))

  ;; For some reason esc u u is bound to 'upcase-word which is a friction point for me.
  ;; This unbinds it.
  ;; See https://emacs.stackexchange.com/questions/14755/how-to-remove-bindings-to-the-esc-prefix-key for more information
  (define-key esc-map "u" nil)

  (repeat-mode)

  ;; https://stackoverflow.com/questions/27142996/electric-pair-mode-dont-pair-if-cursor-precedes-a-non-whitespace-character
  (setq electric-pair-preserve-balance nil
		electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)

  ;; Confirm closing emacs
  (setq confirm-kill-emacs 'y-or-n-p)

  ;; ;; Change to y-or-n-p when killing buffer that contains changes.
  ;; ;; https://emacs.stackexchange.com/questions/22569/kill-buffer-with-y-or-n-p-instead-of-yes-or-no-p
  ;; (defun yes-or-no-p@maybe-just-y-or-n-p (orig-fun prompt)
  ;; (funcall
  ;;  (if (eq this-command 'kill-buffer)
  ;;      #'y-or-n-p
  ;;    #'yes-or-no-p)
  ;;  prompt))

  ;; (advice-add 'yes-or-no-p :around #'yes-or-no-p@maybe-just-y-or-n-p)

                                        ; (defun yes-or-no-p->-y-or-n-p (orig-fun &rest r)
                                        ;   (cl-letf (((symbol-function 'yes-or-no-p) #'y-or-n-p))
                                        ;     (apply orig-fun r)))

                                        ;(advice-add 'kill-buffer :around #'yes-or-no-p->-y-or-n-p)

  ;; ediff
  ;; Split windows horizontally
  (setq ediff-split-window-function 'split-window-horizontally)
  ;; Prevent ediff from creating new frames
  ;; https://emacs.stackexchange.com/questions/17064/never-create-frame-in-ediff
  ;;(defun ediff-window-display-p () nil)
  (advice-add 'ediff-window-display-p :override #'ignore)
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)

  ;; disable scrollbars in vertical frames
  (add-to-list 'default-frame-alist
			   '(vertical-scroll-bars . nil))

  ;; Change the minimum level of messages to error
                                        ;(setq native-comp-async-report-warnings-errors 'silent)
                                        ;(setq warning-minimum-level :error)

  ;; move backups to a different directory to prevent it being present in the git status
  ;; https://stackoverflow.com/a/151946
  (setq backup-directory-alist `(("." . "~/.emacs-backups"))))

;;; Package:
(provide 'my-defaults)
;;; my-defaults.el ends here
