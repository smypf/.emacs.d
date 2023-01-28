;;;; my-defaults.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;; Set emacs defaults on a system wide basis

;;; Code:

;; Ensure spaces are used, and indentation width is 4 characters
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; For some reason esc u u is bound to 'upcase-word which is a friction point for me.
;; This unbinds it.
;; See https://emacs.stackexchange.com/questions/14755/how-to-remove-bindings-to-the-esc-prefix-key for more information
(define-key esc-map "u" nil)

(repeat-mode)

(setq electric-pair-preserve-balance t
      electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit
      electric-pair-mode t)

;; Confirm closing emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; ediff
;; Split windows horizontally
(setq ediff-split-window-function 'split-window-horizontally)
;; Prevent ediff from creating new frames
;; https://emacs.stackexchange.com/questions/17064/never-create-frame-in-ediff
;;(defun ediff-window-display-p () nil)
(advice-add 'ediff-window-display-p :override #'ignore)
(setq ediff-window-setup-function #'ediff-setup-windows-plain)

;;; Package:
(provide 'my-defaults)
;;; my-defaults.el ends here
