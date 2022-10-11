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

;;; Package:
(provide 'my-defaults)
;;; my-defaults.el ends here
