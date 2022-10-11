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

;; For some reason esc u u is bound to 'upcase-word which is a friction point.
;; This unbinds it.
(define-key esc-map "u" nil)

;;; Package:
(provide 'my-defaults)
;;; my-defaults.el ends here
