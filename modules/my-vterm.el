;;;; my-vterm.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;;

;;; Code:

(use-package vterm
  :defer t
  :ensure t
  :hook
  ;; Disable line numbers for vterm mode
  (vterm-mode . (lambda() (display-line-numbers-mode -1)))
)

;; (general-define-key
;;  :states 'normal
;;  :keymaps 'override
;;  :prefix leader
;;  "'" 'vterm)

;;; Package:
(provide 'my-vterm)
;;; my-vterm.el ends here
