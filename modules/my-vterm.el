;;;; my-vterm.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;; 

;;; Code:

(use-package vterm
  :after evil
  :defer t
  :ensure t
  :config
  (evil-set-initial-state 'vterm-mode 'insert))

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix leader
 "'" 'vterm)


;;; Package:
(provide 'my-vterm)
;;; my-vterm.el ends here
