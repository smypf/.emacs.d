;;;; my-node.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;; 

;;; Code:

;; TODO
;; 1. Should this be removed? It's heavier and I don't know if it's actually providing lots of utility
;; 1. Does this belong here?
(use-package tide
:defer t
  :ensure t
  :hook
  ((typescript-mode . tide-setup)
   (typescript-mode . tide-hl-identifier-mode)))



;; Running M-x compile will allow to jumping to errors in the output
;; https://emacs.stackexchange.com/a/44708
(require 'compile)
(defun add-node-error-regex ()
  (setq compilation-error-regexp-alist-alist
	;; Tip: M-x re-builder to test this out
	(cons '(node "\\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?"
		     1 ;; file
		     2 ;; line
		     3 ;; column
		     )
	      compilation-error-regexp-alist-alist))
  (add-to-list 'compilation-error-regexp-alist 'node))
(add-hook 'after-init-hook 'add-node-error-regex)


;;; Package:
(provide 'my-node)
;;; my-node.el ends here
