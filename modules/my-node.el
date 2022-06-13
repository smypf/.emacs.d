;;;; my-node.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;; 

;;; Code:

;; Use typescipt-mode instead of tide.
;; LSP is doing most of the heavy lifting anyway
(use-package typescript-mode)

;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia
;; TODO This could probably go somewhere else
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

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
