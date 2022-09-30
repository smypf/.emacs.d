;;;; my-node.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;; 

;;; Code:

;; Use typescipt-mode instead of tide.
;; LSP is doing most of the heavy lifting anyway
;; https://vxlabs.com/2022/06/12/typescript-development-with-emacs-tree-sitter-and-lsp-in-2022/ for more information
(use-package typescript-mode
  :config
  (add-hook 'typescript-mode-hook 'eglot-ensure)
  :dash "TypeScript" "JavaScript" "NodeJS" "HTML" "CSS")

;; From the wiki
(with-eval-after-load 'eglot
   (setq completion-category-defaults nil))

;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia
;; TODO This could probably go somewhere else
;; I think that this may be slowing down things
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
        ;; Passing tests
	;; (cons '(compilation "\\(?:[\(\\)?\\(src/[a-zA-Z\.0-9_/-]+\\.spec.ts)?$")
	;; 	1
	;; 	))
  (add-to-list 'compilation-error-regexp-alist 'node))
(add-hook 'after-init-hook 'add-node-error-regex)


;;; Package:
(provide 'my-node)
;;; my-node.el ends here
