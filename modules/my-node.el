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
;; Removed in favour of typescript-ts-mode (see https://github.com/emacs-typescript/typescript.el/tree/4fcb4594819caf472ae42ea068a1c7795cf07f46#a-short-note-on-development-halt)
;;;(use-package typescript-mode
;;;  :defer t
;;;  :config
;;;  (add-hook 'typescript-mode-hook 'eglot-ensure)
;;;  :dash "TypeScript" "JavaScript" "NodeJS" "HTML" "CSS")

;; note to get this working I needed to copy the files in ~/.emacs.d/elpa/tree-sitter-langs/bin to ~/.emacs.d/tree-sitter and prefix the file with "libtree-sitter-" (e.g. "typescript.dylib" is renamed to "libtree-sitter-typescript.dylib")
;; (setq atlassian-tab-width 2)

;; From the wiki
(with-eval-after-load 'eglot
  (setq completion-category-defaults nil)
  ;; TODO this doesn't work as the hook doesn't exist. Need to determine an automatic way to eglot-ensure
  (add-to-list 'eglot-server-programs
			   '((typescript-ts-mode) . ("typescript-language-server" "--stdio"))
			   '((tsx-ts-mode) . ("typescript-language-server" "--stdio"))))

(setq typescript-ts-mode-indent-offset (symbol-value 'tab-width))
;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia
;; TODO This could probably go somewhere else
;; I think that this may be slowing down things
(use-package apheleia
  :defer t
  :ensure t
  :config
  ;; Prevent Apheleia from being enabled for files I don't care about
  (defun smypf/inhibit-apheleia-files ()
	"Return false if the buffer file contains 'node_modules' or '.d.ts'."
	(message "testing if apheleia should be inhibited")
	(let ((file-name (buffer-file-name)))
	  (if (or (and file-name (string-match-p "node_modules" file-name))
			  (and file-name (string-match-p "\\.d\\.ts$" file-name)))
		  (progn
			(message "smypf/inhibit-apheleia-files contains 'node_modules' or '.d.ts'.")
			t)
		(progn
		  (message "it was not found")
		  nil))))

  (add-to-list 'apheleia-inhibit-functions #'smypf/inhibit-apheleia-files))

;; Running M-x compile will allow to jumping to errors in the output
;; https://emacs.stackexchange.com/a/44708
(require 'compile)
(add-to-list 'compilation-error-regexp-alist-alist
	         ;; Tip: M-x re-builder to test this out
	         '(tsx "(\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\))"
				   1 ;; file
				   2 ;; line
				   3 ;; column
				   ))
;; Passing tests
;; (cons '(compilation "\\(?:[\(\\)?\\(src/[a-zA-Z\.0-9_/-]+\\.spec.ts)?$")
;;  1
;;  ))
(add-to-list 'compilation-error-regexp-alist 'tsx)


;; Changed based on https://www.reddit.com/r/emacs/comments/4xhxfw/comment/d6ghhmq/?utm_source=share&utm_medium=web2x&context=3
(add-hook 'typescript-ts-base-mode
		  (lambda ()
			(add-to-list (make-local-variable 'electric-pair-pairs)
						 (cons ?` ?`))))

(add-hook 'compilation-mode-hook (lambda () (goto-address-mode -1)))
(add-hook 'compilation-mode-hook (lambda () (goto-address-prog-mode -1)))
;; Atlassian uses tabs
;; (add-hook 'jtsx-tsx-mode 'indent-tabs-mode)
;; (add-hook 'jtsx-jsx-mode 'indent-tabs-mode)
;; (add-hook 'json-ts-mode 'indent-tabs-mode)

;;; Package:
(provide 'my-node)
;;; my-node.el ends here
