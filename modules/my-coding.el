;;;; my-coding.el --- Making working with code easier -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;; Customisations for making it easier to work with code

;;; Code:

;; Searching for references

;; Navigate camelCaseWords easier
(global-subword-mode)

;; Search for whatever is under the cursor
(defun find-references-at-point ()
  (interactive)
  (xref-find-references (thing-at-point 'symbol)))

(use-package eglot
  :defer 3
  :config
  (setq eglot-autoshutdown t
	eglot-send-changes-idle-time 1
	;; Remove logging to speed up eglot when using TypeScript / Javascript
	;; https://www.reddit.com/r/emacs/comments/vau4x1/comment/ic6wd9i/
	eglot-events-buffer-size 0
	))

(use-package consult-eglot
  :after consult
  :defer t)

;; Found via Hacker News
;; https://news.ycombinator.com/item?id=32903246
;; https://masukomi.github.io/private_comments/
;; https://github.com/masukomi/private_comments
(use-package private-comments-mode
  :defer t)

;; Ensure that the docset is activated. If this is not done results will not be shown.
;; See line ~20 of modules/my-node.el
(use-package dash-docs
  :defer t)
(use-package consult-dash
  :defer t
  :after dash-docs)
  ;; These lines have been disabled since I prefer to search for something myself.
  ;; :config
  ;; Use the symbol at point as initial search term
  ;; (consult-customize consult-dash :initial (thing-at-point 'symbol)))

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix leader
 "d" 'consult-dash)

(defun xref-list-references()
  (interactive)
  (setq xref-show-xrefs-function 'xref--show-xref-buffer)
  (evil-collection-define-key 'normal 'xref--xref-buffer-mode-map (kbd "RET") 'xref-quit-and-goto-xref)
  (find-references-at-point)
  (setq xref-show-xrefs-function 'consult-xref))

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix leader
 "cd" 'xref-find-definitions
 "cD" 'find-references-at-point
 "cl" 'xref-list-references
 "cr" 'eglot-rename
 "ce" 'consult-flymake
 "cs" 'consult-eglot-symbols)

;; (evil-collection-define-key 'normal 'xref--xref-buffer-mode-map (kbd "RET") 'xref-quit-and-goto-xref)
;; (general-define-key
;;  :states 'normal
;;  :keymap 'xref--xref-buffer-mode-map
;;  (kbd "RET") 'xref-quit-and-goto-xref)

(use-package ansi-color
  ;; turn off ensure for this pre-installed package
  ;; https://github.com/jwiegley/use-package/issues/977
  :defer t
  :ensure nil
  :init
  (ansi-color-for-comint-mode-off)
  ;; (setq ansi-color-for-comint-mode 'filter)
  :hook (compilation-filter . ansi-color-compilation-filter))

;; Automatically insert matching pair for delimiters
(electric-pair-mode 1)
;; Prevent electric 
(setq electric-pair-preserve-balance nil)



;;; (use-package highlight-indent-guides
;;;   :defer 3
;;;   :hook
;;;   (prog-mode . highlight-indent-guides-mode)
;;;   :config
;;;   (setq highlight-indent-guides-method 'character
;;; 	highlight-indent-guides-responsive 'stack
;;; 	highlight-indent-guides-auto-enabled nil))

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix leader
 "ti" 'highlight-indent-guides-mode)

;;; (use-package tree-sitter
;;;   :config
;;;     (global-tree-sitter-mode))
;;; 
;;; (use-package tree-sitter-langs
;;;   :after tree-sitter)
;;; 
;;; (use-package fancy-narrow
;;;   :after tree-sitter)
;;; 
;;; ;; https://blog.meain.io/2022/more-treesitter-emacs/
;;; ;; https://github.com/meain/dotfiles/blob/34ef5e3331757ac32dd066f5baa54f76cf78211b/emacs/.config/emacs/init.el#L2213
;;; (use-package evil-textobj-tree-sitter
;;;   :config
;;;   ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
;;;   (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
;;;   ;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
;;;   (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
;;; 
;;;   ;(define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.outer"))
;;;   ;(define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.inner"))
;;;   ;(define-key evil-outer-text-objects-map "C" (evil-textobj-tree-sitter-get-textobj "comment.outer"))
;;;   ;(define-key evil-inner-text-objects-map "C" (evil-textobj-tree-sitter-get-textobj "comment.outer"))
;;;   (define-key evil-outer-text-objects-map "o" (evil-textobj-tree-sitter-get-textobj "loop.outer"))
;;;   (define-key evil-inner-text-objects-map "o" (evil-textobj-tree-sitter-get-textobj "loop.inner"))
;;;   (define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "conditional.outer"))
;;;   (define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "conditional.inner"))
;;;   (define-key evil-inner-text-objects-map "r" (evil-textobj-tree-sitter-get-textobj "parameter.inner"))
;;;   (define-key evil-outer-text-objects-map "r" (evil-textobj-tree-sitter-get-textobj "parameter.outer"))
;;; 
;;;   ;; COMMENT Is recenter required?
;;;   ;; TODO Only recenter if the target is not within the viewport
;;;   (defun meain/goto-and-recenter (group &optional previous end query)
;;;     (interactive)
;;;     (evil-textobj-tree-sitter-goto-textobj group previous end query)
;;;     (recenter 7))
;;; 
;;;   ;; TODO change this to general
;;;   ;; TODO Extract the function
;;;   (define-key evil-normal-state-map (kbd "]r") (lambda () (interactive) (meain/goto-and-recenter "parameter.inner")))
;;;   (define-key evil-normal-state-map (kbd "[r") (lambda () (interactive) (meain/goto-and-recenter "parameter.inner" t)))
;;;   (define-key evil-normal-state-map (kbd "]R") (lambda () (interactive) (meain/goto-and-recenter "parameter.inner" nil t)))
;;;   (define-key evil-normal-state-map (kbd "[R") (lambda () (interactive) (meain/goto-and-recenter "parameter.inner" t t)))
;;;   (define-key evil-normal-state-map (kbd "]c") (lambda () (interactive) (meain/goto-and-recenter "conditional.outer")))
;;;   (define-key evil-normal-state-map (kbd "[c") (lambda () (interactive) (meain/goto-and-recenter "conditional.outer" t)))
;;;   (define-key evil-normal-state-map (kbd "]C") (lambda () (interactive) (meain/goto-and-recenter "conditional.outer" nil t)))
;;;   (define-key evil-normal-state-map (kbd "[C") (lambda () (interactive) (meain/goto-and-recenter "conditional.outer" t t)))
;;;   ;(define-key evil-normal-state-map (kbd "]c") (lambda () (interactive) (meain/goto-and-recenter "class.outer")))
;;;   ;(define-key evil-normal-state-map (kbd "[c") (lambda () (interactive) (meain/goto-and-recenter "class.outer" t)))
;;;   ;(define-key evil-normal-state-map (kbd "]C") (lambda () (interactive) (meain/goto-and-recenter "class.outer" nil t)))
;;;   ;(define-key evil-normal-state-map (kbd "[C") (lambda () (interactive) (meain/goto-and-recenter "class.outer" t t)))
;;;   (define-key evil-normal-state-map (kbd "]f") (lambda () (interactive) (meain/goto-and-recenter "function.outer")))
;;;   (define-key evil-normal-state-map (kbd "[f") (lambda () (interactive) (meain/goto-and-recenter "function.outer" t)))
;;;   (define-key evil-normal-state-map (kbd "]F") (lambda () (interactive) (meain/goto-and-recenter "function.outer" nil t)))
;;;   (define-key evil-normal-state-map (kbd "[F") (lambda () (interactive) (meain/goto-and-recenter "function.outer" t t))))
;;; 
;;; ;; Fancy narrow to textobj
;;; (use-package emacs
;;;   :ensure nil
;;;   :commands (meain/fancy-narrow-to-thing)
;;;   :config
;;;   (defun meain/fancy-narrow-to-thing (thing)
;;;     (interactive)
;;;     (if (buffer-narrowed-p) (fancy-widen))
;;;     (let ((range (evil-textobj-tree-sitter--range 1 (list (intern thing)))))
;;;       (fancy-narrow-to-region (car range) (cdr range))))
;;;   ;; TODO rewrite to general
;;;   (general-define-key
;;;    :states 'normal
;;;    :keymaps 'override
;;;    :prefix leader
;;;    "nn" (lambda () (interactive) (fancy-widen))
;;;    "nf" (lambda () (interactive) (meain/fancy-narrow-to-thing "function.outer"))
;;;    "nc" (lambda () (interactive) (meain/fancy-narrow-to-thing "class.outer"))
;;;    "nC" (lambda () (interactive) (meain/fancy-narrow-to-thing "comment.outer"))
;;;    "no" (lambda () (interactive) (meain/fancy-narrow-to-thing "loop.outer"))
;;;    "nc" (lambda () (interactive) (meain/fancy-narrow-to-thing "conditional.outer"))
;;;    "nr" (lambda () (interactive) (meain/fancy-narrow-to-thing "parameter.outer"))))



;; TODO install https://github.com/flymake/emacs-flymake instead of the builtin one?
;; Are these the same or different?

;; TODO Add "K" = eldoc-doc-buffer as an alternative to C-h .
;; TODO add and remove the binding when eglot is toggled

;; Show the error at the cursor in the mini-buffer
(use-package flymake-cursor
  :defer t
  :after flymake
  :hook
  (prog-mode . flymake-cursor-mode))

;; Enable folding code sections
(use-package emacs
  :ensure nil
  :hook
  (prog-mode . hs-minor-mode))

;;; Package:
(provide 'my-coding)
;;; my-coding.el ends here
