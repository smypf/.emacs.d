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

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix leader
 "cd" 'xref-find-definitions
 "cD" 'find-references-at-point
 "cr" 'eglot-rename
 "ce" 'consult-flymake
 "cs" 'consult-eglot-symbols)

(use-package ansi-color
  ;; turn off ensure for this pre-installed package
  ;; https://github.com/jwiegley/use-package/issues/977
  :ensure nil
  :init
  (ansi-color-for-comint-mode-off)
  ;; (setq ansi-color-for-comint-mode 'filter)
  :hook (compilation-filter . ansi-color-compilation-filter))

;; Automatically insert matching pair for delimiters
(electric-pair-mode 1)

(use-package highlight-indent-guides
  :defer 3
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character
	highlight-indent-guides-responsive 'stack
	highlight-indent-guides-auto-enabled nil))

;; TODO install https://github.com/flymake/emacs-flymake instead of the builtin one?
;; Are these the same or different?

;;; Package:
(provide 'my-coding)
;;; my-coding.el ends here
