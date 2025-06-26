;;;; my-coding.el --- Making working with code easier -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;; Customisations for making it easier to work with code

;;; Code:

;; Navigate camelCaseWords easier
(global-subword-mode)

;; Search for whatever is under the cursor
(defun find-references-at-point ()
  (interactive)
  (xref-find-references (thing-at-point 'symbol)))

;; This shouldn't be here.
;; There's a dependency that this needs to be first which I don't like
;; (use-package flycheck
;;   :ensure t
;;   :commands (global-flycheck-mode)
;;   :config
;;   (global-flycheck-mode)
;;   ((flycheck-add-mode 'javascript-eslint 'typescript-ts-mode)
;;    (flycheck-add-mode 'javascript-eslint 'tsx-ts-mode)
;;    (flycheck-add-mode 'javascript-eslint 'jsx-ts-mode)))

(use-package eglot
  :disabled
  ;; :after flycheck
  :defer nil
  :ensure nil
  :preface
  (defun mp-eglot-eldoc ()
	(setq eldoc-documentation-strategy
			'eldoc-documentation-compose-eagerly))
  :hook ((eglot-managed-mode . mp-eglot-eldoc))
  :config
  (setq eglot-autoshutdown t
		eglot-send-changes-idle-time 1
		;; Remove logging to speed up eglot when using TypeScript / Javascript
		;; https://www.reddit.com/r/emacs/comments/vau4x1/comment/ic6wd9i/
		;; eglot-events-buffer-size 0)

		;; More speed
		;; https://www.reddit.com/r/emacs/comments/1b25904/comment/ksj593p/
		;; (fset #'jsonrpc--log-event #'ignore))
		;; (setf (plist-get eglot-events-buffer-config :size) 0))
		))

(use-package consult-eglot
  :after consult
  :defer t)

;; (use-package eglot-booster
;;   :after eglot
;;   :config	(eglot-booster-mode))

;; Found via Hacker News
;; https://news.ycombinator.com/item?id=32903246
;; https://masukomi.github.io/private_comments/
;; https://github.com/masukomi/private_comments
(use-package private-comments-mode
  :defer t)

(use-package devdocs
  :defer t
  :bind (("C-c L" . devdocs-search)))

(defun xref-list-references()
  (interactive)
  (setq xref-show-xrefs-function 'xref--show-xref-buffer)
  (find-references-at-point)
  (setq xref-show-xrefs-function 'consult-xref))

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
  :commands (ansi-color-for-comint-mode-off)
  :init
  (ansi-color-for-comint-mode-off)
  ;; (setq ansi-color-for-comint-mode 'filter)
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package combobulate
  :config
  (defalias 'combobulate-key-map combobulate-key-map)
  (meow-normal-define-key '("N" . combobulate-key-map))
  (define-key combobulate-key-map "N" 'combobulate)

  ;;:custom
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  ;; (combobulate-key-prefix "C-c l")
  :hook ((prog-mode . combobulate-mode))
  ;; Amend this to the directory where you keep Combobulate's source
  ;; code.
  :load-path ("~/tools/combobulate/"))

(use-package treesit-auto
  :commands (global-treesit-auto-mode)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (setq go-ts-mode-indent-offset (symbol-value 'tab-width))
  ;; This doesn't work as the file name is not in the expected format (it should have tree-sitter- as the prefix)
  (setq treesit-extra-load-path '("/Users/yees6f/.emacs.d/elpa/tree-sitter-langs-20230114.1524/bin/"))
  (push '(css-mode . css-ts-mode) major-mode-remap-alist)
  (push '(python-mode . python-ts-mode) major-mode-remap-alist)
  (push '(javascript-mode . js-ts-mode) major-mode-remap-alist)
  (push '(js-json-mode . json-ts-mode) major-mode-remap-alist)
  ;;(push '(typescript-mode . typescript-ts-mode) major-mode-remap-alist)
  (push '(typescript-mode . tsx-ts-mode) major-mode-remap-alist)
  (push '(go-mode . go-ts-mode) major-mode-remap-alist)
  (push '(c-mode . c-ts-mode) major-mode-remap-alist)
  (push '(c++-mode . c++-ts-mode) major-mode-remap-alist))

(add-hook 'tree-sitter-after-on-hook 'eglot-ensure)

(with-eval-after-load 'eglot
  (setq completion-category-defaults nil)
  ;; TODO this doesn't work as the hook doesn't exist. Need to determine an automatic way to eglot-ensure
  (add-to-list 'eglot-server-programs
			   '((python-ts-mode) . ("pyright-langserver" "--stdio"))))

(use-package tree-sitter-langs
  :after tree-sitter)

(setq-default show-trailing-whitespace nil)

;; (use-package consult-flycheck
;;   :after flycheck-eglot
;;   :defer t)

(use-package markdown-mode
  :defer t
  :after eglot)

;; Enable folding code sections
(use-package emacs
  :ensure nil
  :bind (("C-c z c" . hs-hide-block)
		 ("C-c z o" . hs-show-block)
		 ("C-c z C" . hs-hide-all)
		 ("C-c z O" . hs-show-all))
  :hook
  (prog-mode . hs-minor-mode)
  (prog-mode . whitespace-cleanup)
  ;; Could this be what is causing electric-pair-mode to be unhelpful?
  ;; (prog-mode . electric-pair-mode)
  (prog-mode . (lambda ()
				 (setq-local show-trailing-whitespace t))))

(use-package json-mode
  :defer t)

;; (use-package editorconfig
;;   :defer t
;;   :config
;;   (editorconfig-mode 1))

(use-package surround
  :ensure t
  :defer t
  :bind-keymap ("M-'" . surround-keymap))

(use-package jtsx
  :ensure t
  :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
		 ("\\.tsx?\\'" . jtsx-tsx-mode))
  :commands jtsx-install-treesit-language
  :hook ((jtsx-jsx-mode . hs-minor-mode)
		 (jtsx-tsx-mode . hs-minor-mode)
		 (jtsx-jsx-mode . eglot-ensure)
		 (jtsx-tsx-mode . eglot-ensure))
  :custom
  (js-indent-level (symbol-value 'tab-width))
  (typescript-ts-mode-indent-offset (symbol-value 'tab-width))
  (jtsx-switch-indent-offset (symbol-value 'tab-width))
  (jtsx-indent-statement-block-regarding-standalone-parent nil)
  (jtsx-jsx-element-move-allow-step-out t)
  (jtsx-enable-jsx-electric-closing-element t)
  (jtsx-enable-all-syntax-highlighting-features t)
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . jtsx-tsx-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . jtsx-jsx-mode))
  (add-hook 'tsx-ts-mode-hook 'eglot-ensure)
  (add-hook 'jsx-ts-mode-hook 'eglot-ensure)
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
  (defun jtsx-bind-keys-to-mode-map (mode-map)
	"Bind keys to MODE-MAP."
	(define-key mode-map (kbd "C-c C-j") 'jtsx-jump-jsx-element-tag-dwim)
	(define-key mode-map (kbd "C-c j o") 'jtsx-jump-jsx-opening-tag)
	(define-key mode-map (kbd "C-c j c") 'jtsx-jump-jsx-closing-tag)
	(define-key mode-map (kbd "C-c j r") 'jtsx-rename-jsx-element)
	(define-key mode-map (kbd "C-c <down>") 'jtsx-move-jsx-element-tag-forward)
	(define-key mode-map (kbd "C-c <up>") 'jtsx-move-jsx-element-tag-backward)
	(define-key mode-map (kbd "C-c C-<down>") 'jtsx-move-jsx-element-forward)
	(define-key mode-map (kbd "C-c C-<up>") 'jtsx-move-jsx-element-backward)
	(define-key mode-map (kbd "C-c C-S-<down>") 'jtsx-move-jsx-element-step-in-forward)
	(define-key mode-map (kbd "C-c C-S-<up>") 'jtsx-move-jsx-element-step-in-backward)
	(define-key mode-map (kbd "C-c j w") 'jtsx-wrap-in-jsx-element))

  (defun jtsx-bind-keys-to-jtsx-jsx-mode-map ()
	(jtsx-bind-keys-to-mode-map jtsx-jsx-mode-map))

  (defun jtsx-bind-keys-to-jtsx-tsx-mode-map ()
	(jtsx-bind-keys-to-mode-map jtsx-tsx-mode-map))

  (add-hook 'jtsx-jsx-mode-hook 'jtsx-bind-keys-to-jtsx-jsx-mode-map)
  (add-hook 'jtsx-tsx-mode-hook 'jtsx-bind-keys-to-jtsx-tsx-mode-map))


(use-package dape
  :defer t
  ;; To use window configuration like gud (gdb-mi)
  ;; :init
  ;; (setq dape-buffer-window-arrangment 'gud)
  :config
  ;; Info buffers to the right
  (setq dape-buffer-window-arrangment 'right)

  ;; To remove info buffers
  ;; (remove-hook 'dape-update-ui-hooks 'dape-info-update)

  ;; To remove repl buffer on startup
  ;; (remove-hook 'dape-on-start-hooks 'dape-repl)

  ;; By default dape uses gdb keybinding prefix
  (setq dape-key-prefix "\C-x\C-a")

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-compile-hooks 'kill-buffer)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-on-start-hooks
  ;;           (defun dape--save-on-start ()
  ;;             (save-some-buffers t t)))

  ;; Projectile users
  (setq dape-cwd-fn 'projectile-project-root))

(use-package indent-bars
  :config
  (require 'indent-bars-ts)         ; not needed with straight
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  ;; Add other languages as needed
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
	  if_statement with_statement while_statement)))
  ;; Note: wrap may not be needed if no-descend-list is enough
  ;;(indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
  ;;                      list list_comprehension
  ;;                      dictionary dictionary_comprehension
  ;;                      parenthesized_expression subscript)))

  ;; from https://github.com/jdtsmith/indent-bars/blob/main/examples.md#in-terminal
  ;; allows for non-stipple support
  (indent-bars-prefer-character t)
  (indent-bars-color '(highlight :face-bg t :blend 0.5)
  (indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1))
  (indent-bars-unspecified-fg-color "white")
  (indent-bars-unspecified-bg-color "black")

  :hook ((prog-mode js-ts-mode jsx-ts-mode tsx-ts-mode) . indent-bars-mode)))
;;; Package:
(provide 'my-coding)
;;; my-coding.el ends here
