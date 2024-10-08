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
        ;; eglot-events-buffer-size 0)

        ;; More speed
        ;; https://www.reddit.com/r/emacs/comments/1b25904/comment/ksj593p/
        ;; (fset #'jsonrpc--log-event #'ignore))
        ;; (setf (plist-get eglot-events-buffer-config :size) 0))
        ))

;; This wasn't that useful
;; (use-package eldoc-box
;;   :after eglot
;;   :hook
;;   (prog-mode . eldoc-box-hover-mode))

(use-package consult-eglot
  :after consult
  :defer t)

(use-package eglot-booster
  :after eglot
  :config	(eglot-booster-mode))

;; Found via Hacker News
;; https://news.ycombinator.com/item?id=32903246
;; https://masukomi.github.io/private_comments/
;; https://github.com/masukomi/private_comments
(use-package private-comments-mode
  :defer t)

;; Ensure that the docset is activated. If this is not done results will not be shown.
;; See line ~20 of modules/my-node.el
;; removed as these aren't being used.
;;;(use-package dash-docs
;;;  :defer t)
;;;(use-package consult-dash
;;;  :defer t
;;;  :after dash-docs)
;; These lines have been disabled since I prefer to search for something myself.
;; :config
;; Use the symbol at point as initial search term
;; (consult-customize consult-dash :initial (thing-at-point 'symbol)))

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

;; Removed - These were causing too many errors
;; (use-package highlight-indent-guides
;;   :defer t
;;   :hook
;;   (prog-mode . highlight-indent-guides-mode)
;;   :bind (("C-c t g" . highlight-indent-guides-mode))
;;   :config
;;   (setq highlight-indent-guides-method 'bitmap
;;         highlight-indent-guides-responsive 'stack
;;         highlight-indent-guides-auto-enabled nil))

;; `M-x combobulate' (or `C-c o o') to start using Combobulate
;; Do not forget to customize Combobulate to your liking:
;;
;;  M-x customize-group RET combobulate RET
;;

(use-package combobulate
  ;; Optional, but recommended.
  ;;
  ;; You can manually enable Combobulate with `M-x
  ;; combobulate-mode'.
  :hook ((python-ts-mode . combobulate-mode)
         (js-ts-mode . combobulate-mode)
         (css-ts-mode . combobulate-mode)
         (yaml-ts-mode . combobulate-mode)
         ;;(typescript-ts-mode . combobulate-mode)
         (tsx-ts-mode . combobulate-mode)))

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

;; (use-package fancy-narrow
;;   :after tree-sitter)

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
;;(use-package flymake-cursor
;;  :defer t
;;  :after flymake
;;  :hook
;;  (prog-mode . flymake-cursor-mode))

(setq-default show-trailing-whitespace nil)

(use-package flycheck
  :defer t
  :ensure t
  :init (global-flycheck-mode))

(use-package flycheck-eglot
  :ensure t
  :defer t
  :after (flycheck eglot)
  :init (global-flycheck-eglot-mode))


;; This wasn't being used much
;; (use-package hl-todo
;;   :ensure t)

;; (defun flycheck-hl-todo-follow-mode ()
;;   (setq flycheck-hl-todo-enabled hl-todo-mode)
;;   ;; Force flycheck update
;;   (flycheck-buffer))

;; (use-package flycheck-hl-todo
;;   :ensure t
;;   :defer t ; Need to be initialized after the rest of checkers
;;   :after (flycheck)
;;   :hook
;;   (hl-todo-mode-hook . flycheck-hl-todo-follow-mode)
;;   :config
;;   (flycheck-hl-todo-setup))

(use-package consult-flycheck
  :after flycheck-eglot
  :defer t)

(use-package markdown-mode
  :defer t
  :after eglot)

;;;(use-package eldoc-box
;;;  :defer t
;;;  :after eglot
;;;  :commands (eldoc-box-help-at-point)
;;;  :config
;;;  (setq eldoc-echo-area-use-multiline-p nil)
;;;  :bind (("C-c c i" . eldoc-box-help-at-point)))
;;;
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
  (prog-mode . (lambda ()
                 (setq-local show-trailing-whitespace t))))

(use-package json-mode
  :defer t)

(use-package editorconfig
  :defer t
  :config
  (editorconfig-mode 1))

;; Removed... Never used
;; (use-package realgud
;;   :defer t
;;   :commands realgud)

;; Removed... Never used
;; (use-package realgud-trepan-ni
;;   :defer t
;;   :commands trepan-ni realgud:trepan-ni)

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

;;; Package:
(provide 'my-coding)
;;; my-coding.el ends here
