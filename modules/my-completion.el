;;;; my-completion.el --- Make it quicker to do stuff  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;; Functionality which allows for doing more with fewer keystrokes

;;; Code:

;; Vertico is a nice completion package
;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)
  (vertico-multiform-mode)
  :config
  ;; Show more candidates
  (setq vertico-count 15
	;; Configure the display per command.
	;; Use a buffer with indices for imenu
	;; and a flat (Ido-like) menu for M-x.
	vertico-multiform-commands '((consult-ripgrep buffer indexed)
				     (consult-eglot-symbols buffer indexed))

	;; Display ripgrep candidates in a buffer on the right of the page
	vertico-buffer-display-action
	'(display-buffer-in-side-window
	  (side . right)
	  (window-width . 0.3)))
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :ensure nil
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  ;; (setq enable-recursive-minibuffers t)
  )

;; TODO get this working properly
;; (use-package vertico-directory
;;   :after vertico
;;   :ensure nil
;;   :config
;;   (define-key vertico-map (kbd "M-h") 'vertico-directory-up))


;; TODO Optionally close vertico only when I want to.
;; E.g. Pressing S-Enter will keep the mini buffer around allowing me to return to it (somehow)
;; Look into (setq enable-recursive-minibuffers t)
;; https://www.reddit.com/r/emacs/comments/qkgnhe/comment/hixh0f7

(use-package vertico-repeat
  :after vertico
  :defer t
  :ensure nil)

(use-package consult
  :defer t
  :init
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)
  :config
  (autoload 'projectile-project-root "projectile")

  :hook
  (completion-list-mode . consult-preview-at-point-mode))

(use-package orderless
  :defer t
  :init
  (setq completion-styles '(orderless flex)))

;; Corfu
;; Perhaps use this
(use-package corfu
  :defer t
  :config
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
	  completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  (define-key corfu-map "\M-m" #'corfu-move-to-minibuffer)
  ;; Optional customizations
  :custom
  (corfu-cycle t)                   ;; Enable cycling for `corfu-next/previous'
  (corfu-auto nil)                  ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current t)         ;; Enable current candidate preview
  (corfu-preselect-first t)         ;; Enable candidate preselection
  (corfu-on-exact-match 'quit)      ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  :bind
  (:map corfu-map
	;; Tab will select the next option
	("TAB" . corfu-next)
	([tab] . corfu-next)
	;; Shift-Tab will select the previous option
	("S-TAB" . corfu-previous)
	([backtab] . corfu-previous)
	;; Enter will select the option
	([enter] . corfu-complete))

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

;; https://github.com/minad/corfu#transfer-completion-to-the-minibuffer

;; Allow usage of corfu in terminal windows
(use-package corfu-terminal
  :defer t
  :after corfu
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

;; A few more useful configurations...
(use-package emacs
  :ensure nil
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 2)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  ;; https://emacsredux.com/blog/2016/01/31/use-tab-to-indent-or-complete/
  (setq tab-always-indent 'complete))

(use-package marginalia
  :defer t
  :ensure t
  :config
  (marginalia-mode))


;; Embark Export (M-. E) will send all results to a buffer.
;; This is useful as it means that it is not necessary to continuously open the consult buffer to visit subsequent matches
;; This is enough for me to enable this.
(use-package embark
  :defer t
  :ensure t
  :bind
  (("M-." . embark-act)         ;; pick some comfortable binding
   ("M-/" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))



;; Use Embark to show bindings in a key prefix with `C-h`
(setq prefix-help-command #'embark-prefix-help-command)

(with-eval-after-load 'embark-consult
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

(defun search-thing-at-point ()
  (interactive)
  (consult-ripgrep (projectile-project-root) (thing-at-point 'symbol)))

;; Enable IDO for file completion
;; I don't like this
;; (ido-mode)

;;; Package:
(provide 'my-completion)
;;; my-completion.el ends here
