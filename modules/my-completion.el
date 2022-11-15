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

  ;; Bind TAB and Shift-TAB to next and previous option.
  ;; This allows for completion at point in a way that is useful for me
  ;; This is adapted from https://github.com/minad/vertico/issues/143
  :bind (:map vertico-map
              ("TAB" . vertico-next)
              ("S-TAB" . vertico-previous))

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
;;  (setq minibuffer-prompt-properties
;;        '(read-only t cursor-intangible t face minibuffer-prompt))
;;  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  ;; (setq enable-recursive-minibuffers t)
    ;; TAB cycle if there are only few candidates

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;;(setq read-extended-command-predicate
  ;;      #'command-completion-default-include-p)

  ;; Completion using vertico as the completion ui.
  (setq completion-cycle-threshold 2)
  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  ;; https://emacsredux.com/blog/2016/01/31/use-tab-to-indent-or-complete/
  (setq tab-always-indent 'complete))

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

(use-package marginalia
  :defer t
  :ensure t
  :config
  (marginalia-mode))

(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev))        ;; or dabbrev-completion
;;;;;;         ("C-c p h" . cape-history)
;;;;;;         ("C-c p f" . cape-file)
;;;;;;         ("C-c p k" . cape-keyword)
;;;;;;         ("C-c p s" . cape-symbol)
;;;;;;         ("C-c p a" . cape-abbrev)
;;;;;;         ("C-c p i" . cape-ispell)
;;;;;;         ("C-c p l" . cape-line)
;;;;;;         ("C-c p w" . cape-dict)
;;;;;;         ("C-c p \\" . cape-tex)
;;;;;;         ("C-c p _" . cape-tex)
;;;;;;         ("C-c p ^" . cape-tex)
;;;;;;         ("C-c p &" . cape-sgml)
;;;;;;         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

(use-package backward-forward
  :init
  (backward-forward-mode))

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
