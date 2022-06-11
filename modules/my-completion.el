;;;; my-completion.el --- Make it quicker to do stuff  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;; Functionality which allows for doing more with fewer keystrokes

;;; Code:

;; Vertico is a nice completion package
(use-package vertico
  :defer t
  :init
  (vertico-mode)
  (setq vertico-count 15))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :config
  ;; TODO get this working properly
  (define-key vertico-map (kbd "M-h") 'vertico-directory-up))

(general-define-key
 :states 'normal
 :keymaps 'override
 " " 'vertico-find)

(use-package consult
  :defer t
  :init
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)
  :config
  (autoload 'projectile-project-root "projectile"))

(use-package orderless
  :init
  (setq completion-styles '(orderless flex)))

;; TODO Replace this with corfu
;; https://github.com/minad/corfu
(use-package company
  :defer 2
  :config
  ;; Prevent Company completion in Text Mode from being converted into lowercase
  (setq company-dabbrev-downcase nil))

;; Corfu
;; Perhaps use this
;; https://github.com/minad/corfu#transfer-completion-to-the-minibuffer
;; Otherwise use corfu-terminal
;; https://codeberg.org/akib/emacs-corfu-terminal


;; I don't know what the point of embark is
;; (use-package marginalia
;;   :ensure t
;;   :config
;;   (marginalia-mode))
;; 
;; 
;; (use-package embark
;;   :ensure t
;; 
;;   :bind
;;   (("M-." . embark-act)         ;; pick some comfortable binding
;;    ("M-/" . embark-dwim)        ;; good alternative: M-.
;;    ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
;; 
;;   :init
;; 
;;   ;; Optionally replace the key help with a completing-read interface
;;   (setq prefix-help-command #'embark-prefix-help-command)
;; 
;;   :config
;; 
;;   ;; Hide the mode line of the Embark live/completions buffers
;;   (add-to-list 'display-buffer-alist
;;                '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;;                  nil
;;                  (window-parameters (mode-line-format . none)))))
;; 
;; ;; Consult users will also want the embark-consult package.
;; (use-package embark-consult
;;   :ensure t
;;   :after (embark consult)
;;   :demand t ; only necessary if you have the hook below
;;   ;; if you want to have consult previews as you move around an
;;   ;; auto-updating embark collect buffer
;;   :hook
;;   (embark-collect-mode . consult-preview-at-point-mode))
;; 
;; 
;; 
;; ;; Use Embark to show bindings in a key prefix with `C-h`
;; (setq prefix-help-command #'embark-prefix-help-command)
;; 
;; (with-eval-after-load 'embark-consult
;;   (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix leader
 ":" 'execute-extended-command)

;;; Package:
(provide 'my-completion)
;;; my-completion.el ends here
