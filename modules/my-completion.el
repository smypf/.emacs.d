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


;; TODO is this useful
;; TODO convert this
(require 'embark)
(require 'embark-consult)

(global-set-key [remap describe-bindings] #'embark-bindings)
(global-set-key (kbd "C-.") 'embark-act)



;; Use Embark to show bindings in a key prefix with `C-h`
(setq prefix-help-command #'embark-prefix-help-command)

(with-eval-after-load 'embark-consult
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

;;; Package:
(provide 'my-completion)
;;; my-completion.el ends here
