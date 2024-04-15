;;;; my-golang.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;;

;;; Code:
(defun go-install-save-hooks ()
  (add-hook 'before-save-hook #'eglot-format-buffer t t)
  (add-hook 'before-save-hook #'eglot-code-action-organize-imports t t))

(setq-default eglot-workspace-configuration
              '((:gopls .
                        ((staticcheck . t)
                         (matcher . "CaseSensitive")))))
(add-hook 'go-ts-mode-hook 'eglot-ensure)
(add-hook 'go-ts-mode-hook 'go-install-save-hooks)

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))

(use-package flycheck-golangci-lint
  :defer t
  :after flycheck
  :ensure t
  :hook (go-ts-mode . flycheck-golangci-lint-setup))

(require 'compile)
(defun add-go-error-regex ()
  (setq compilation-error-regexp-alist-alist
        ;; Tip: M-x re-builder to test this out
        (cons '(go "Error Trace:\t\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\)"
                     1 ;; file
                     2 ;; line
                     )
              compilation-error-regexp-alist-alist))
  ;; Passing tests
  ;; (cons '(compilation "\\(?:[\(\\)?\\(src/[a-zA-Z\.0-9_/-]+\\.spec.ts)?$")
  ;;  1
  ;;  ))
  (add-to-list 'compilation-error-regexp-alist 'go))
(add-hook 'go-ts-mode 'add-go-error-regex)

;;; Package:
(provide 'my-golang)
;;; my-golang.el ends here
