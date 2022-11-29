;;;; my-tab-bar.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;; Copied from http://www.gonsie.com/blorg/tab-bar.html

;;; Code:

(use-package emacs
  :ensure nil
  :config
  (when (< 26 emacs-major-version)
    (tab-bar-mode 1)                           ;; enable tab bar
    (setq tab-bar-show 1)                      ;; hide bar if <= 1 tabs open
    (setq tab-bar-close-button-show nil)       ;; hide tab close / X button
    (setq tab-bar-new-tab-choice "*scratch*");; buffer to show in new tabs
    (setq tab-bar-tab-hints t)                 ;; show tab numbers
    (setq tab-bar-select-tab-modifiers "super")
    (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator)))
  (global-set-key (kbd "s-{") 'tab-bar-switch-to-prev-tab)
  (global-set-key (kbd "s-}") 'tab-bar-switch-to-next-tab)
  (global-set-key (kbd "s-t") 'tab-bar-new-tab)
  (global-set-key (kbd "s-w") 'tab-bar-close-tab))
  ;; elements to include in bar

;;; Package:
(provide 'my-tab-bar)
;;; my-tab-bar.el ends here
