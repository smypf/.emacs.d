;;;; my-tab-bar.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;; Copied from http://www.gonsie.com/blorg/tab-bar.html

;;; Code:
;; from https://old.reddit.com/r/emacs/comments/t4j5lu/tabbarmode_how_to_change_tab_bar_appearence/hz1uhs4/

(defface smypf-tab-bar-tab
  `((t :inherit 'tab-bar-tab))
  ;;:foreground ,(face-attribute 'font-lock-keyword-face :foreground nil t)))
  "Face for active tab in tab-bar."
  :group 'smypf-tab-bar)

(defface smypf-tab-bar-tab-hint
  `((t :inherit 'default))
  ;;:foreground ,(face-attribute 'tab-bar-tab-inactive :foreground nil t)))
  "Face for active tab hint in tab-bar."
  :group 'smypf-tab-bar)

(defface smypf-tab-bar-tab-inactive
  `((t :inherit 'tab-bar-tab-inactive))
  "Face for inactive tab in tab-bar."
  :group 'smypf-tab-bar)

(defface smypf-tab-bar-tab-hint-inactive
  `((t :inherit 'default))
  "Face for inactive tab hint in tab-bar."
  :group 'smypf-tab-bar)

(defun smypf-tab-bar-name-format (tab i)
    (let* ((current-p (eq (car tab) 'current-tab))
          (tab-face (if current-p
                         'smypf-tab-bar-tab
                       'smypf-tab-bar-tab-inactive))
           (hint-face (if current-p
                          'smypf-tab-bar-tab-hint
                        'smypf-tab-bar-tab-hint-inactive)))
      (concat (propertize (if tab-bar-tab-hints (format "  %d: " i) "  ")
                          'face hint-face)
              (propertize
               (concat
                (alist-get 'name tab)
                (or (and tab-bar-close-button-show
                         (not (eq tab-bar-close-button-show
                                  (if current-p 'non-selected 'selected)))
                         tab-bar-close-button)
                    "")
                "  ")
               'face tab-face))))



(use-package emacs
  :ensure nil
  :init
  (when (< 26 emacs-major-version)
    (tab-bar-mode 1))                           ;; enable tab bar

  :config
  (when (< 26 emacs-major-version)
    (setq tab-bar-show 1)                      ;; hide bar if <= 1 tabs open
    (setq tab-bar-close-button-show nil)       ;; hide tab close / X button
    (setq tab-bar-new-tab-choice "*scratch*");; buffer to show in new tabs
    (setq tab-bar-tab-hints t)                 ;; show tab numbers
    (setq tab-bar-select-tab-modifiers "super")
    (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
    (setq tab-bar-tab-name-format-function 'smypf-tab-bar-name-format))

  :bind
  ((
    ("s-{" . 'tab-bar-switch-to-prev-tab)
    ("s-}" . 'tab-bar-switch-to-next-tab)
    ("C-S-<tab>" . 'tab-bar-switch-to-prev-tab)
    ("C-<tab>" . 'tab-bar-switch-to-next-tab)
    ("s-t" . 'tab-bar-new-tab)
    ("s-1" . (lambda() (interactive) (tab-bar-select-tab 1)))
    ("s-2" . (lambda() (interactive) (tab-bar-select-tab 2)))
    ("s-3" . (lambda() (interactive) (tab-bar-select-tab 3)))
    ("s-4" . (lambda() (interactive) (tab-bar-select-tab 4)))
    ("s-5" . (lambda() (interactive) (tab-bar-select-tab 5)))
    ("s-6" . (lambda() (interactive) (tab-bar-select-tab 6)))
    ("s-w" . 'tab-bar-close-tab))))
  ;; elements to include in bar

;;; Package:
(provide 'my-tab-bar)
;;; my-tab-bar.el ends here
