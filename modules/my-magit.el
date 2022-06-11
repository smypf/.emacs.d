;;;; my-magit.el --- Magit is essential -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;; Source Control is essential. Magit makes it easier.

;;; Code:

;; Magit is a wrapper around git which is nice to use.
(use-package magit
  :after evil
  :defer t
  :init (defvar evil-collection-magit-use-$-for-end-of-line nil)
  :config
  ;; Set the max length of the commit message before wrapping to the next line
  (setq git-commit-summary-max-length 120)

  ;; Open in other window instead of the current window
  ;; TODO - Change this to use use-package's :bind
  (define-key magit-hunk-section-map (kbd "RET") 'magit-diff-visit-file-other-window)
  (define-key magit-file-section-map (kbd "RET") 'magit-diff-visit-file-other-window)

  ;; Add goto-address-mode to magit status window and process window
  (add-hook 'magit-process-mode-hook 'goto-address-mode)
  (add-hook 'magit-status-sections-hook 'goto-address-mode)

  ;; https://magit.vc/manual/magit/Automatic-Refreshing-of-Magit-Buffers.html
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix leader
 "g" 'magit)

;; Kill the magit buffer and close the pane
(general-define-key
 :state 'normal
 :keymaps 'magit-mode-map
 "q" 'kill-buffer-and-window)

;; Automatically start in insert state when openning the commit buffer
;; https://emacs.stackexchange.com/a/14012
(add-hook 'with-editor-mode-hook 'evil-insert-state)

;; TODO Should I also install git-timemachine?
;; https://github.com/emacsmirror/git-timemachine
;; (use-package git-timemachine
;; :after magit)


;;; Package:
(provide 'my-magit)
;;; my-magit.el ends here
