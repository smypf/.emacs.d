;;;; my-popper.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;;

;;; Code:


(defvar smypf/default-alignment 'right
  "Default alignment for popup windows.")

(defvar smypf/default-width 0.5
  "Default width for popup windows as a percentage (0.0-1.0).")

(defun smypf/setup-display-rules ()
  "Setup display rules for various buffers."

  ;; Help buffers
  (setf (alist-get "*Help*"
				   display-buffer-alist
				   nil nil #'string=)
		`((display-buffer-reuse-window
		   display-buffer-in-side-window)
		  (side . ,smypf/default-alignment)
		  (window-width . ,smypf/default-width)))

  ;; Warnings
  (setf (alist-get "*Warnings*"
				   display-buffer-alist
				   nil nil #'string=)
		'((display-buffer-reuse-window
		   display-buffer-at-bottom)
		  (window-height . 0.2)))

  ;; Howm view summary
  (setf (alist-get ".*howm-view-summary-mode.*"
				   display-buffer-alist
				   nil nil #'string=)
		`((display-buffer-reuse-window
		   display-buffer-in-side-window)
		  (side . ,smypf/default-alignment)
		  (window-width . ,smypf/default-width)))

  ;; HTTP Response
  (setf (alist-get "*HTTP Response*"
				   display-buffer-alist
				   nil nil #'string=)
		`((display-buffer-reuse-window
		   display-buffer-in-side-window)
		  (side . ,smypf/default-alignment)
		  (window-width . ,smypf/default-width)))

  ;; Eldoc
  (setf (alist-get "*eldoc*"
				   display-buffer-alist
				   nil nil #'string=)
		'((display-buffer-reuse-window
		   display-buffer-at-bottom)
		  (window-height . 0.2)))

  ;; Helpful mode
  (setf (alist-get ".*helpful mode.*"
				   display-buffer-alist
				   nil nil #'string=)
		`((display-buffer-reuse-window
		   display-buffer-in-side-window)
		  (side . ,smypf/default-alignment)
		  (window-width . ,smypf/default-width)))

  ;; Info
  (setf (alist-get "*info*"
				   display-buffer-alist
				   nil nil #'string=)
		`((display-buffer-reuse-window
		   display-buffer-in-side-window)
		  (side . ,smypf/default-alignment)
		  (window-width . ,smypf/default-width)))

  ;; RG mode
  (setf (alist-get ".*rg-mode.*"
				   display-buffer-alist
				   nil nil #'string=)
		`((display-buffer-reuse-window
		   display-buffer-in-side-window)
		  (side . ,smypf/default-alignment)
		  (window-width . ,smypf/default-width)))

  ;; Messages
  (setf (alist-get "*Messages*"
				   display-buffer-alist
				   nil nil #'string=)
		`((display-buffer-reuse-window
		   display-buffer-in-side-window)
		  (side . ,smypf/default-alignment)
		  (window-width . ,smypf/default-width)))

  ;; Compilation
  (setf (alist-get ".*compilation-mode.*"
				   display-buffer-alist
				   nil nil #'string=)
		`((display-buffer-reuse-window
		   display-buffer-in-side-window)
		  (side . ,smypf/default-alignment)
		  (window-width . ,smypf/default-width)))
  
  (setf (alist-get "*compilation*"
				   display-buffer-alist
				   nil nil #'string=)
		`((display-buffer-reuse-window
		   display-buffer-in-side-window)
		  (side . ,smypf/default-alignment)
		  (window-width . ,smypf/default-width))))

;; Popper configuration with the new display rules
(use-package popper
  :defer t
  :ensure t
  :bind (("C-c w p p" . popper-toggle)
		 ("C-c w p n" . popper-cycle))
  :init
  (setq popper-reference-buffers
		'("\\*Messages\\*"
		  "\\*Warnings\\*"
		  "\\*HTTP Response\\*"
		  "Output\\*$"
		  "\\*Async Shell Command\\*"
		  help-mode
		  helpful-mode
		  flycheck-error-list-mode
		  rg-mode
		  devdocs-mode
		  "\\*eldoc\\*"
		  "\\*eldoc for .*?\\*"
		  compilation-mode))

  ;; Enable popper modes
  (popper-mode +1)
  (popper-echo-mode +1)

  ;; Enable display control for custom rules
  (setq popper-display-control t)

  ;; Initialize display rules
  (smypf/setup-display-rules)

  :config
  (defvar popper-repeat-map
	(let ((map (make-sparse-keymap)))
	  (define-key map "p" 'popper-toggle)
	  (define-key map "n" 'popper-cycle)
	  map))

  (put 'popper-toggle 'repeat-map 'popper-repeat-map)
  (put 'popper-cycle 'repeat-map 'popper-repeat-map))


;;; Package:
(provide 'my-popper)
;;; my-popper.el ends here
