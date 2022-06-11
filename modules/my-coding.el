;;;; my-coding.el --- Making working with code easier -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;; 

;;; Code:

;; Searching for references

(defun find-references-at-point ()
  (interactive)
  (xref-find-references (thing-at-point 'symbol)))

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix leader
 "cd" 'xref-find-definitions
 "cD" 'find-references-at-point)

(use-package eglot
  :defer 3)


;;; Package:
(provide 'my-coding)
;;; my-coding.el ends here
