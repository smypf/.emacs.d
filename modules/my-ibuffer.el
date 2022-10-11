;;;; my-ibuffer.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;; ibuffer is an alternative to list-buffers

;;; Code:

(define-key global-map (kbd "C-x C-b") #'ibuffer)

(setq ibuffer-display-summary nil)
(setq ibuffer-use-other-window nil)

;;; Package:
(provide 'my-ibuffer)
;;; my-ibuffer.el ends here
