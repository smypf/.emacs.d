;;;; my-modes.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;; Here files have major modes assigned to them if necessary

;;; Code:
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . html-mode))

;;; Package:
(provide 'my-modes)
;;; my-modes.el ends here
