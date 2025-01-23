;;;; my-search.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;;

;;; Code:

(use-package  deadgrep
  :ensure t
  :defer t
  :bind (("C-c /" . deadgrep)))


;;; Package:
(provide 'my-search)
;;; my-something.el ends here
