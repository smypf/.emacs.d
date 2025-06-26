;;;; my-search.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;;

;;; Code:

;; TODO: create function which returns `switch-to-buffer-other-window` when not on the scratch buffer. otherwise use the current window and set deadgrep-display-buffer-function to use this function
(use-package  deadgrep
  :ensure t
  :defer t
  :bind (("C-c /" . deadgrep)))


;;; Package:
(provide 'my-search)
;;; my-something.el ends here
