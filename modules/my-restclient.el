;;;; my-restclient.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;; RapidAPI is being difficult. Lets see if Emacs can fill this more appropriately

;;; Code:
(use-package restclient
  :defer t)

(use-package restclient-jq
  :defer t
  :after restclient)

(use-package restclient-test
  :defer t
  :after restclient)

;;; Package:
(provide 'my-restclient)
;;; my-restclient.el ends here
