;;;; my-compile.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;;

;;; Code:

;; https://github.com/ReanGD/emacs-multi-compile

;; TODO replace items with a variable read from .dir-locals.el
(defun smypf-choose-compile-command ()
  "Prompt the user to select multiple items from a list and call `compile`."
  (interactive)
  (let ((items '("npm test" "npm run build" "npm test -- -u")))
    (compile (car (last (completing-read-multiple "Select compile job: " items))))))

;; This is an example of how we can bind keys outside of the my-meow module
(use-package emacs
  :after meow
  :init
  (meow-leader-define-key
   '("T" . smypf-choose-compile-command)))

;;; Package:
(provide 'my-compile)
;;; my-compile.el ends here
