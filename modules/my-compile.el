;;;; my-compile.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;;

;;; Code:
;; Automatically scroll compilation buffer output
(use-package compile
  :ensure nil
  :config
  (setq compilation-scroll-output t))

(defvar smypf-compile-items '("nvm use; npm test" "nvm use; npm run build" "nvm use; npm test -- -u"))

;; TODO replace items with a variable read from .dir-locals.el
(defun smypf-choose-compile-command ()
  "Prompt the user to select multiple items from a list and call `compile`."
  (interactive)
  (let* ((selected-items (completing-read-multiple "Select compile job: " smypf-compile-items))
         (new-items (cl-remove-if (lambda (item) (member item smypf-compile-items)) selected-items))
         (updated-list (append smypf-compile-items new-items)))
    (setq smypf-compile-items updated-list)
    (compile (car (last selected-items nil)))))

(global-set-key (kbd "C-c T") 'smypf-choose-compile-command)

;;; Package:
(provide 'my-compile)
;;; my-compile.el ends here
