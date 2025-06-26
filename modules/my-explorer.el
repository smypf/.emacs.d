;;;; my-explorer.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;;

							
;;; Code:

;; - When the buffer is the scratch buffer the heading should be a top level heading
;; - When the buffer does not change file the heading level should be a child heading under the heading with the file name
;; - When the previous buffer was a file  and the new buffer is a file the heading should a child heading
;; - When the buffer is the same file the heading should be the 

(defun smypf/explorer/generate-string ()
  "Generate a unique string based on the current visible tab."
  (interactive)
  ;; Generate a unique string using the tab name and a timestamp
  (let ((unique-string (format "%s" (format-time-string "%Y%m%d") )))
	(message "Unique string for tab: %s" unique-string)
	unique-string))

(defun smypf/explorer/get-file-name ()
  "Create the file path for embarking on a new exploration."
  (interactive)
  (format "%s.org" (smypf/explorer/generate-string)))

(defun smypf/explorer/get-file-path ()
  "Create the file path for embarking on a new exploration."
  (interactive)
  (format "~/org/explorer/%s.org" (smypf/explorer/generate-string)))


(defun smypf/explorer/create-explorer-file (directory file-name)
  "Create a file with the given FILE-NAME in the DIRECTORY."
  (interactive)
  (let* ((filepath (expand-file-name file-name directory)))
	;; Ensure the directory exists
	(unless (file-exists-p directory)
	  (make-directory directory t))
	;; Create the file if it doesn't already exist
	(if (file-exists-p filepath)
		(message "File '%s' already exists." filepath)
	  (write-region "" nil filepath)
	  (message "File '%s' created." filepath))))

(setq org-capture-templates
      '(("d" "Dynamic Link" entry
         (file+function "~/org/notes.org" my-select-heading)
         "* %? \nCaptured on: %U\nSource: %a\n\n")))
  )

(defun smypf/explorer/add-new-entry ()

;;; Package:
(provide 'my-explorer)
;;; my-explorer.el ends here

