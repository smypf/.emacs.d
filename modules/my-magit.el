;;;; my-magit.el --- Magit is essential -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;; Source Control is essential. Magit makes it easier.

;;; Code:

;; Magit is a wrapper around git which is nice to use.
(use-package magit
  :defer t
  :config
  ;; Set the max length of the commit message before wrapping to the next line
  (setq git-commit-summary-max-length 120)

  ;; Open in other window instead of the current window
  ;; This is using `define-key` rather than general.el since as this would require overriding the key first
  ;; https://github.com/noctuid/evil-guide#prevent-text-property-maps-from-overriding-evil
  (define-key magit-hunk-section-map (kbd "RET") 'magit-diff-visit-file-other-window)
  (define-key magit-file-section-map (kbd "RET") 'magit-diff-visit-file-other-window)

  ;; Add goto-address-mode to magit status window and process window
  (add-hook 'magit-process-mode-hook 'goto-address-mode)
  (add-hook 'magit-status-sections-hook 'goto-address-mode)

  ;; From https://emacs.stackexchange.com/a/44685
  (defun insert-issue-key()
    (interactive)
    ;; TODO also include the commit type by testing for a string proceeded by a '/'
    ;; e.g. feat/, fix/, docs/ etc...
    ;; Set a regex for identifying an issue
    (let ((ISSUEKEYREGEX "[[:upper:]]+-[[:digit:]]+"))
      ;; Save the Issue Key as a variable from the current branch
      (let ((ISSUEKEY (replace-regexp-in-string
		       (concat ".*?\\(" ISSUEKEYREGEX "\\).*")
		       "\\1"
		       (magit-get-current-branch)))
	    ;; Find where the first instance of the "#" character is, which designates the start of the comments in the commit message
	    (COMMITMESSAGEEND (search-forward "#")))

	;; When the current branch has an issue key in it
	(when (string-match-p ISSUEKEYREGEX (magit-get-current-branch))
	  ;; Unless the buffer contains the current Issue Key
	  (unless (string-match ISSUEKEY (buffer-substring-no-properties 1 COMMITMESSAGEEND))
	    ;; Go back to the start of the buffer since search-forward moves the cursor
        (goto-char (point-min))
	    ;; Append the Issue Key to the buffer
	    (insert (concat "\n\nref: " ISSUEKEY)))

	  ;; Go back to the start of the buffer
      (goto-char (point-min))
      ;; TODO fix this so that it isn't dependent on the something
      (if (fboundp 'meow-insert) (meow-insert))))) )
      ;;(cond (fboundp 'meow-insert meow-insert)
            ;;(fboundp 'evil-insert-line (evil-insert-line 1)))))))

  (add-hook 'git-commit-setup-hook 'insert-issue-key)

  ;; https://magit.vc/manual/magit/Automatic-Refreshing-of-Magit-Buffers.html
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))

;; Automatically start in insert state when openning the commit buffer
;; https://emacs.stackexchange.com/a/14012
;; (add-hook 'with-editor-mode-hook 'evil-insert-state)

;; TODO Should I also install git-timemachine?
;; https://github.com/emacsmirror/git-timemachine
;; (use-package git-timemachine
;; :after magit)

;; Magit Delta
;; This may have problems with git-delta v. 0.13.0
;; This has been disabled as it may be causing issues with staging changes
;; (use-package magit-delta
;;   :after magit
;;   :config
;;   (setq magit-delta-delta-args '(
;; 				 "--syntax-theme" "none"))
;; 				 ;;"--side-by-side" "true"))
;;   :hook (magit-mode . magit-delta-mode))

;; TODO Override (magit-visit-thing) to work with (goto-address-mode)

;; Override inbuilt magit functionality to tell you off when inserting a space
;; https://stackoverflow.com/a/15725437
(eval-after-load "magit"
  '(defun magit-whitespace-disallowed ()
     (interactive)
     (insert "-")))

;;; Package:
(provide 'my-magit)
;;; my-magit.el ends here
