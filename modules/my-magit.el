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

  ;; https://mbork.pl/2022-11-19_Streamlining_my_workflow_with_Magit_and_BitBucket
  (defun magit-open-pull-request ()
    "Open the pull request URL if applicable."
    (interactive)
    (save-excursion
      (set-buffer (magit-process-buffer t))
      (goto-char (point-max))
      (magit-section-backward)
      (when
    (search-backward-regexp "remote: \\(To create a merge\\|Create pull\\) request" nil t)
    (forward-line 1)
    (re-search-forward "remote: +" (line-end-position) t)
    (browse-url-at-point))))

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
        (insert (concat "\n\nref: " ISSUEKEY))))

      ;; Go back to the start of the buffer
      (goto-char (point-min))
      ;; TODO fix this so that it isn't dependent on the something
      (if (fboundp 'meow-insert) (meow-insert)))))
      ;;(cond (fboundp 'meow-insert meow-insert)
            ;;(fboundp 'evil-insert-line (evil-insert-line 1)))))))

  (add-hook 'git-commit-setup-hook 'insert-issue-key)

  ;; https://magit.vc/manual/magit/Automatic-Refreshing-of-Magit-Buffers.html
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)

  (defun th/magit--with-difftastic (buffer command)
  "Run COMMAND with GIT_EXTERNAL_DIFF=difft then show result in BUFFER."
  (let ((process-environment
         (cons (concat "GIT_EXTERNAL_DIFF=difft --width="
                       (number-to-string (frame-width)))
               process-environment)))
    ;; Clear the result buffer (we might regenerate a diff, e.g., for
    ;; the current changes in our working directory).
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer))
    ;; Now spawn a process calling the git COMMAND.
    (make-process
     :name (buffer-name buffer)
     :buffer buffer
     :command command
     ;; Don't query for running processes when emacs is quit.
     :noquery t
     ;; Show the result buffer once the process has finished.
     :sentinel (lambda (proc event)
                 (when (eq (process-status proc) 'exit)
                   (with-current-buffer (process-buffer proc)
                     (goto-char (point-min))
                     (ansi-color-apply-on-region (point-min) (point-max))
                     (setq buffer-read-only t)
                     (view-mode)
                     (end-of-line)
                     ;; difftastic diffs are usually 2-column side-by-side,
                     ;; so ensure our window is wide enough.
                     (let ((width (current-column)))
                       (while (zerop (forward-line 1))
                         (end-of-line)
                         (setq width (max (current-column) width)))
                       ;; Add column size of fringes
                       (setq width (+ width
                                      (fringe-columns 'left)
                                      (fringe-columns 'right)))
                       (goto-char (point-min))
                       (pop-to-buffer
                        (current-buffer)
                        `(;; If the buffer is that wide that splitting the frame in
                          ;; two side-by-side windows would result in less than
                          ;; 80 columns left, ensure it's shown at the bottom.
                          ,(when (> 80 (- (frame-width) width))
                             #'display-buffer-at-bottom)
                          (window-width
                           . ,(min width (frame-width))))))))))))

  ;; copied from https://tsdh.org/posts/2022-08-01-difftastic-diffing-with-magit.html
  (defun th/magit-show-with-difftastic (rev)
    "Show the result of \"git show REV\" with GIT_EXTERNAL_DIFF=difft."
    (interactive
     (list (or
            ;; If REV is given, just use it.
            (when (boundp 'rev) rev)
            ;; If not invoked with prefix arg, try to guess the REV from
            ;; point's position.
            (and (not current-prefix-arg)
                 (or (magit-thing-at-point 'git-revision t)
                     (magit-branch-or-commit-at-point)))
            ;; Otherwise, query the user.
            (magit-read-branch-or-commit "Revision"))))
    (if (not rev)
        (error "No revision specified")
      (th/magit--with-difftastic
       (get-buffer-create (concat "*git show difftastic " rev "*"))
       (list "git" "--no-pager" "show" "--ext-diff" rev))))

  (defun th/magit-diff-with-difftastic (arg)
    "Show the result of \"git diff ARG\" with GIT_EXTERNAL_DIFF=difft."
    (interactive
     (list (or
            ;; If RANGE is given, just use it.
            (when (boundp 'range) range)
            ;; If prefix arg is given, query the user.
            (and current-prefix-arg
                 (magit-diff-read-range-or-commit "Range"))
            ;; Otherwise, auto-guess based on position of point, e.g., based on
            ;; if we are in the Staged or Unstaged section.
            (pcase (magit-diff--dwim)
              ('unmerged (error "unmerged is not yet implemented"))
              ('unstaged nil)
              ('staged "--cached")
              (`(stash . ,value) (error "stash is not yet implemented"))
              (`(commit . ,value) (format "%s^..%s" value value))
              ((and range (pred stringp)) range)
              (_ (magit-diff-read-range-or-commit "Range/Commit"))))))
    (let ((name (concat "*git diff difftastic"
                        (if arg (concat " " arg) "")
                        "*")))
      (th/magit--with-difftastic
       (get-buffer-create name)
       `("git" "--no-pager" "diff" "--ext-diff" ,@(when arg (list arg))))))

  (transient-define-prefix th/magit-aux-commands ()
    "My personal auxiliary magit commands."
    ["Auxiliary commands"
     ("d" "Difftastic Diff (dwim)" th/magit-diff-with-difftastic)
     ("s" "Difftastic Show" th/magit-show-with-difftastic)])

  (transient-append-suffix 'magit-dispatch "!"
    '("#" "My Magit Cmds" th/magit-aux-commands))

  (define-key magit-status-mode-map (kbd "#") #'th/magit-aux-commands))

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
;;               "--syntax-theme" "none"))
;;               ;;"--side-by-side" "true"))
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
