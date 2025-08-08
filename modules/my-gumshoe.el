;;;; my-gumshoe.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;;

;;; Code:

(defun smypf/gumshoe-line-not-empty-or-punctuation-p ()
  "Return t if current line contains more than just whitespace, );} characters."
  (let ((line (thing-at-point 'line t)))
    (and line
         (not (string-match-p "^[[:space:]);}\n]*$" line)))))

(defun smypf/setup-gumshoe ()
  "Customise Gumshoe after the `after-init` hook is run"
  (interactive)
  (add-to-list 'gumshoe-ignore-predicates 'smypf/gumshoe-line-not-empty-or-punctuation-p)
  (add-to-list 'gumshoe-ignore-predicates (lambda () (not (xah-user-buffer-q))))
  (smypf/gumshoe-window))

(defun smypf/reset-gumshoe ()
  " Enable and disable Gumshoe to reset the ring"
  (interactive)
  (global-gumshoe-mode)
  (global-gumshoe-mode))

;;; smypf-gumshoe-window-config.el --- Custom window management for gumshoe backtracking -*- lexical-binding: t; -*-

;; This function provides custom window management for gumshoe backtracking.
;; It overrides the gumshoe--jump method to implement a configurable window layout
;; where new buffers are displayed in new windows to the left (forward)
;; or right (backward) of the current window.

(defun smypf/gumshoe-window()
  ;; This function provides custom window management for gumshoe backtracking.
  ;; It overrides the gumshoe--jump method to implement a configurable window layout
  ;; where new buffers are displayed in new windows to the left (forward)
  ;; or right (backward) of the current window.

  (defgroup smypf/gumshoe nil
    "Custom window management for gumshoe backtracking."
    :group 'gumshoe
    :prefix "smypf/gumshoe-")

  (defcustom smypf/gumshoe-max-windows 2
    "Maximum number of vertical windows to maintain during gumshoe backtracking."
    :type 'integer
    :group 'smypf/gumshoe)

  ;; Customize the gumshoe--jump method to implement custom window management
  (cl-defmethod gumshoe--jump ((self gumshoe--entry))
    "Jump Point to buffer and position in SELF with custom window management.

This overrides the default gumshoe--jump method to implement a configurable window layout:
- Maximum of smypf/gumshoe-max-windows vertical windows
- Forward navigation: new window to the left
- Backward navigation: new window to the right
- Same buffer navigation: no window changes"
    (let* ((position (overlay-start (oref self footprint-overlay)))
           (target-buffer (oref self buffer))
           (current-buffer (current-buffer))
           (current-window (selected-window))
           (is-same-buffer (gumshoe--in-current-buffer-p self))
           (is-forward (smypf/gumshoe--is-forward-navigation-p))
           (windows (window-list nil nil nil))
           (window-count (length windows)))

      ;; If navigating within the same buffer, just move point
      (if is-same-buffer
          (progn
            (pop-to-buffer-same-window target-buffer)
            (goto-char position))

        ;; For different buffers, implement custom window management
        (let ((new-window nil)
              (target-window nil))
          ;; If we have reached the maximum windows, delete the oldest one (farthest from current)
          (when (>= window-count smypf/gumshoe-max-windows)
            (smypf/gumshoe--delete-oldest-window current-window windows))

          ;; Create new window
          (setq new-window (split-window-horizontally))

          ;; Determine which window to use based on direction
          (setq target-window
                (if is-forward
                    ;; For forward navigation, use the left window (new window)
                    new-window
                  ;; For backward navigation, use the right window (original window)
                  current-window))

          ;; Display the new buffer in the target window
          (set-window-buffer target-window target-buffer)
          (select-window target-window)
          (goto-char position)))))

  ;; Helper function to determine if this is a forward navigation
  (defun smypf/gumshoe--is-forward-navigation-p ()
    "Return t if the current navigation is forward (backward in time)."
    ;; This is a heuristic - you might need to adjust based on your needs
    ;; One approach is to check if we're in backtracking mode and track direction
    (and global-gumshoe-backtracking-mode
         (eq this-command 'global-gumshoe-backtracking-mode-forward)))

  ;; Helper function to delete the oldest window when we exceed the limit
  (defun smypf/gumshoe--delete-oldest-window (current-window windows)
    "Delete the window farthest from CURRENT-WINDOW from WINDOWS list.
This helps maintain the smypf/gumshoe-max-windows limit by removing the oldest window."
    (let* ((current-edges (window-edges current-window))
           (current-left (car current-edges))
           (farthest-window nil)
           (max-distance 0))

      ;; Find the window farthest from the current window
      (dolist (window windows)
        (unless (eq window current-window)
          (let* ((window-edges (window-edges window))
                 (window-left (car window-edges))
                 (distance (abs (- window-left current-left))))
            (when (> distance max-distance)
              (setq max-distance distance)
              (setq farthest-window window)))))

      ;; Delete the farthest window
      (when farthest-window
        (delete-window farthest-window))))

  ;; Alternative approach using display-buffer actions
  ;; This is a more "gumshoe-native" way to customize buffer display
  (defun smypf/gumshoe--custom-display-buffer-action (buffer alist)
    "Custom display-buffer action for gumshoe backtracking.
This function can be used in gumshoe-display-buffer-action."
    (let* ((current-window (selected-window))
           (is-forward (smypf/gumshoe--is-forward-navigation-p))
           (windows (window-list nil nil nil))
           (window-count (length windows))
           (new-window nil))

      ;; If we have reached the maximum windows, delete the oldest one
      (when (>= window-count smypf/gumshoe-max-windows)
        (smypf/gumshoe--delete-oldest-window current-window windows))

      ;; Create new window
      (setq new-window (split-window-horizontally))

      ;; Return the appropriate window based on direction
      (if is-forward
          new-window  ;; Left window for forward navigation
        current-window))  ;; Right window for backward navigation

    ;; Example configuration to use the custom display action
    ;; Uncomment the line below to use this approach instead of overriding gumshoe--jump
    ;; (setq gumshoe-display-buffer-action '((smypf/gumshoe--custom-display-buffer-action)))
    )
  )

(use-package gumshoe
  :commands (global-gumshoe-mode)
  :custom
  (gumshoe-horizontal-scale 4)
  (gumshoe-cover-old-footprints-p nil)
  (gumshoe-show-footprints-p t)
  ;; Enabing global-gumshoe-backtracking-mode will initiate tracking
  ;; customize peruse slot display if you like
  ;;:custom
  ;;(gumshoe-follow-distance 1000000000)
  ;; (gumshoe-slot-schema '(time buffer position line))
  :bind (
         ("C-c n" . gumshoe-backtrack)
         ;; ("C-M-;" . gumshoe-buf-backtrack)
         ;; ("C-M-/" .  gumshoe-peruse-globally)

         ("C-M-," . gumshoe-backtrack)
         ("C-M-;" . gumshoe-buf-backtrack)
         ("C-M-/" .  gumshoe-peruse-globally)

         :map global-gumshoe-backtracking-mode-map

         ("C-M-;" .  gumshoe-buf-backtrack-back)
         ("C-M-'" . gumshoe-buf-backtrack-forward)

         ("C-M-." .  gumshoe-win-backtrack-forward)
         ("C-M-," . gumshoe-win-backtrack-back)
         ("C-M-r" . smypf/reset-gumshoe)
         ))

(add-hook 'after-init-hook
          (lambda ()
            (global-gumshoe-mode)
            (smypf/setup-gumshoe)))



;;; Package:
(provide 'my-gumshoe)
;;; my-gumshoe.el ends here
