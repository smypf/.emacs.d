;;;; my-system.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;;

;;; Code:
(defun setup-osx()
  "Setting up various functionalities for OSX including moving items to trash and Cut/Copy/Paste functionality"

  (setq trash-directory "~/.Trash")

  ;; Ensure that the window is not forced to be bound to the width and height of a character
  (setq frame-resize-pixelwise t)

  ;; See `trash-directory' as it requires defining `system-move-file-to-trash'.
  (defun system-move-file-to-trash (file)
    "Use \"trash\" to move FILE to the system trash."
    (cl-assert (executable-find "trash") nil "'trash' must be installed. Needs \"brew install trash\"")
    (call-process "trash" nil 0 nil "-F"  file))
  (defun isolate-kill-ring()
    "Isolate Emacs kill ring from OS X system pasteboard.
This function is only necessary in window system."
    (interactive)
    (setq interprogram-cut-function nil)
    (setq interprogram-paste-function nil))


  ;; https://gist.github.com/zmwangx/faf7c442ce3f43d85ba2
  (defun pasteboard-copy()
    "Copy region to OS X system pasteboard."
    (interactive)
    (shell-command-on-region
     (region-beginning) (region-end) "pbcopy"))

  (defun pasteboard-paste()
    "Paste from OS X system pasteboard via `pbpaste' to point."
    (interactive)
    (shell-command-on-region
     (point) (if mark-active (mark) (point)) "pbpaste" nil t))

  (defun pasteboard-cut()
    "Cut region and put on OS X system pasteboard."
    (interactive)
    (pasteboard-copy)
    (delete-region (region-beginning) (region-end)))

  (if window-system
      (progn
        (isolate-kill-ring)
        ;; bind CMD+C to pasteboard-copy
        (global-set-key (kbd "s-c") 'pasteboard-copy)
        ;; bind CMD+V to pasteboard-paste
        (global-set-key (kbd "s-v") 'pasteboard-paste)
        ;; bind CMD+X to pasteboard-cut
        (global-set-key (kbd "s-x") 'pasteboard-cut)))
  ;; you might also want to assign some keybindings for non-window
  ;; system usage (i.e., in your text terminal, where the
  ;; command->super does not work)
  )

(if (eq system-type 'darwin)
    (setup-osx))


;;; Package:
(provide 'my-system)
;;; my-system.el ends here
