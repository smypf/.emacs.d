;; Compile Angel
;; Ensure Emacs loads the most recent byte-compiled files.
(setq load-prefer-newer t)

;; Ensure JIT compilation is enabled for improved performance by
;; native-compiling loaded .elc files asynchronously
(setq native-comp-jit-compilation t)
(setq native-comp-deferred-compilation t) ; Deprecated in Emacs > 29.1




;; Prevent pauses due to garbage collection by increasing the threshold
(setq gc-cons-threshold (* 50 1000 1000))


(add-to-list 'default-frame-alist '(ns-transparent-titlebar . nil)) ;; Ensure that the title bar is shown
(add-to-list 'default-frame-alist '(ns-appearance . light)) ;; Make the title bar match the appearance
(add-to-list 'initial-frame-alist '(fullscreen . maximized)) ;; Start emacs maximised
(setq ns-use-proxy-icon nil) ;; Hide the file icon in the title bar
(tool-bar-mode -1) ;; Hide the icons in the title bar
(select-frame-set-input-focus (selected-frame))
;; (unless window-system
;;  (add-to-list 'default-frame-alist '(undecorated . t)))

;; Uncomment these lines to determine startup time
;;   (add-hook 'emacs-startup-hook
;;      (lambda ()
;;        (message "Emacs ready in %s with %d garbage collections."
;;             (format "%.2f seconds"
;;                 (float-time
;;              (time-subtract after-init-time before-init-time)))
;;             gcs-done)))
