;; Prevent pauses due to garbage collection by increasing the threshold
(setq gc-cons-threshold (* 50 1000 1000))


(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . light)) ;; assuming you are using a dark theme
(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)
(tool-bar-mode -1)

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
