;; Prevent pauses due to garbage collection by increasing the threshold
(setq gc-cons-threshold (* 50 1000 1000))

;; Uncomment these lines to determine startup time
;;   (add-hook 'emacs-startup-hook
;; 	    (lambda ()
;; 	      (message "Emacs ready in %s with %d garbage collections."
;; 		       (format "%.2f seconds"
;; 			       (float-time
;; 				(time-subtract after-init-time before-init-time)))
;; 		       gcs-done)))

