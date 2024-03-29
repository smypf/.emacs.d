;;; package -- Sasha's Personal Configuration
;;; Commentary:
;;; Stuff gets loaded here.
;;; Most of the stuff is in module files.  These are located in the `/module` folder
;;; Code:

;; Set up melpa which is a package host
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; Setting this significantly increases the statup time as packages are refreshed from melpa
;; See C-h f package-refresh-contents
;; (package-refresh-contents)
;; I think that this is not necessary as use-package handles this
(setq package-enable-at-startup nil)

;; Set up use-package
;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path (expand-file-name "elpa/" user-emacs-directory))
  (unless (package-installed-p 'use-package)
    (require 'use-package)))
(setq use-package-always-ensure t)

;; Benchmark startup times
;; Comment out this to enable benchmarking of startup
;;(use-package benchmark-init
;;  :ensure t
;;  :config
;;  ;; To disable collection of benchmark data after init is done.
;;  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Add the modules folder to the load path
(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))

(require 'my-defaults)
(require 'my-compile)
;; (require 'my-evil)
(require 'my-meow)
(require 'my-visual-customisations)
(require 'my-completion)
(require 'my-magit)
(require 'my-dired)
(require 'my-ibuffer)
(require 'my-modes)
(require 'my-org)
(require 'my-projectile)
(require 'my-node)
(require 'my-coding)
(require 'my-utility)
(require 'my-system)
(require 'my-vterm)
(require 'my-tab-bar)
(require 'my-native-comp)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;; Load server if it isn't already running
(use-package server
  :ensure nil
  :config
  (unless (server-running-p) (server-start)))

;; Enable mouse and scrolling for terminal
;; https://stackoverflow.com/a/62266648
(unless (display-graphic-p)
  ;; activate mouse-based scrolling
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; For compiling files. This needs to be moved into a file which is only called sometimes.
;; See https://github.com/gilbertw1/emacs-literate-starter
;; (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
;; (byte-compile-file (expand-file-name "config.el" user-emacs-directory))
;; (byte-compile-file (expand-file-name "init.el" user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("cdab8585cf30d8ae482b4d5bb5dd2de3cd8d0a486b0b90da1958ed04c187363b" "021321ae56a45794f43b41de09fb2bfca184e196666b7d7ff59ea97ec2114559" "40d29cf577515779b178069a54d998a3da35a28b5d42246e4006dbdf7cf4b7a5" "f0393dbed2e12400391e2845e7443d92fbbc109a6a5b68549db416ffa9a7d26a" "43723b620f335ac047727a9dc13cb629b74a7c23349e9b5e0e6535dd662dadc4" "96ac3799e504479c862cce31b6882274fa4ad9490c57ccfab81c1bfb8c326795" "77ccee107184be05753c15ba11cae1f4f03012505969d46c4e3d76cac264e077" "3848c2c3e7a48d6dec6defbd5a90ea6f8c03c4aac835461ead0c2bef7651a174" "c6d63b27dea1738060614c48ce48cee42ee82ce27263dbd612a9230c86a4a8eb" "f98c6f84330f1f3490021c1f0ccb9f7e90797df0f2700fe3bd7fe8ad4dd67369" "680ba271ab61df49c4f8464b6f4d04b5bb2965691cec658bbd16bd8039faf69b" default))
 '(package-selected-packages
   '(benchmark-init dirvish rainbow-mode yasnippet flymake-cursor company xclip doom-modeline no-littering eglot general consult vertico magit use-package vterm))
 '(tab-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
