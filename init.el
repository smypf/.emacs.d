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

;; General is used for setting up keybinds
;; Since this is used across all modules it is loaded here
(use-package general
  :init
  (general-evil-setup t)
  :config
  ;; Set the leader. This is used for keybindings
  (defconst leader "SPC"))

;; Add the modules folder to the load path
(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))

(require 'my-evil)
(require 'my-visual-customisations)
(require 'my-completion)
(require 'my-magit)
(require 'my-dired)
(require 'my-projectile)
(require 'my-node)
(require 'my-coding)
(require 'my-utility)
;(require my-benchmark)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

(load "server")
(unless (server-running-p) (server-start))

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
   '("f0393dbed2e12400391e2845e7443d92fbbc109a6a5b68549db416ffa9a7d26a" "43723b620f335ac047727a9dc13cb629b74a7c23349e9b5e0e6535dd662dadc4" "96ac3799e504479c862cce31b6882274fa4ad9490c57ccfab81c1bfb8c326795" "77ccee107184be05753c15ba11cae1f4f03012505969d46c4e3d76cac264e077" "3848c2c3e7a48d6dec6defbd5a90ea6f8c03c4aac835461ead0c2bef7651a174" "c6d63b27dea1738060614c48ce48cee42ee82ce27263dbd612a9230c86a4a8eb" "f98c6f84330f1f3490021c1f0ccb9f7e90797df0f2700fe3bd7fe8ad4dd67369" "680ba271ab61df49c4f8464b6f4d04b5bb2965691cec658bbd16bd8039faf69b" default))
 '(package-selected-packages
   '(company xclip doom-modeline no-littering eglot general consult vertico magit evil-collection evil use-package vterm)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
