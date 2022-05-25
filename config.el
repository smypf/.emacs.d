(menu-bar-mode -1)
(setq visible-bell 1)

;;   (require 'package)
;;   (add-to-list 'package-archives
;; 	       '("melpa" . "https://melpa.org/packages/"))

  ;; Setting these significantly increases the statup time as packages are refreshed from melpa
  ;; (package-refresh-contents)
  ;; (package-initialize)
  ;; (setq package-enable-at-startup nil)

;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "<path where use-package is installed>")
  (unless (package-installed-p 'use-package)
    (require 'use-package)))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (evil-mode))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package general
  :ensure t
  :init
  (general-evil-setup t))

  (defconst leader " ")

(package-install-file "~/.emacs.d/eink-theme.el")

(load-theme 'eink t)



(use-package magit
  :ensure t)

(general-nmap
 :prefix leader
 "g" 'magit)

(use-package which-key
  :ensure t)
(which-key-mode)

(general-nmap
  "-" 'dired-jump)

; (evil-define-key 'normal 'global "-" 'dired-jump)

(general-define-key
:states 'normal
:keymaps 'override
:prefix "SPC"
"f" 'find-file)
 (general-define-key
:states 'normal
:keymaps 'override
    :prefix "SPC"
    "w" 'evil-window-map)

  (define-key evil-window-map (kbd "<right>") 'evil-window-right)
  (define-key evil-window-map (kbd "<left>") 'evil-window-left)
  (define-key evil-window-map (kbd "<up>") 'evil-window-up)
  (define-key evil-window-map (kbd "<down>") 'evil-window-down)

  (define-key evil-window-map (kbd "S-<right>") 'evil-window-move-far-right)
  (define-key evil-window-map (kbd "S-<left>") 'evil-window-move-far-left)
  (define-key evil-window-map (kbd "S-<up>") 'evil-window-move-very-top)
  (define-key evil-window-map (kbd "S-<down>") 'evil-window-move-very-bottom)

(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-count 15))

(general-nmap
  "  " 'vertico-find)

(use-package consult
  :init)

(use-package orderless
  :init
					; (setq completion-styles '(orderless-basic)
					; 	     completion-category-defaults nil
					; 	     completion-category-overrides '((files (styles partial-completion))))
  )
