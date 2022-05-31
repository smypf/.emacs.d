(setq gc-cons-threshold (* 50 1000 1000))

;; Uncomment these lines to determine startup time
;;   (add-hook 'emacs-startup-hook
;; 	    (lambda ()
;; 	      (message "Emacs ready in %s with %d garbage collections."
;; 		       (format "%.2f seconds"
;; 			       (float-time
;; 				(time-subtract after-init-time before-init-time)))
;; 		       gcs-done)))

(menu-bar-mode -1)
(setq visible-bell 1)

(unless (package-installed-p 'no-littering)
  (require 'no-littering))

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

;; Setting this significantly increases the statup time as packages are refreshed from melpa
;; See C-h f package-refresh-contents
;; (package-refresh-contents)
(setq package-enable-at-startup nil)

;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path (expand-file-name "elpa/" user-emacs-directory))
  (unless (package-installed-p 'use-package)
    (require 'use-package)))
(setq use-package-always-ensure t)

(use-package evil
  :init

  (setq evil-want-integration t
	evil-want-keybinding nil
	evil-vsplit-window-right t
	evil-split-window-below nil
	evil-undo-system 'undo-redo)

  ; Setting `split-height-threshold` to nil and `split-width-threshold` to 1 forces vertical splits
  ; This was specifically done for ensuring that magit panes are not opened in horizontal splits
  ; These values are copied from the Doom Emacs repository
  ; https://github.com/doomemacs/doomemacs/blob/61a7c541655038615e3f846a87db2e7d5883d35a/core/core-ui.el#L290
  (setq split-height-threshold nil
	split-width-threshold 160)
  (evil-mode))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package general
  :init
  (general-evil-setup t))

(defconst leader "SPC")

(setq scroll-conservatively 101) ; Prevent the cursor jumping to the middle of the page when scrolling to the bottom of the screen
					; https://stackoverflow.com/a/25777730
(setq scroll-margin 5)

; For some reason this doesn't work
; (evil-ex-define-cmd "\"w" 'evil-write)
(evil-ex-define-cmd "W" 'evil-write)

(setq lazy-highlight-cleanup nil
      lazy-highlight-max-at-a-time nil
      lazy-highlight-initial-delay 0
      isearch-allow-scroll t)

(unless (package-installed-p 'eink-theme)
  (package-install-file (expand-file-name "eink-theme.el" user-emacs-directory)))

(load-theme 'eink t)

;; Add line numbers globally
(global-display-line-numbers-mode)

;;
(global-hl-line-mode)

(use-package doom-modeline
  :init (doom-modeline-mode)
  :config
  (setq doom-modeline-major-mode-icon nil
	doom-modeline-vcs-max-length 40
	doom-modeline-workspace-name nil
	doom-modeline-buffer-encoding nil
	doom-modeline-persp-name nil
	doom-modeline-persp-icon t
	doom-modeline-buffer-file-name-style 'relative-to-project))

(use-package magit
  :after evil
  :defer t
  :init (defvar evil-collection-magit-use-$-for-end-of-line nil)
  :config
  (setq git-commit-summary-max-length 120)

  ;; Open in other window instead of the current window
  ;; TODO - Change this to use use-package's :bind
  (define-key magit-hunk-section-map (kbd "RET") 'magit-diff-visit-file-other-window)
  (define-key magit-file-section-map (kbd "RET") 'magit-diff-visit-file-other-window)

  ;; Add goto-address-mode to magit status window and process window
  (add-hook 'magit-process-mode-hook 'goto-address-mode)
  (add-hook 'magit-status-sections-hook 'goto-address-mode)

  ;; https://magit.vc/manual/magit/Automatic-Refreshing-of-Magit-Buffers.html
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))


(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix leader
 "g" 'magit)

;; Kill the magit buffer and close the pane
(general-define-key
 :state 'normal
 :keymaps 'magit-mode-map
 "q" 'kill-buffer-and-window)

;; https://emacs.stackexchange.com/a/14012
(add-hook 'with-editor-mode-hook 'evil-insert-state)

(use-package which-key
  :defer t
  :config
  (which-key-mode))

(general-define-key
 :states 'normal
 "-" 'dired-jump)

(evil-define-key 'normal dired-mode-map "q" 'kill-buffer-and-window)

;; Prevent new buffers from being created when navigating directories
(setq dired-kill-when-opening-new-dired-buffer t)

;; Hide the "." and ".." directories
;; https://stackoverflow.com/a/43632653
(add-hook 'dired-mode-hook 'dired-omit-mode)

;; use gls to ensure that folders are sorted at the top
(setq insert-directory-program "gls" dired-use-ls-dired t
      dired-listing-switches "-alGh --group-directories-first"
      dired-omit-files
      (rx (or (seq bol (? ".") "#")
	      (seq bol "." eol)
	      (seq bol ".." eol))))

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix leader
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
  :defer t
  :init
  (vertico-mode)
  (setq vertico-count 15))

(general-nmap
  "  " 'vertico-find)

(use-package consult
  :defer t
  :init
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)
  :config
  (autoload 'projectile-project-root "projectile"))

(use-package orderless
  :defer t
  :custom
  (completion-styles '(orderless flex))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(setq-default fill-column 120)

(use-package projectile
  :defer t)

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix leader
 "SPC" 'projectile-find-file
 "pp" 'projectile-switch-project)

(setq projectile-project-search-path '("~/projects/"))
(projectile-discover-projects-in-search-path)

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix leader
 ":" 'execute-extended-command)

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix leader
 "bp" 'evil-prev-buffer
 "bn" 'evil-next-buffer
 "bb" 'switch-to-buffer)

(defalias 'yes-or-no-p 'y-or-n-p)

(defun search-thing-at-point ()
  (interactive)
  (consult-ripgrep (projectile-project-root) (thing-at-point 'symbol)))


(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix leader
 "?" 'consult-ripgrep
 "/" 'search-thing-at-point
 "s" 'consult-line)

(use-package tide
:defer t
  :ensure t
  :hook ((typescript-mode . tide-setup)
	 (typescript-mode . tide-hl-identifier-mode)))

(use-package vterm
  :after evil
  :defer t
  :ensure t
  :config
  (evil-set-initial-state 'vterm-mode 'insert))

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix leader
 "'" 'vterm)

(global-subword-mode)

(defun find-references-at-point ()
  (interactive)
  (xref-find-references (thing-at-point 'symbol)))


(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix leader
 "cd" 'xref-find-definitions
 "cD" 'find-references-at-point)

;; Copy Pasting
(use-package xclip
  :ensure t
  :init (xclip-mode)
  :config
  (setq select-enable-clipboard nil))

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
   '("6f1e32040ff938f2dcd758702db99a6b2fddfbd9128d732fe23c6dccd82d8567" default))
 '(package-selected-packages
   '(xclip doom-modeline no-littering eglot general consult vertico magit evil-collection evil use-package vterm)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
