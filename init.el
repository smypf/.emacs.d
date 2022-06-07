;; Increate the garbage collection threshold to prevent GC pauses
(setq gc-cons-threshold (* 50 1000 1000))

;; Uncomment these lines to determine startup time
;;   (add-hook 'emacs-startup-hook
;; 	    (lambda ()
;; 	      (message "Emacs ready in %s with %d garbage collections."
;; 		       (format "%.2f seconds"
;; 			       (float-time
;; 				(time-subtract after-init-time before-init-time)))
;; 		       gcs-done)))

;; Hide the menu bar
(menu-bar-mode -1)

;; Remove the audio bell when an error occurs
(setq visible-bell 1)

;; No Littering prevents backup files from being created in the same location as the file being worked on
(unless (package-installed-p 'no-littering)
  (require 'no-littering))

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
      ;; Prevent creation of "#...#" lock files
      ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Interlocking.html#Interlocking
      create-lockfiles nil)

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

;; Benchmark startup times
;; (use-package benchmark-init
;;   :ensure t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Setup evil for vim style keybinds
(use-package evil
  :init
  (setq evil-want-integration t
	evil-want-keybinding nil
	evil-vsplit-window-right t
	evil-split-window-below nil
	evil-undo-system 'undo-redo
	;; Search for the word under the cursor instead of the symbol
	;; https://github.com/emacs-evil/evil/pull/1431/commits/84347427a729b7cc325be05ea2996ec1ad3efda3
	evil-symbol-word-search t)

  ; Setting `split-height-threshold` to nil and `split-width-threshold` to 1 forces vertical splits
  ; This was specifically done for ensuring that magit panes are not opened in horizontal splits
  ; These values are copied from the Doom Emacs repository
  ; https://github.com/doomemacs/doomemacs/blob/61a7c541655038615e3f846a87db2e7d5883d35a/core/core-ui.el#L290
  (setq split-height-threshold nil
	split-width-threshold 160)
  (evil-mode))

;; Evil Collection is used for setting up vim keybindings in other buffers
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; General is used for setting up keybinds
(use-package general
  :init
  (general-evil-setup t))

;; Set the leader. This is used for keybindings
(defconst leader "SPC")

;; Prevent the cursor jumping to the middle of the page when scrolling to the bottom of the screen
;; https://stackoverflow.com/a/25777730
(setq scroll-conservatively 101)
(setq scroll-margin 5)

;; For some reason this doesn't work
;; (evil-ex-define-cmd "\"w" 'evil-write)
(evil-ex-define-cmd "W" 'evil-write)

(setq lazy-highlight-cleanup nil
      lazy-highlight-max-at-a-time nil
      lazy-highlight-initial-delay 0
      isearch-allow-scroll t)

;; Setup my theme to be used
(unless (package-installed-p 'eink-theme)
  (package-install-file (expand-file-name "eink-theme.el" user-emacs-directory)))

;; Load the theme
(load-theme 'eink t)

;; Add line numbers globally
(global-display-line-numbers-mode)

;; Use hl-line-mode everywhere
;; This makes it easier to see which line the cursor is on
(global-hl-line-mode)

;; Doom modeline is a nice package for showing meta information about the current buffer
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

;; Magit is a wrapper around git which is nice to use.
(use-package magit
  :after evil
  :defer t
  :init (defvar evil-collection-magit-use-$-for-end-of-line nil)
  :config
  ;; Set the max length of the commit message before wrapping to the next line
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

;; Automatically start in insert state when openning the commit buffer
;; https://emacs.stackexchange.com/a/14012
(add-hook 'with-editor-mode-hook 'evil-insert-state)

;; Set up which-key. This shows what options are availabe for key sequences
(use-package which-key
  :defer t
  :init
  (which-key-mode))

;; Open dired by pressing the '-' (hyphen) button
(general-define-key
 :states 'normal
 "-" 'dired-jump)

;; Kill the dired buffer when pressing 'q'
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

;; Keybinds for manipulating window panes
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

;; Vertico is a nice completion package
(use-package vertico
  :defer t
  :init
  (vertico-mode)
  (setq vertico-count 15))

(general-define-key
 :states 'normal
 :keymaps 'override
 " " 'vertico-find)

(use-package consult
  :defer t
  :init
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)
  :config
  (autoload 'projectile-project-root "projectile"))

(use-package orderless
  :init
  (setq completion-styles '(orderless flex)))

(setq-default fill-column 120)

(use-package projectile
  :defer t
  :config
  ;; Add npm projects
  ;; This allows for usage of `projectile-toggle-between-implementation-and-test`
  (projectile-mode)
  (projectile-register-project-type 'npm '("package.json")
                                  :project-file "package.json"
				  :configure "npm ci"
				  :compile "npm run build"
				  :test "npm test"
				  :run "npm start"
				  :test-suffix ".spec"))

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix leader
 "SPC" 'projectile-find-file
 "pA" 'projectile-add-known-project
 "pC" 'projectile-compile-project
 "pT" 'projectile-test-project
 "pi" 'projectile-invalidate-cache
 "pp" 'projectile-switch-project
 "pt" 'projectile-toggle-between-implementation-and-test)

;; Auto discovery wasn't working for me. Instead manually add the project with "SPC P A" to add the project to the cache
;; (setq projectile-project-search-path '("~/projects/"))
;; (projectile-discover-projects-in-search-path)

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

;; Set the initial buffer to the scratch buffer
(setq inhibit-startup-message t)
;; (setq initial-buffer-choice 'scratch)
;; Set a different message
(setq initial-scratch-message ";; Stay focussed\n\n")

(use-package company
  :defer 2
  :config
  ;; Prevent Company completion in Text Mode from being converted into lowercase
  (setq company-dabbrev-downcase nil))

(use-package eglot
  :defer 3)

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
