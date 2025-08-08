;;; package -- Sasha's Personal Configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Stuff gets loaded here.
;;; Most of the stuff is in module files.  These are located in the `/module` folder
;;; Code:

(select-frame-set-input-focus (selected-frame))

;; Set up melpa which is a package host
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; Compile Angel
                                        ; (use-package compile-angel
                                        ;   :ensure t
                                        ;   :demand t
                                        ;   :custom
                                        ;   (compile-angel-verbose nil)
                                        ;   :config
                                        ;   (with-eval-after-load "recentf"
                                        ;   (push (concat "/" (file-name-nondirectory recentf-save-file))
                                        ;         compile-angel-excluded-files))

                                        ;   (push ".emacs.d/init.el" compile-angel-excluded-files)
                                        ;   (push ".emacs.d/early-init.el" compile-angel-excluded-files)

                                        ;   (setq compile-angel-predicate-function
                                        ;       (lambda (file)
                                        ;         (and (not (file-in-directory-p file (expand-file-name "modules/" user-emacs-directory))))))
                                        ;   (compile-angel-on-load-mode)
                                        ;   (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode))

;; Uncomment to ensure that packages are installed.
;; (require 'use-package-ensure)
;; (setq use-package-always-ensure t)

;; settings to speedup initialisation
(setq gc-cons-threshold (* 32 1024 1024))
(setq gcmh-high-cons-threshold (* 32 1024 1024))
(setq gcmh-idle-delay-factor 20)
(setq jit-lock-defer-time 0.05)
(setq read-process-output-max (* 1024 1024))
(setq package-native-compile t)

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold (* 32 1024 1024)))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; if you don't use RTL ever, this could improve perf
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t)



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

;; Enable this to allow use-package-report to show package start time
(use-package use-package
  :ensure t
  :custom
  (use-package-compute-statistics t))

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
(require 'my-rust)
(require 'my-coding)
(require 'my-utility)
(require 'my-popper)
(require 'my-system)
(require 'my-tab-bar)
(require 'my-native-comp)
(require 'my-atlassian)
(require 'my-search)
(require 'my-ai)
(require 'my-vterm)
(require 'my-gumshoe)

;;(require 'my-golang)
;;(require 'my-restclient)

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

(setq use-package-compute-statistics t)
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
 '(compilation-error-regexp-alist
   '(rust-dbg! cargo rustc-panics rustc-backtrace rustc-colon rustc rustc-refs tsx gnu typescript-tsc-plain
               typescript-tsc-pretty))
 '(custom-safe-themes
   '("6c291791e76352f28e5b9034e2335fefced38c2af275d73e664c8ffe54a2efc6"
     "c5ce8f96cd0baa23ed973d0572c0a1cc24181c83eaf93e1444fbb99668acd259"
     "a7368ad6c2016b19aa6b5d853fd182a1f1d07a81f4d88412ef6438e21383e55e"
     "bb697acd1c079178f606a36b3e7217e2a6f5f920905f1ce21815aaadd711b713"
     "3f495f29670e5d84260a7a4e85955626ddb99da8fee9c580e38dd6341f602f7d"
     "308632d8369829e0c3610b73a3d1c708db68f4487d66be2760bc82a1102a288e"
     "4dfdc4ee3b5ace84c2c306596e53f02478e0cbe9d35471d161832488ad4713af"
     "c63281bceaa0ab53b694c28390f9e927e1e17a26ee1af52ca61ba7a49a2cdf6b"
     "e7820b899036ae7e966dcaaec29fd6b87aef253748b7de09e74fdc54407a7a02"
     "1781e8bccbd8869472c09b744899ff4174d23e4f7517b8a6c721100288311fa5"
     "cdab8585cf30d8ae482b4d5bb5dd2de3cd8d0a486b0b90da1958ed04c187363b"
     "021321ae56a45794f43b41de09fb2bfca184e196666b7d7ff59ea97ec2114559"
     "40d29cf577515779b178069a54d998a3da35a28b5d42246e4006dbdf7cf4b7a5"
     "f0393dbed2e12400391e2845e7443d92fbbc109a6a5b68549db416ffa9a7d26a"
     "43723b620f335ac047727a9dc13cb629b74a7c23349e9b5e0e6535dd662dadc4"
     "96ac3799e504479c862cce31b6882274fa4ad9490c57ccfab81c1bfb8c326795"
     "77ccee107184be05753c15ba11cae1f4f03012505969d46c4e3d76cac264e077"
     "3848c2c3e7a48d6dec6defbd5a90ea6f8c03c4aac835461ead0c2bef7651a174"
     "c6d63b27dea1738060614c48ce48cee42ee82ce27263dbd612a9230c86a4a8eb"
     "f98c6f84330f1f3490021c1f0ccb9f7e90797df0f2700fe3bd7fe8ad4dd67369"
     "680ba271ab61df49c4f8464b6f4d04b5bb2965691cec658bbd16bd8039faf69b" default))
 '(eglot-code-action-indicator "α")
 '(eldoc-echo-area-use-multiline-p nil)
 '(global-display-line-numbers-mode t)
 '(jest-test-options nil)
 '(jinx-camel-modes
   '(java-mode java-ts-mode js-mode js-ts-mode ruby-mode ruby-ts-mode rust-mode rust-ts-mode haskell-mode kotlin-mode
               swift-mode csharp-mode csharp-ts-mode objc-mode typescript-ts-mode typescript-mode python-mode
               python-ts-mode dart-mode go-mode go-ts-mode scala-mode groovy-mode tsx-ts-mode typescript-ts-mode))
 '(magit-process-apply-ansi-colors t nil nil "Customized with use-package magit")
 '(meow-tree-sitter-major-mode-language-alist
   '(("c++" . "cpp") ("ess-r" . "r") ("js" . "javascript") ("js2" . "javascript") ("js3" . "javascript")
     ("rjsx" . "javascript") ("rustic" . "rust") ("sh" . "bash") ("shell-script" . "bash") ("tsx" . "typescript")
     ("jtsx-jsx" . "javascript") ("jtsx-tsx" . "typescript")))
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(package-selected-packages
   '(abridge-diff apheleia ast-grep balanced-windows cape compile-angel consult-eglot copilot corfu dape deadgrep devdocs
                  dogears doom-modeline editorconfig eglot-booster embark-consult exec-path-from-shell fussy fuz-bin
                  git-link gptel gumshoe helpful highlight howm indent-bars jinx jira json-mode jtsx lua-mode magit
                  magit-prime meow-tree-sitter no-littering orderless org-contrib ox-jira perspective
                  pleasant-monochromish-theme popper private-comments-mode projectile rainbow-mode repeat-fu
                  reveal-in-osx-finder rg rustic shackle smart-delete surround tree-sitter-langs treesit-auto
                  ultra-scroll vertico vim-tab-bar visual-fill-column visual-regexp visual-regexp-steroids vterm
                  which-key xclip))
 '(package-vc-selected-packages
   '((ast-grep :vc-backend Git :url "https://github.com/sunskyxh/ast-grep.el")
     (copilot :vc-backend Git :url "https://github.com/copilot-emacs/copilot.el")
     (ultra-scroll :vc-backend Git :url "https://github.com/jdtsmith/ultra-scroll")
     (fuz-bin :vc-backend Git :url "https://github.com/jcs-legacy/fuz-bin")
     (eglot-booster :vc-backend Git :url "https://github.com/jdtsmith/eglot-booster")
     (tree-sitter-langs :vc-backend Git :url "https://github.com/emacs-tree-sitter/tree-sitter-langs")
     (combobulate :vc-backend Git :url "https://github.com/mickeynp/combobulate")))
 '(popper-mode-line '(:eval (propertize " POP " 'face 'mode-line-emphasis)))
 '(safe-local-variable-values
   '((eval progn (setq-local compilation-error-regexp-alist '(tsx)))
     (smypf-compile-items
      "root platform; nvm use; afm test unit -- /Users/syee/atlassian/afm/platform/packages/uip/pillar")
     (smypf-compile-items
      (format "root platform; nvm use; afm test unit -- /Users/syee/atlassian/afm/platform/packages/uip/pillar"))
     (smypf-compile-items . local-compile-items)
     (eval setq local-compile-items
           (list (format "root platform; nvm use; afm test unit -- %s" (projectile-project-root))))
     (smypf-compile-items eval ("root platform; nvm use; afm test unit -- %s" . projectile-project-root))
     (smypf-compile-items (format "root platform; nvm use; afm test unit -- %s" (projectile-project-root)))
     (smypf-compile-items "root platform; nvm use; afm test unit -- %s" . projectile-project-root)
     (smypf-compile-items "root platform; nvm use; afm test unit -- %s" projectile-project-root)
     (smypf-compile-items quote ("root platform; nvm use; afm test unit -- (pwd)"))
     (smypf-compile-items quote ("root platform; nvm use; yarn test -- services/volt-playground/"))
     (smypf-compile-items quote ("root platform; nvm use; yarn test -- packages/uip/pillar/"))
     (smypf-compile-items quote ("root; cd platform; nvm use; yarn test -- packages/uip/pillar/"))
     (eval magit-disable-selction-inserter 'magit-insert-unpulled-from-pushremote)
     (eval magit-disable-selection-inserter 'magit-insert-unpushed-to-pushremote)
     (smypf-compile-items quote ("parent; cd platform; yarn test -- packages/uip/volt")) (magit-refresh-buffers)
     (smypf-compile-items "parent; cd platform; yarn test -- packages/uip/frontkit")))
 '(tab-bar-mode t)
 '(vr/command-python
   "python3 /Users/syee/.emacs.d/elpa/visual-regexp-steroids-20170222.253/regexp.py")
 '(vr/engine python3))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code" :foundry "nil" :slant normal :weight regular :height 140 :width normal))))
 '(eglot-inlay-hint-face ((t nil)))
 '(tab-bar-tab ((t (:background "#dfcee7" :foreground "#444444" :box (:line-width (3 . 3) :color "#dfcee7")))))
 '(tab-bar-tab-inactive ((t (:background "#fffff8" :foreground "#c1c1bd" :box (:line-width (3 . 3) :color "#fffff8"))))))
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
