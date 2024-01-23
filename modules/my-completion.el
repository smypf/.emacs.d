;;;; my-completion.el --- Make it quicker to do stuff  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;; Functionality which allows for doing more with fewer keystrokes

;;; Code:

;; Vertico is a nice completion package
;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)
  (vertico-multiform-mode)

  ;; Bind TAB and Shift-TAB to next and previous option.
  ;; This allows for completion at point in a way that is useful for me
  ;; This is adapted from https://github.com/minad/vertico/issues/143
  ;; TODO Figure out what the correct implementation of this is.
  ;; Tab to select - Any other characters to choose and continue?
  ;; This should also only apply to the completion and ont other sections
  :bind (:map vertico-map
              ("TAB" . vertico-next)
              ("M-TAB" . vertico-insert)
              ;; ("TAB" . minibuffer-complete)
              ("<backtab>" . vertico-previous)
              ("S-TAB" . vertico-previous))

  :config
  ;; Show more candidates
  (setq vertico-count 15
        vertico-cycle t
        ;; Configure the display per command.
        ;; Use a buffer with indices for imenu
        ;; and a flat (Ido-like) menu for M-x.
        vertico-multiform-commands '((consult-ripgrep buffer indexed)
                                     (consult-eglot-symbols buffer indexed))

        ;; Display ripgrep candidates in a buffer on the right of the page
        vertico-buffer-display-action
        '(display-buffer-in-side-window
          (side . right)
          (window-width . 0.3)))
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

;; Configure directory extension.
;; This can be done by checking if the string ends with a '/'.
(defun my-vertico-expand-next ()
  "Enter directory or shift to next file."
  (interactive)
  (if (string-suffix-p "/" (vertico--candidate)) (vertico-insert) (vertico-next)))

(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  ;; TODO this overrides the binds above. This is not really a problem since my-vertico-expand-next works nicely
  ;; but this should be fixed at some point
  :bind (:map vertico-map
              ("TAB" . my-vertico-expand-next)
              ("<backtab>" . vertico-directory-up)
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :ensure nil
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  ;;  (setq minibuffer-prompt-properties
  ;;        '(read-only t cursor-intangible t face minibuffer-prompt))
  ;;  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  ;; (setq enable-recursive-minibuffers t)
  ;; TAB cycle if there are only few candidates

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;;(setq read-extended-command-predicate
  ;;      #'command-completion-default-include-p)

  ;; Completion using vertico as the completion ui.
  (setq completion-cycle-threshold 2)
  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  ;; TODO this is where the completion in vertico is.
  ;; Perhaps this needs to be changed here.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  ;; https://emacsredux.com/blog/2016/01/31/use-tab-to-indent-or-complete/
  (setq tab-always-indent 'complete))

;; TODO get this working properly
;; (use-package vertico-directory
;;   :after vertico
;;   :ensure nil
;;   :config
;;   (define-key vertico-map (kbd "M-h") 'vertico-directory-up))


;; TODO Optionally close vertico only when I want to.
;; E.g. Pressing S-Enter will keep the mini buffer around allowing me to return to it (somehow)
;; Look into (setq enable-recursive-minibuffers t)
;; https://www.reddit.com/r/emacs/comments/qkgnhe/comment/hixh0f7

(use-package vertico-repeat
  :after vertico
  :defer t
  :ensure nil)

(use-package consult
  :defer t
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (autoload 'projectile-project-root "projectile")
  ;; Set the selected region to be the initial search term for consult-line and consult-ripgrep
  ;; https://takeonrules.com/2023/03/14/spending-a-bit-of-time-reviewing-consult-emacs-package/
  (consult-customize consult-line
                     consult-ripgrep
                     :initial (when (use-region-p)
                                (buffer-substring-no-properties
                                 (region-beginning) (region-end))))
  ;; Hide the modeline when opening vertico buffers
  ;; This is done to hide the ugly modeline
  ;; It needs to be done on a timer as this is overwritten by vertico
  ;; https://github.com/minad/vertico/blob/main/extensions/vertico-buffer.el#L163C22-L163C22
  ;; This was adapted from https://bzg.fr/en/emacs-hide-mode-line/
  ;; This was causing the mode line to be hidden on other buffers, which was not desirableq
  ;; (defun hide-mode-line ()
  ;;   (interactive)
  ;;   (setq mode-line-format nil)
  ;;   (force-mode-line-update)
  ;;   (redraw-display))
  ;; (add-hook 'vertico-buffer-mode-hook (lambda () (run-with-timer 0.1 nil 'hide-mode-line)))
  :bind (("C-c h" . consult-history)
         ;; Removed as this doesn't allow for entering the "*" character when searching
         ;; :map vertico-map
         ;; ("*" . vertico-next)
         :map meow-normal-state-keymap
         ("*" . consult-line))
  :hook
  (completion-list-mode . consult-preview-at-point-mode))

(use-package orderless
  :defer t
  :init
  (setq completion-styles '(orderless flex))
  :commands (orderless-filter))

;; This give better sorting
(use-package fuz-bin
  :commands (fuz-bin-load-dyn)
  :init (fuz-bin-load-dyn)
  :load-path "~/tools/fuz-bin/")

(use-package fussy
  :ensure t
  :config
  (setq fussy-score-fn 'fussy-fuz-bin-score)
  (setq fussy-filter-fn 'fussy-filter-orderless)
  ;;(setq fussy-filter-fn 'fussy-filter-orderless-flex)

  (push 'fussy completion-styles)
  (setq
   ;; For example, project-find-file uses 'project-files which uses
   ;; substring completion by default. Set to nil to make sure it's using
   ;; flx.
   completion-category-defaults nil
   completion-category-overrides nil)

  ;; `eglot' defaults to flex, so set an override to point to fussy instead.
  (with-eval-after-load 'eglot
    (add-to-list 'completion-category-overrides
                 '(eglot (styles fussy basic)))))

;; This may be the package to use having exhausted the list here
;; https://github.com/jojojames/fussy
;;(use-package fuz
;;  :ensure t
;;  :config
;;  (setq fussy-score-fn 'fussy-fuz-bin-score)
;;  (unless (require 'fuz-core nil t)
;;    (fuz-build-and-load-dymod)))

;; I wasn't using this.
                                        ; (use-package marginalia
                                        ;  :defer t
                                        ;  :ensure t
                                        ;  :config
                                        ;  (marginalia-mode))

(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c c p" . completion-at-point)  ;; capf
;;;;;;         ("C-c c t" . complete-tag)        ;; etags
         ("C-c c P" . cape-dabbrev))        ;; or dabbrev-completion
;;;;;;         ("C-c p h" . cape-history)
;;;;;;         ("C-c p f" . cape-file)
;;;;;;         ("C-c p k" . cape-keyword)
;;;;;;         ("C-c p s" . cape-symbol)
;;;;;;         ("C-c p a" . cape-abbrev)
;;;;;;         ("C-c p i" . cape-ispell)
;;;;;;         ("C-c p l" . cape-line)
;;;;;;         ("C-c p w" . cape-dict)
;;;;;;         ("C-c p \\" . cape-tex)
;;;;;;         ("C-c p _" . cape-tex)
;;;;;;         ("C-c p ^" . cape-tex)
;;;;;;         ("C-c p &" . cape-sgml)
;;;;;;         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-sgml)
  (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-line)

  ;; Add completion in the git commit message buffer.
  ;; This isn't a perfect implementation
  (eval-after-load 'magit
    '(progn
       (define-key git-commit-mode-map (kbd "TAB") #'cape-dabbrev)))
  )

(use-package backward-forward
  :init
  (backward-forward-mode)
  :bind
  (("C-M-<left>" . backward-forward-previous-location)
   ("C-M-<right>" . backward-forward-next-location)))

;; Embark Export (M-. E) will send all results to a buffer.
;; This is useful as it means that it is not necessary to continuously open the consult buffer to visit subsequent matches
;; This is enough for me to enable this.
(use-package embark
  :defer t
  :ensure t
  :bind
  (("M-." . embark-act)         ;; pick some comfortable binding
   ("M-/" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))



;; Use Embark to show bindings in a key prefix with `C-h`
(setq prefix-help-command #'embark-prefix-help-command)

(with-eval-after-load 'embark-consult
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

(defun search-thing-at-point ()
  (interactive)
  (consult-ripgrep (projectile-project-root) (thing-at-point 'symbol)))

;; Enable IDO for file completion
;; I don't like this
;; (ido-mode)

;; https://github.com/mhayashi1120/Emacs-wgrep
(use-package wgrep
  :defer t)

(when window-system
  ;; Corfu
  ;; Perhaps use this
  (use-package corfu
    :defer t
    :config
    (defun corfu-move-to-minibuffer ()
      (interactive)
      (let ((completion-extra-properties corfu--extra)
            completion-cycle-threshold completion-cycling)
        (apply #'consult-completion-in-region completion-in-region--data)))
    (define-key corfu-map "\M-m" #'corfu-move-to-minibuffer)

    (defun my-corfu-complete-and-space ()
      (interactive)
      (corfu-complete)
      (insert " "))
    ;; Optional customizations
    :custom
    (corfu-cycle t)                   ;; Enable cycling for `corfu-next/previous'
    (corfu-auto nil)                  ;; Enable auto completion
    ;; (corfu-separator ?\s)          ;; Orderless field separator
    ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
    ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
    (corfu-preview-current t)         ;; Enable current candidate preview
    (corfu-preselect-first t)         ;; Enable candidate preselection
    (corfu-on-exact-match 'quit)      ;; Configure handling of exact matches
    ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
    ;; (corfu-scroll-margin 5)        ;; Use scroll margin

    :bind
    (:map corfu-map
          ;; Tab will select the next option
          ("TAB" . corfu-next)
          ([tab] . corfu-next)
          ;; Shift-Tab will select the previous option
          ("S-TAB" . corfu-previous)
          ([backtab] . corfu-previous)
          ;; todo space doesn't work
          ([? ] . my-corfu-complete-and-space)
          ;; Enter will select the option
          ([enter] . corfu-complete))

    ;; Enable Corfu only for certain modes.
    ;; :hook ((prog-mode . corfu-mode)
    ;;        (shell-mode . corfu-mode)
    ;;        (eshell-mode . corfu-mode))

    ;; Recommended: Enable Corfu globally.
    ;; This is recommended since Dabbrev can be used globally (M-/).
    ;; See also `corfu-excluded-modes'.
    :init
    (global-corfu-mode)))

;; This has been removed as it was slowing everything down too much.
;;(use-package corfu-candidate-overlay
;;  :after corfu
;;  :config
;;  ;; enable corfu-candidate-overlay mode globally
;;  ;; this relies on having corfu-auto set to nil
;;  (corfu-candidate-overlay-mode +1)
;;  ;; bind Ctrl + TAB to trigger the completion popup of corfu
;;  ;; (global-set-key (kbd "C-<tab>") 'completion-at-point)
;;  ;; bind Ctrl + Shift + Tab to trigger completion of the first candidate
;;  ;; (keybing <iso-lefttab> may not work for your keyboard model)
;;  ;;(global-set-key (kbd "C-<iso-lefttab>") 'corfu-candidate-overlay-complete-at-point))
;;  )

;;; Package:
(provide 'my-completion)
;;; my-completion.el ends here
