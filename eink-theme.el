;;; eink-theme.el --- eink
;;; Version: 1.0
;;; Commentary:
;;; A theme called eink
;;; Code:

;; Copyright (C) 2020 Sasha Yee

;; Author: Sasha Yee
;; Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,

;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Low distraction, minimalistic color theme emulating reading
;; on E Ink devices.

;;; Credits:

;; Inspired by:
;;
;; https://bitbucket.org/kisom/eink.vim
;; https://github.com/dmand/eink.el
;; http://www.daveliepmann.stfi.re/tufte-css/?sf=wklwy

;;; Code:

(deftheme eink
  "Theme emulating reading on an E Ink device.")

;; base purple - dfcee7
;; analogous - #e7cee3 & #d3cee7
(let ((fg                "#444444")
      (fg-table          "#222291")
      (bg                "#fffff8")
      ;; (bg-light          "#646464")
      (comment           "#c1c1bd")
      ;; (fg-light          "#ddddd8")
      (bg-highlight      "#fff1aa")
      (bg-highlight-2    "LightCyan")
      (bg-highlight-3    "LightGreen")
      (hl-line-bg        "#f4f4e8")
      ;; (org-todo          "#ff98a3")
      (isearch-bg        "#d3cee7")
      (lazy-highlight-bg "#e1c5e8")
      (purple-bg      "#dfcee7"))


  (custom-theme-set-faces
   'eink

   ;; generic stuff
   `(default ((t (:background ,bg :foreground ,fg))))

   `(button ((t (:foreground ,fg :underline t))))
   `(widget-field ((t (:foreground ,fg :background ,bg :underline t))))
   `(custom-visibility ((t (:foreground ,fg :background ,bg :underline t))))
   `(custom-group-tag ((t (:foreground ,fg :background ,bg :weight bold))))
   `(custom-state ((t (:foreground ,fg :background ,bg :weight bold))))

   `(cursor ((t (:background ,fg :foreground "white smoke"))))
   `(custom-variable-tag ((t (:foreground ,fg))))
   `(default-italic ((t (:italic t))))

   `(region ((t (:background ,purple-bg :foreground ,fg))))

   `(reb-match-0 ((t (:background ,purple-bg :weight bold))))
   `(reb-match-1 ((t (:background ,purple-bg :weight bold))))
   `(reb-match-2 ((t (:background ,purple-bg :weight bold))))
   `(reb-match-3 ((t (:background ,purple-bg :weight bold))))

   ;; `(font-latex-bold-face ((t (:foreground ,fg))))
   ;; `(font-latex-italic-face ((t (:foreground ,fg :slant italic))))
   ;; `(font-latex-match-reference-keywords ((t (:foreground ,fg))))
   ;; `(font-latex-match-variable-keywords ((t (:foreground ,fg))))
   ;; `(font-latex-string-face ((t (:foreground "#a9a9a9"))))

   `(font-lock-builtin-face ((t (:background ,bg :foreground ,fg))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,comment))))
   `(font-lock-comment-face ((t (:foreground ,comment))))
   `(font-lock-constant-face ((t (:foreground ,fg))))
   `(font-lock-doc-face ((t (:foreground ,fg))))
   `(font-lock-function-name-face ((t (:foreground ,fg))))
   `(font-lock-keyword-face ((t (:foreground ,fg))))
   `(font-lock-preprocessor-face ((t (:foreground ,fg))))
   `(font-lock-reference-face ((t (:foreground ,fg))))
   `(font-lock-string-face ((t (:foreground ,fg))))
   `(font-lock-type-face ((t (:foreground ,fg))))
   `(font-lock-variable-name-face ((t (:foreground ,fg :underline nil))))
   `(font-lock-warning-face ((t (:foreground ,fg))))

   `(fringe ((t (:background ,bg :foreground ,bg))))
   ;; `(gnus-header-content ((t (:foreground ,fg))))
   ;; `(gnus-header-from ((t (:foreground ,fg))))
   ;; `(gnus-header-name ((t (:foreground ,fg))))
   ;; `(gnus-header-subject ((t (:foreground ,fg))))

   `(highlight ((t (:background ,purple-bg))))
   
   ;; `(ido-first-match ((t (:foreground ,fg))))
   ;; `(ido-only-match ((t (:foreground ,fg))))
   ;; `(ido-subdir ((t (:foreground ,fg))))

   `(isearch ((t (:background ,isearch-bg :foreground ,fg))))

   `(lazy-highlight ((t (:background ,purple-bg :foreground ,fg))))

   `(link ((t (:foreground ,fg))))

   `(minibuffer-prompt ((t (:foreground ,fg))))

   `(mode-line ((t (:background ,purple-bg :foreground ,fg))))
   `(doom-modeline-evil-insert-state ((t (:foreground ,fg))))
   `(doom-modeline-evil-visual-state ((t (:foreground ,fg))))
   `(doom-modeline-evil-normal-state ((t (:foreground ,fg))))
   `(doom-modeline-lsp-success ((t (:foreground ,fg))))
   `(doom-modeline-buffer-modified ((t (:foreground "#9e4d93" :weight bold))))
   `(doom-modeline-urgent ((t (:foreground "#d4a7cd"))))

   `(doom-modeline-info ((t (:foreground ,fg))))

   ;; `(mode-line-buffer ((t (:foreground ,fg))))
   ;; `(mode-line-inactive ((t (:background ,bg-light :foreground ,bg-light :height 0.8))))
   ;; `(mode-line-minor-mode ((t (:weight ultra-light))))
   `(slime-repl-inputed-output-face ((t (:foreground ,fg))))
   `(whitespace-line ((t (:background ,bg-highlight-2 :foreground ,fg))))
   `(hc-tab ((t (:inherit default))))

   ;; org
   ;; `(org-agenda-date ((t (:foreground ,fg))))
   ;; `(org-agenda-date-today ((t (:foreground ,fg))))
   ;; `(org-agenda-date-weekend ((t (:foreground ,fg :weight normal))))
   ;; `(org-agenda-structure ((t (:foreground ,fg))))
   ;; `(org-block ((t (:foreground ,fg))))
   ;; `(org-block-begin-line ((t (:foreground ,fg-light))))
   ;; `(org-block-end-line ((t (:foreground ,fg-light))))
   ;; `(org-verbatim ((t (:foreground ,fg))))
   ;; `(org-date ((t (:foreground ,fg) :underline)))
   ;; `(org-headline-done ((t (:foreground ,fg))))
   ;; `(org-done ((t (:foreground ,fg-light))))
   ;; `(org-hide ((t (:foreground ,bg))))
   ;; ;;use :overline to give headings more top margin
   ;; `(org-level-1 ((t (:foreground ,fg :weight bold))))
   ;; `(org-level-2 ((t (:foreground ,fg :weight bold))))
   ;; `(org-level-3 ((t (:foreground ,fg :weight bold))))
   ;; `(org-level-4 ((t (:foreground ,fg :weight bold))))
   ;; `(org-level-5 ((t (:foreground ,fg :weight bold))))
   ;; `(org-level-6 ((t (:foreground ,fg :weight bold))))
   ;; `(org-link ((t (:foreground ,fg :underline t))))
   ;; `(org-quote ((t (:foreground ,fg :slant italic :inherit org-block))))
   ;; `(org-scheduled ((t (:foreground ,fg))))
   ;; `(org-sexp-date ((t (:foreground ,fg))))
   ;; `(org-special-keyword ((t (:foreground ,fg))))
   ;; `(org-todo ((t (:foreground ,org-todo :weight bold))))
   ;; `(org-verse ((t (:inherit org-block :slant italic))))
   ;; `(org-table ((t (:foreground ,fg-table))))
   ;; `(org-document-title ((t (:foreground ,fg :weight semi-bold))))

   ;; magit
   `(magit-header ((t (:weight semi-bold))))
   `(magit-item-mark ((t (:background ,bg-highlight))))
   `(magit-item-highlight ((t (:weight bold))))
   `(magit-section-heading ((t (:weight semi-bold))))
   `(magit-section-highlight ((t (:background ,hl-line-bg))))
   `(magit-diff-context-highlight ((t (:foreground ,fg))))
   `(magit-branch-local ((t (:weight bold))))
   `(magit-branch-remote ((t (:weight bold))))
   `(magit-log-author ((t (:foreground ,fg))))

   ;; diff
   `(diff-added ((t (:background "#e9ffe9"))))
   `(diff-removed ((t (:background "#ffecec"))))
   `(diff-refine-added ((t (:background "#a4f4a3"))))
   `(diff-refine-removed ((t (:background "#f9cbca"))))
   `(magit-diff-added-highlight ((t (:weight demibold :background "#e9ffe9"))))
   `(magit-diff-added ((t (:background "#e9ffe9"))))
   `(magit-diff-removed-highlight ((t (:weight demibold :background "#ffecec"))))
   `(magit-diff-removed ((t (:background "#ffecec"))))

   ;; git-timemachine
   ;; `(git-timemachine-minibuffer-author-face ((t (:inherit default))))
   ;; `(git-timemachine-minibuffer-detail-face ((t (:weight bold))))

   ;; compile
   `(compilation-error ((t (:inherit error))))

   ;; flycheck
   `(flycheck-error ((t (:inherit error))))
   `(flycheck-warning ((t (:inherit warning))))

   ;; dired
   `(dired-directory ((t (:weight bold))))
   `(dired-subtree-depth-1-face ((t (:inherit default))))
   `(dired-subtree-depth-2-face ((t (:inherit default))))
   `(dired-subtree-depth-3-face ((t (:inherit default))))
   `(dired-subtree-depth-4-face ((t (:inherit default))))
   `(diredfl-file-name ((t (:inherit default))))
   `(diredfl-file-suffix ((t (:inherit default))))
   `(diredfl-date-time ((t (:inherit default))))
   `(diredfl-number ((t (:inherit default))))
   `(diredfl-no-priv ((t (:inherit default))))
   `(diredfl-dir-priv ((t (:inherit default))))
   `(diredfl-read-priv ((t (:inherit default))))
   `(diredfl-write-priv ((t (:inherit default))))
   `(diredfl-exec-priv ((t (:inherit default))))
   `(diredfl-rare-priv ((t (:inherit default))))
   `(diredfl-dir-heading ((t (:inherit default))))
   `(diredfl-dir-name ((t (:inherit default))))
   `(diredfl-ignored-file-name ((t (:inherit default))))
   `(diredfl-compressed-file-suffix ((t (:inherit default))))
   `(diredfl-symlink ((t (:inherit default))))

   ;; helm
   ;; `(helm-source-header ((t (:foreground ,fg :background "grey90"))))
   ;; `(helm-header ((t (:foreground ,fg))))
   ;; `(helm-selection-line ((t (:inherit region))))
   ;; `(helm-selection ((t (:background ,bg-highlight))))
   ;; `(helm-ff-directory ((t (:foreground ,fg))))
   ;; `(helm-ff-dotted-directory ((t (:foreground ,fg))))
   ;; `(helm-ff-symlink ((t (:foreground ,fg :slant italic))))
   ;; `(helm-ff-executable ((t (:foreground ,fg))))

   ;; iedit
   ;; `(iedit-occurrence ((t (:background ,bg-highlight-3 :foreground ,fg))))

   ;; company
   `(company-echo-common ((t (:foreground ,fg))))
   `(company-tooltip-selection ((t (:background ,bg-highlight))))

   ;; parens - parenface
   ;; '(parenface-paren-face ((t (:foreground "gray70"))))
   ;; '(parenface-curly-face ((t (:foreground "gray70"))))
   ;; '(parenface-bracket-face ((t (:foreground "gray70"))))

   ;; parens - paren-face
   ;; '(parenthesis ((t (:foreground "gray70"))))

   ;; parens - other
   ;; `(sp-show-pair-match-face ((t (:foreground "black"))))
   ;; `(sp-show-pair-mismatch-face ((t (:background "red" :foreground "black"))))
   `(show-paren-match ((t (:foreground "#AFA7D4"))))
   `(show-paren-mismatch ((t (:background "red" :foreground "black"))))

   ;; js2
   ;; `(js2-function-param ((t (:foreground ,fg))))
   ;; `(js2-external-variable ((t (:foreground ,fg))))

   ;; perl
   ;; `(cperl-hash-face ((t (:foreground ,fg))))
   ;; `(cperl-array-face ((t (:foreground ,fg))))
   ;; `(cperl-nonoverridable-face ((t (:foreground ,fg))))

   ;; rpm-spec-mode
   ;; `(rpm-spec-tag-face ((t (:inherit default))))
   ;; `(rpm-spec-package-face ((t (:inherit default))))
   ;; `(rpm-spec-macro-face ((t (:inherit default))))
   ;; `(rpm-spec-doc-face ((t (:inherit default))))
   ;; `(rpm-spec-var-face ((t (:inherit default))))
   ;; `(rpm-spec-ghost-face ((t (:inherit default))))
   ;; `(rpm-spec-section-face ((t (:inherit default))))

   ;; linum / nlinum-relative
   `(nlinum-relative-current-face ((t (:inherit normal))))
   `(linum ((t (:inherit normal))))

   ;; web-mode
   ;; `(web-mode-current-element-highlight-face ((t (:inherit normal :foreground ,fg))))

   ;; misc
   ;; `(idle-highlight ((t (:background ,bg-highlight))))
   ;; `(yas-field-highlight-face ((t (:background "#eeeee8" :foreground ,fg))))
   ;; `(eshell-prompt ((t (:foreground ,fg))))
   ;; `(cider-result-overlay-face ((t (:weight bold))))

   ;; evil-quickscope
   ;; `(evil-quickscope-first-face ((t (:foreground ,fg :background "#eeeee8"))))
   ;; `(evil-quickscope-second-face ((t (:foreground ,fg :background ,bg-highlight-3))))

   ;; evil-snipe
   ;; `(evil-snipe-first-match-face ((t (:foreground ,fg :background "#eeeee8"))))
   ;; `(evil-snipe-matches-face ((t (:foreground ,fg :background ,bg-highlight-3))))

   ;; evil
   `(evil-ex-lazy-highlight ((t (:background ,isearch-bg))))
   `(evil-ex-substitute-matches ((t (:background ,bg-highlight-2))))
   `(evil-ex-substitute-replacement ((t (:background ,bg-highlight :underline nil :foreground ,fg))))

   ;; describe-text-properties found this
   `(typescript-jsdoc-tag ((t (:inherit font-lock-comment-face))))
   `(typescript-jsdoc-type ((t (:inherit font-lock-comment-face))))
   `(typescript-jsdoc-value ((t (:inherit font-lock-comment-face))))

   `(hl-line ((t (:background ,hl-line-bg))))
   `(hl-fill-column-face ((t (:background ,hl-line-bg))))

   ;; `(whitespace-indentation ((t (:foreground ,fg-light :background ,bg))))
   ;; `(highlight-indent-guides-character-face ((t (:foreground ,fg-light))))
   ;; `(highlight-indent-guides-even-face ((t (:foreground ,fg-light))))
   ;; `(highlight-indent-guides-odd-face ((t (:foreground ,fg-light))))

   ;; from https://lepisma.xyz/2017/10/28/ricing-org-mode/
   ;;`(variable-pitch ((t (:family ,et-font :background ,bg :foreground ,fg :height 1.7))))

   ;;`(org-document-title ((t
                          ;;(:inherit variable-pitch :height 1.3 :weight normal)
                          ;;(:inherit nil :family ,et-font :height 1.8 :underline nil))))

   ;;`(org-document-info ((t (:slant italic) (:height 1.2 :slant italic))))

   ;;`(org-level-1 ((t (:inherit variable-pitch :height 1.3 :weight bold))))
   ;;`(org-level-2 ((t (:inherit variable-pitch :height 1.2 :weight bold))))
   ;;`(org-level-3 ((t (:inherit variable-pitch :height 1.1 :weight bold))))
   ;;`(org-level-4 ((t (:inherit variable-pitch :height 1.1 :weight bold))))
   ;;`(org-level-5 ((t (:inherit variable-pitch :height 1.1 :weight bold))))
   ;;`(org-level-6 ((t (:inherit variable-pitch :height 1.1 :weight bold))))
   ;;`(org-level-7 ((t (:inherit variable-pitch :height 1.1 :weight bold))))
   ;;`(org-level-8 ((t (:inherit variable-pitch :height 1.1 :weight bold))))
   ;;`(org-hide ((t (:foreground ,bg))))
   ;;`(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   ;;`(org-headline-done ((t (:family ,et-font :strike-through t))))
   ;;`(org-block-begin-line ((t (:height 0.8 :family ,sans-mono-font))))
   ;;`(org-block-end-line ((t (:height 0.8 :family ,sans-mono-font))))
   ;;`(org-document-info-keyword ((t (:height 0.8 :foreground ,comment))))
   ;;`(org-link ((t (:underline nil :weight normal))))
   ;;`(org-special-keyword ((t (:family ,sans-mono-font :height 0.8))))
   ;;`(org-done ((t (:inherit variable-pitch))))

   `(transient-active-infix ((t (:inherit default))))
   `(info-index-match ((t (:inherit default))))
   `(match ((t (:background ,bg-highlight :foreground ,fg))))

   `(reb-match-0 ((t (:inherit default))))
   `(reb-match-1 ((t (:inherit default))))
   `(reb-match-2 ((t (:inherit default))))
   `(reb-match-3 ((t (:inherit default))))

   ;; `(org-agenda-clocking ((t (:inherit default))))
   ;; `(org-agenda-column-dateline ((t (:inherit default))))
   ;; `(org-clock-overlay ((t (:inherit default))))

   ;; `(sh-quoted-exec ((t (:inherit default))))

   ;; `(window-divider ((t (:foreground ,bg-light :background ,bg-light))))

   `(lsp-ui-sideline-code-action ((t (:foreground ,comment))))
   `(lsp-ui-sideline-current-symbol ((t (:foreground ,comment))))


   `(flycheck-fringe-info ((t (:foreground ,comment))))
   `(compilation-info ((t (:foreground ,comment))))
   `(flycheck-error-list-info ((t (:foreground ,comment))))

   ;`(font-lock-type-face ((t (:foreground ,comment))))

   ;`(dap-ui-breakpoint-verified-fringe ((t (:foreground ,comment))))
   ;`(lsp-treemacs-file-info ((t (:foreground ,comment))))

   ; I don't like this. This is for the lsp messages
   `(success ((t (:foreground ,comment))))


   `(vertical-border ((t (:foreground ,comment :background ,bg))))
   ;;`(vertical-border ((t (:foreground ,bg-light :background ,bg-light))))

   ; Removed rainbow-delimiters
   ;; `(rainbow-delimiters-depth-1-face ((t (:foreground ,fg))))
   ;; `(rainbow-delimiters-depth-2-face ((t (:foreground ,fg))))
   ;; `(rainbow-delimiters-depth-3-face ((t (:foreground ,fg))))
   ;; `(rainbow-delimiters-depth-4-face ((t (:foreground ,fg))))
   ;; `(rainbow-delimiters-depth-5-face ((t (:foreground ,fg))))
   ;; `(rainbow-delimiters-depth-6-face ((t (:foreground ,fg))))
   ;; `(rainbow-delimiters-depth-7-face ((t (:foreground ,fg))))
   ;; `(rainbow-delimiters-depth-8-face ((t (:foreground ,fg))))
   ;; `(rainbow-delimiters-depth-9-face ((t (:foreground ,fg))))

   ;; `(tree-sitter-hl-face:property ((t (:inherit default))))

   ; Orderless
   `(orderless-match-face-0 ((t (:background ,purple-bg :weight bold))))
   `(orderless-match-face-1 ((t (:background ,purple-bg :weight bold))))
   `(orderless-match-face-2 ((t (:background ,purple-bg :weight bold))))
   `(orderless-match-face-3 ((t (:background ,purple-bg :weight bold))))

   `(consult-preview-match ((t (:weight bold :underline t))))
   `(consult-file ((t (:background ,purple-bg :weight bold))))


   ;; `(vertico-current ((t (:background ,hl-line-bg))))

   ;; I couldn't figure out how to strip things.
   ;; It has something to do with ansi-color.el and ansi-color-for-comint-mode
   ;; This _should_ work but it doesn't.
   ;; https://stackoverflow.com/a/71785402
   ;; Here we just replace the colours instead
   `(ansi-color-red ((t (:foreground ,fg))))
   `(ansi-color-blue ((t (:foreground ,fg))))
   `(ansi-color-bold ((t (:foreground ,fg))))
   `(ansi-color-cyan ((t (:foreground ,fg))))
   `(ansi-color-black ((t (:foreground ,fg))))
   `(ansi-color-faint ((t (:foreground ,fg))))
   `(ansi-color-green ((t (:foreground ,fg))))
   `(ansi-color-white ((t (:foreground ,bg))))
   `(ansi-color-italic ((t (:foreground ,fg))))
   `(ansi-color-yellow ((t (:foreground ,fg))))
   `(ansi-color-magenta ((t (:foreground ,fg))))
   `(ansi-color-bright-red ((t (:foreground ,fg))))
   `(ansi-color-bright-blue ((t (:foreground ,fg))))
   `(ansi-color-bright-cyan ((t (:foreground ,fg))))
   `(ansi-color-bright-black ((t (:foreground ,fg))))
   `(ansi-color-bright-green ((t (:foreground ,fg))))
   `(ansi-color-bright-white ((t (:foreground ,fg))))
   `(ansi-color-bright-yellow ((t (:foreground ,fg))))
   `(ansi-color-bright-magenta ((t (:foreground ,fg))))

   `(highlight-indent-guides-character-face ((t (:foreground ,comment))))
   `(highlight-indent-guides-top-character-face ((t (:foreground ,purple-bg))))
   ; `(highlight-indent-guides-stack-character-face ((t (:foreground ,purple-bg))))
   ))

;; to here
;;
;;


;; TODO Comments should remain toned down with hl-line-mode
;; TODO Remove usages of non variable values
;; TODO Fix highlight indent guide colours

(provide-theme 'eink)
;;; eink-theme.el ends here

