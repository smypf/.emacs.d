;;;; my-compile.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;;

;;; Code:
;; TODO replace items with a variable read from .dir-locals.el
(defvar smypf-compile-items '("nvm use; npm test" "nvm use; npm run build" "nvm use; npm test -- -u"))

(defun smypf-choose-compile-command ()
  "Prompt the user to select multiple items from a list and call `compile`."
  (interactive)
  (let* ((selected-items (completing-read-multiple "Select compile job: " smypf-compile-items))
         (new-items (cl-remove-if (lambda (item) (member item smypf-compile-items)) selected-items))
         (updated-list (append smypf-compile-items new-items)))
    (setq smypf-compile-items updated-list)
    (compile (car (last selected-items nil)))))

;; Automatically scroll compilation buffer output
(use-package compile
  :ensure nil
  :bind ("C-c T" . smypf-choose-compile-command)
  :custom
  ;;(compilation-error-regexp-alist ('gnu))
  (compilation-scroll-output t))


;; original values for compilation-error-regexp-alist
;; In the end I just deleted these using the customise interface
;; Set to original value to restore this.
;; (rust-dbg! cargo rustc-panics rustc-backtrace rustc-colon rustc rustc-refs tsx absoft ada aix ant bash borland
;;            python-tracebacks-and-caml cmake cmake-info comma msft edg-1 edg-2 epc ftnchek gradle-kotlin
;;            gradle-kotlin-legacy gradle-android iar ibm irix java javac jikes-file maven jikes-line clang-include
;;            gcc-include ruby-Test::Unit lua lua-stack gmake gnu cucumber lcc makepp mips-1 mips-2 oracle perl php rust
;;            rxp shellcheck sparc-pascal-file sparc-pascal-line sparc-pascal-example sun sun-ada watcom 4bsd gcov-file
;;            gcov-header gcov-nomark gcov-called-line gcov-never-called perl--Pod::Checker perl--Test perl--Test2
;;            perl--Test::Harness weblint guile-file guile-line typescript-tsc-plain typescript-tsc-pretty)

;;; Package:
(provide 'my-compile)
;;; my-compile.el ends here
