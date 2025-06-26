;;;; my-ai.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Sasha Yee

;;; Commentary:

;;

;;; Code:

;; (use-package gptel
;;   :defer t
;;   :config
;;   ;; OPTIONAL configuration
;;   (setq
;;    gptel-model 'devstral
;;    gptel-backend (gptel-make-ollama "Ollama"
;;                    :host "localhost:11434"
;;                    :stream t
;;                    :models '(devstral))))

;; (use-package gptel-ollama
;;   :custom
;;      (gptel-backend (gptel-make-ollama "Ollama"
;;                    :host "localhost:11434"
;;                    :stream t
;;                    :models '(devstral))))

;;; Package:
(provide 'my-ai)
;;; my-ai.el ends here
