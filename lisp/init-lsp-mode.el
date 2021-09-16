;;; init-lsp-mode.el --- Load lsp mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; Load lsp mode configuration

;;; Code:

(use-package lsp-mode
  :ensure t
  :defer 0
  :hook (
   (lsp-mode . lsp-enable-which-key-integration)
   (java-mode . #'lsp-deferred))
  :init
  (setq
    lsp-keymap-prefix "C-c l" ; this is for which-key integration documentation, need to use lsp-mode-map
    lsp-enable-file-watchers nil
    read-process-output-max (* 1024 1024)  ; 1 mb
    lsp-idle-delay 0.500)
  (setq lsp-headerline-breadcrumb-enable nil)
  :config
  (setq lsp-enable-on-type-formatting nil)
  (setq gc-cons-threshold 100000000)
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

(provide 'init-lsp-mode)

;;; init-lsp-mode.el ends here
