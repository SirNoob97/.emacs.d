;;; init-lsp-mode.el --- Load lsp mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; Load lsp mode configuration

;;; Code:
;;; init-lsp-mode.el --- Load lsp mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; Load lsp mode configuration

;;; Code:

(use-package lsp-mode
  :ensure t
  :defer 0
  :bind
  (:map lsp-mode-map
	("M-RET" . lsp-execute-code-action)
	("C-c l f b" . lsp-format-buffer)
	("C-c l f r" . lsp-format-region))
  :hook (
   (lsp-mode . lsp-enable-which-key-integration)
   (java-mode . #'lsp-deferred))
  :init
  (setq lsp-keymap-prefix "C-c l") ; this is for which-key integration documentation, need to use lsp-mode-map
  (setq lsp-enable-file-watchers nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-lens-enable nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-modeline-diagnostics-enable nil) ; flycheck handle this
  (setq lsp-log-io nil)
  (setq read-process-output-max (* 1024 1024))  ; 1 mb
  (setq lsp-idle-delay 0.500)
  (setq gc-cons-threshold 100000000)
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

(provide 'init-lsp-mode)

;;; init-lsp-mode.el ends here
