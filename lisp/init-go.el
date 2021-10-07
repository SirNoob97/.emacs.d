;;; init-go.el --- Load go mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; Load go mode configuration

;;; Code:

(use-package go-mode :config (add-hook 'go-mode-hook 'lsp))

;;; init-go.el ends here
