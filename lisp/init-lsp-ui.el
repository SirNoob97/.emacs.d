;;; init-lsp-ui.el --- Load lsp-ui configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; Load lsp-ui configuration

;;; Code:

(use-package lsp-ui
  :ensure t
  :defer 0
  :bind (:map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references))
  :init
  (setq lsp-ui-peek-always-show t)
  :config
  (define-key lsp-ui-mode-map (kbd "C-c l k") 'lsp-ui-doc-show)
  (define-key lsp-ui-mode-map (kbd "C-c l s") 'lsp-ui-doc-hide))

(provide 'init-lsp-ui)

;;; init-lsp-ui.el ends here
