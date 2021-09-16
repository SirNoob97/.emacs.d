;;; init-treemacs.el --- Load treemacs -*- lexical-binding: t -*-

;;; Commentary:
;;; Load treemacs and lsp treemacs configuration

;;; Code:

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :ensure t
  :commands lsp-treemacs-errors-list
  :bind (:map lsp-mode-map
         ("M-9" . lsp-treemacs-errors-list)))

(use-package treemacs
  :ensure t
  :defer 0
  :commands (treemacs)
  :after (lsp-mode))

(provide 'init-treemacs)

;;; init-treemacs.el ends here
