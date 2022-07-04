;;; init-treemacs.el --- Load treemacs -*- lexical-binding: t -*-

;;; Commentary:
;;; Load treemacs and lsp treemacs configuration

;;; Code:

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :ensure t)

(use-package treemacs
  :ensure t
  :defer 0
  :bind (:map global-map
              ("C-c t d" . treemacs-delete-other-windows)
              ("C-c t t" . treemacs)
              ("C-c t b" . treemacs-bookmark)
              ("C-c t f" . treemacs-find-file)
	      )
  :config
  (setq treemacs-no-png-images t))

(provide 'init-treemacs)

;;; init-treemacs.el ends here
