;;; init-python.el --- Load python configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; Load lsp-pyright(at the moment) configuration

;;; Code:

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp)))
  :config
  (setq lsp-pyright-log-level "error"))

;; There's also blacken if you like it better.
(use-package yapfify
  :ensure t
  :defer t
  :hook (python-mode . yapf-mode))

(provide 'init-python)

;;; init-python.el ends here
