;;; init-python.el --- Load python configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; Load lsp-pyright(at the moment) configuration

;;; Code:

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp)))
  :config (setq lsp-pyright-typechecking-mode "strict")
  (setq lsp-pyright-log-level "error"))

(provide 'init-python)

;;; init-python ends here
