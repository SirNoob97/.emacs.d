;;; init-typescript.el --- Load typescript setup -*- lexical-binding: t -*-

;;; Commentary:
;;; Load company and its configuration

;;; Code:

(use-package typescript-mode
  :ensure t
  :defer 0
  :config
  (add-hook 'typescript-mode-hook 'lsp)
  (setq typescript-indent-level 2))

(setq lsp-clients-angular-node-get-prefix-command 'nil)

(provide 'init-typescript)

;;; init-typescript.el ends here
