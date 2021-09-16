;;; init-dap-mode.el --- Load dap mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; Load dap mode cofiguration and clients

;;; Code:

(use-package dap-mode
  :after lsp-mode
  :bind (:map lsp-mode-map
            ("<f5>" . dap-debug)
            ("M-<f5>" . dap-hydra))
  :config
  (dap-mode t)
  (dap-ui-mode t)
  (dap-tooltip-mode 1)
  (tooltip-mode 1))

(use-package dap-java
  :ensure nil
  :after (lsp-java))

(provide 'init-dap-mode)

;;; init-dap-mode.el ends here
