;;; init-dap-mode.el --- Load dap mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; Load dap mode cofiguration and clients

;;; Code:

(use-package dap-mode
  :after lsp-mode
  :bind (:map lsp-mode-map
	      ("<f2>" . dap-debug)
              ("M-<f2>" . dap-hydra)
	      ("C-c l d b" . dap-ui-breakpoints)
	      :map dap-mode-map
	      ("<f5>" . dap-next)
	      ("<f6>" . dap-step-in)
	      ("<f7>" . dap-step-out)
	      ("<f8>" . dap-continue)
	      ("<f9>" . dap-breakpoint-toggle)
	      )
  :config
  (dap-mode t)
  ;; following only works on gui
  (dap-ui-mode t)
  (dap-tooltip-mode 1)
  (tooltip-mode 1))

(use-package dap-java
  :ensure nil
  :after (lsp-java))

(provide 'init-dap-mode)

;;; init-dap-mode.el ends here
