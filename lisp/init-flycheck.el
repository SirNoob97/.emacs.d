;;; init-flycheck.el --- Load flycheck -*- lexical-binding: t -*-

;;; Commentary:
;;; Load flycheck

;;; Code:

(use-package flycheck
  :ensure t
  :defer 0
  :hook (after-init . global-flycheck-mode)
  :init
  (setq flycheck-check-syntax-automatically '(mode-enabled save)))

(provide 'init-flycheck)

;;; init-flycheck.el ends here
