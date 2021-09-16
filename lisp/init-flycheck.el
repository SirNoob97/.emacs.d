;;; init-flycheck.el --- Load flycheck -*- lexical-binding: t -*-

;;; Commentary:
;;; Load flycheck

;;; Code:

(use-package flycheck
  :ensure t
  :defer 0
  :init (global-flycheck-mode))

(provide 'init-flycheck)

;;; init-flycheck.el ends here
