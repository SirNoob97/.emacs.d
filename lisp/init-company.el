;;; init-company.el --- Load company -*- lexical-binding: t -*-

;;; Commentary:
;;; Load company and its configuration

;;; Code:

(use-package company
  :ensure t
  :defer 0
  :bind (:map company-active-map
              ("TAB" . company-complete-common-or-cycle)
	      ("<backtab>" . company-select-previous))
  :custom
  (company-idle-delay 0.0)
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (company-mode))

(provide 'init-company)

;;; init-company.el ends here
