;;; init-company.el --- Load company -*- lexical-binding: t -*-

;;; Commentary:
;;; Load company and its configuration

;;; Code:

(use-package company
  :ensure t
  :defer 0
  :bind (:map global-map
	      ("C-SPC" . company-complete)
	 :map company-active-map
              ("<tab>" . company-select-next)
	      ("<backtab>" . company-select-previous))
  :init
  (setq company-idle-delay 0.0)
  :config
  (global-company-mode)
  )

(provide 'init-company)

;;; init-company.el ends here
