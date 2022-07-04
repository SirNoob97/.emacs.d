;;; init-company.el --- Load company -*- lexical-binding: t -*-

;;; Commentary:
;;; Load company and its configuration

;;; Code:

(use-package company
  :ensure t
  :defer 0
  :bind (("C-<tab>" . company-complete)
         :map company-active-map
         ("<tab>" . company-select-next)
         ("<backtab>" . company-select-previous))
  :config
  (setq company-idle-delay nil)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-flip-when-above t)
  (setq company-tooltip-align-annotations t)
  (global-company-mode))

(provide 'init-company)

;;; init-company.el ends here
