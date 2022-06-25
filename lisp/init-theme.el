;;; init-theme.el --- Load custom emacs theme -*- lexical-binding: t -*-

;;; Commentary:
;;; Doom themes config

;;; Code:

(use-package doom-themes
  :ensure t
  :defer 0
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (doom-themes-treemacs-config))

(provide 'init-theme)

;;; init-theme.el ends here
