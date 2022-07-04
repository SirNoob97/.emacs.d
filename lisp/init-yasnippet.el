;;; init-yasnippet.el --- Load yasnippet and extra snippets -*- lexical-binding: t -*-

;;; Commentary:
;;; Load yasnippet config and future snippets

;;; Code:


(use-package yasnippet
  :ensure t
  :defer 0
  :hook (prog-mode . yas-minor-mode)
  :commands (yas-reload-all))

(use-package yasnippet-snippets :ensure t)

(provide 'init-yasnippet)

;;; init-yasnippet.el ends here
