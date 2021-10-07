;;; init-flycheck.el --- Load flycheck -*- lexical-binding: t -*-

;;; Commentary:
;;; Load flycheck

;;; Code:

(use-package flycheck
  :ensure t
  :defer 0
  :bind (:map flycheck-mode-map
	 ("M-]" . flycheck-next-error)
	 ("M-[" . flycheck-previous-error)
	 )
  :hook (after-init . global-flycheck-mode))

(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
               (display-buffer-reuse-window
               display-buffer-in-side-window)
              (side            . bottom)
              (reusable-frames . visible)
              (window-height   . 0.25)))

(provide 'init-flycheck)

;;; init-flycheck.el ends here
