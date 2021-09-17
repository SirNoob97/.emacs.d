;;; init-emacs-packages.el --- Setup for usefull emacs packages -*- lexical-binding: t -*-

;;; Commentary:
;;; Load the configuration for some Emacs packages

;;; Code:

;; Ansi colors
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(defun sn-ansi-colorize-buffer ()
  "This will help eliminate weird escape sequences during compilation of projects."
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'sn-ansi-colorize-buffer)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; reload if file changed on disk
(add-hook 'after-init-hook 'global-auto-revert-mode)

;; Eldoc
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'lisp-mode-hook 'eldoc-mode)

;; Electric
(add-hook 'after-init-hook 'electric-indent-mode)
(add-hook 'after-init-hook 'electric-pair-mode)

;; Show paren
(add-hook 'after-init-hook 'show-paren-mode)

(provide 'init-emacs-packages)

;;; init-emacs-packages.el ends here
