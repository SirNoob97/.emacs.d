;;; init-emacs-packages.el --- Setup for usefull emacs packages -*- lexical-binding: t -*-

;;; Commentary:
;;; Load the configuration for some Emacs packages

;; Ansi colors
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(defun sn-ansi-colorize-buffer ()
  "This will help eliminate weird escape sequences during compilation of projects."
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'sn-ansi-colorize-buffer)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; reload if file changed on disk
(add-hook 'after-init-hook 'global-auto-revert-mode)

;; column numbers
(add-hook 'after-init-hook 'column-number-mode)
; disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; line numbers
;(setq display-line-numbers 'relative)

;; Eldoc
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'lisp-mode-hook 'eldoc-mode)

;; Electric
(add-hook 'after-init-hook 'electric-indent-mode)
(add-hook 'after-init-hook 'electric-pair-mode)

;; Show paren
(add-hook 'after-init-hook 'show-paren-mode)

;; Tab-bar
(defun sn-tab-bar-select ()
  "If no othert tab exists, create one and switch to it."
  (interactive)
  (let ((tabs (mapcar (lambda (tab)
			(alist-get 'name tab))
		      (tab-bar--tabs-recent))))
    (cond ((eq tabs nil)
	   (tab-new))
	  ((eq (length tabs) 1)
	   (tab-next))
	  (t
	   (tab-bar-switch-to-next-tab
	    (completing-read "Select tab: " tabs nil t))))))
(global-set-key (kbd "C-c t t") 'sn-tab-bar-select)

(with-eval-after-load 'tab-bar-mode
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (tab-bar-history-mode 1))

(provide 'init-emacs-packages)

;;; Code:
;;; init-emacs-packages.el ends here
