;;; init.el --- Load configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; Load the full configuration, divided in multiple files

;;; Code:

;(setq debug-on-error t)

(setq auto-mode-case-fold nil)

;; don't GC during startup
(setq gc-cons-percentage 0.6
      gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold 800000
		  gc-cons-percentage 0.1)))

;; Custom-file
(defun load-custom-file ()
  "Set `custom-file'."
  (interactive)
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file))

;; Add lisp to load-path
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(update-load-path)
(load-custom-file)

(require 'init-settings)
(require 'init-functions)
(require 'init-keybindings)
(require 'init-emacs-packages)
(require 'init-package)
(require 'init-theme)

(require 'init-yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(use-package iedit
  :ensure t
  :defer 0
  :config (iedit-mode 1))

(use-package exec-path-from-shell :ensure t)
(exec-path-from-shell-initialize)

(use-package xclip
  :defer 0
  :config (xclip-mode 1))

(require 'init-doom-modeline)
(require 'init-wich-key)
(require 'init-ivy-counsel)
(require 'init-groovy-mode)
(require 'init-company)
(require 'init-yasnippet)
(require 'init-flycheck)
(require 'init-dap-mode)
(require 'init-treemacs)
(require 'init-lsp-ui)
(require 'init-lsp-mode)
(require 'init-java)
(require 'init-typescript)
(require 'init-angular)
(require 'init-python)

;(defun ansi-colorize-buffer ()
;  "This will help eliminate weird escape sequences during compilation of projects."
;  (let ((buffer-read-only nil))
;    (ansi-color-apply-on-region (point-min) (point-max))))
;
;(use-package ansi-color
;  :ensure t
;  :config
;  (add-hook 'compilation-filter-hook 'ansi-colorize-buffer))

;;; init.el ends here
