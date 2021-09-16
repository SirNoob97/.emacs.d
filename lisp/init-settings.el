;;; init-settings.el --- Load usefull emacs settings -*- lexical-binding: t -*-

;;; Commentary:
;;; Load usefull Emacs settings

;;; Code:

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font Mono-10"))
(load-theme 'wombat t)
(fset 'yes-or-no-p 'y-or-n-p) ;; never have to type full word
(set-default 'truncate-lines t)

(setq-default buffer-file-coding-system 'utf-8-unix)
(setq coding-system-for-read 'utf-8-unix)
(setq coding-system-for-write 'utf-8-unix)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'fundamental-mode)
(setq scroll-step 5)
(setq scroll-margin 2)
(setq scroll-conservatively 5)
(setq scroll-preserve-screen-position t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
(setq auto-save-timeout 5)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t) ;; copy files, don't rename them.
(setq delete-old-versions t)
(setq kept-new-versions 12)
(setq kept-old-versions 12)
(setq select-enable-clipboard t)
(setq select-enable-primary t)
(setq x-select-enable-clipboard-manager nil)
(setq save-interprogram-paste-before-kill t)
(setq backward-delete-char-untabify-method 'all)
(setq create-lockfiles nil)
(setq sentence-end-double-space nil)
(setq require-final-newline t)

(provide 'init-settings)

;;; init-settings.el ends here
