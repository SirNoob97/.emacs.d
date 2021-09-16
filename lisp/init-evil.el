;;; init-evil.el --- Load vim mode -*- lexical-binding: t -*-

;;; Commentary:
;;; Load vim mode

;;; Code:

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

(provide 'init-evil)

;;; init-evil.el ends here
