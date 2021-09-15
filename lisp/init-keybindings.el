;;; init-keybindings.el --- Define custom keybindings -*- lexical-binding: t -*-

;;; Commentary:
;;; Define keybindings

;;; Code:

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "M-;") 'comment-line) ; comment-dwin by default
(global-set-key (kbd "C-M-;") 'comment-dwim) ; unbound by default
(global-set-key (kbd "C-c e b") 'eval-buffer)
(global-set-key (kbd "C-c e f") 'eval-defun)
(global-set-key (kbd "C-c n") 'next-buffer)
(global-set-key (kbd "C-c p") 'previous-buffer)

(provide 'init-keybindings)

;;; init-keybindings.el ends here
