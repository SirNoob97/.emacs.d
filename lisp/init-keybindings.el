;;; init-keybindings.el --- Define custom keybindings -*- lexical-binding: t -*-

;;; Commentary:
;;; Define keybindings

;;; Code:

(require 'init-functions)
(global-set-key (kbd "C-c c u") 'sn-convert-to-unix-coding-system)
(global-set-key (kbd "C-c s") 'sn-save-all-buffers-silently)
(global-set-key (kbd "C-c b") 'sn-switch-last-buffer)
(global-set-key (kbd "C-c +") 'sn-increment-number)
(global-set-key (kbd "M-p") 'sn-move-line-up)
(global-set-key (kbd "M-n") 'sn-move-line-down)
(global-set-key (kbd "C-c f d") 'sn-delete-current-file)
(global-set-key (kbd "C-c f s") 'sn-sudoedit)
(global-set-key (kbd "C-c f r") 'sn-rename-current-file)
(global-set-key (kbd "C-c f y") 'sn-copy-file-content)
(global-set-key (kbd "C-c f n") 'sn-copy-file-name)
(global-set-key (kbd "C-c C-r") 'sn-reload-init-file)

(global-set-key (kbd "<escape>") 'keyboard-scape-quit)
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
