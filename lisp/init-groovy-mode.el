;;; init-groovy-mode.el --- Load groovy mode for groovy files and gradle files -*- lexical-binding: t -*-

;;; Commentary:
;;; Load groovy mode

;;; Code:

(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))
 
;;; make Groovy mode electric by default.
(add-hook 'groovy-mode-hook
          '(lambda ()
             (require 'groovy-electric)
             (groovy-electric-mode)))

(provide 'init-groovy-mode)

;;; init-groovy-mode.el ends here
