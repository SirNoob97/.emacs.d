;;; init-wich-key.el --- Load which key configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; Load wich key package and configuration

;;; Code:

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode))

(provide 'init-wich-key)

;;; init-wich-key.el ends here
