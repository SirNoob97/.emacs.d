;;; init-doom-modeline.el --- Load modeline -*- lexical-binding: t -*-

;;; Commentary:
;;; Load doom modeline config

;;; Code:

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  ;(setq find-file-visit-truename t) ; to short symlinks
  (setq inhibit-compacting-font-caches t)
  (setq doom-modeline-buffer-file-name-style 'file-name)
  (setq doom-modeline-unicode-fallback nil)
  (setq doom-modeline-gnus nil)
  (setq doom-modeline-irc nil)
  (setq doom-modeline-icon nil))

(provide 'init-doom-modeline)

;;; init-doom-modeline.el ends here
