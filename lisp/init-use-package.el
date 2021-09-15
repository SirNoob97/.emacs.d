;;; init-use-package.el --- Load use-package -*- lexical-binding: t -*-

;;; Commentary:
;;; Load melpa and use-package setup

;;; Code:

(require 'package)
(require 'cl-lib)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("elpa" . "https://elpa.gnu.org/packages/")
			   ("org" . "https://orgmode.org/elpa/")
			   ))
(setq package-enable-at-startup nil)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enablen-imenu-support t))

(eval-when-compile (require 'use-package))

(use-package diminish)
(use-package bind-key)

(defun require-use-package()
  "Solving bug."
  (interactive)
  (require 'use-package))

(provide 'init-use-package)

;;; init-use-package.el ends here
