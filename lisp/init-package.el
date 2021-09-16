;;; init-package.el --- Load use-package -*- lexical-binding: t -*-

;;; Commentary:
;;; Load melpa and use-package setup

;;; Code:

(require 'package)
(require 'cl-lib)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("elpa" . "https://elpa.gnu.org/packages/")
			   ("org" . "https://orgmode.org/elpa/")
			   ))
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enablen-imenu-support t))

(eval-and-compile
  (require 'use-package)
  (require 'bind-key))

(use-package diminish)

(provide 'init-package)

;;; init-package.el ends here
