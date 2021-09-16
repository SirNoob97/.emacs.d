;;; early-init.el --- Load use-package -*- lexical-binding: t -*-

;;; Commentary:
;;; Load use-package before init.e

;;; Code:
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(setq package-enable-at-startup nil)

(setq frame-inhibit-implied-resize t)

(push '(vertical-scroll-bars) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)

(provide 'early-init)

;;; early-init.el ends here
