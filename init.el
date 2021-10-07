;;; init.el --- Load configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; Load the full configuration, divided in multiple files

;;; Code:

;(setq debug-on-error t)

(setq auto-mode-case-fold nil)

;; don't GC during startup
(setq gc-cons-percentage 0.6
      gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold 800000
		  gc-cons-percentage 0.1)))

;; Custom-file
(defun load-custom-file ()
  "Set `custom-file'."
  (interactive)
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file))

;; Add lisp to load-path
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(update-load-path)
(load-custom-file)

(require 'init-settings)
(require 'init-functions)
(require 'init-keybindings)
(require 'init-emacs-packages)
(require 'init-package)

(require 'init-yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(use-package exec-path-from-shell :ensure t)
(exec-path-from-shell-initialize)

;;; this is for evil mode
(use-package xclip
  :defer 0
  :config (xclip-mode 1))

(require 'init-doom-modeline)
(require 'init-wich-key)
(require 'init-ivy-counsel)
(require 'init-company)
(require 'init-yasnippet)
(require 'init-flycheck)
(require 'init-dap-mode)
(require 'init-treemacs)
(require 'init-lsp-ui)
(require 'init-lsp-mode)
(require 'init-java)
(require 'init-python)

;(defun ansi-colorize-buffer ()
;  "This will help eliminate weird escape sequences during compilation of projects."
;  (let ((buffer-read-only nil))
;    (ansi-color-apply-on-region (point-min) (point-max))))
;
;(use-package ansi-color
;  :ensure t
;  :config
;  (add-hook 'compilation-filter-hook 'ansi-colorize-buffer))

;----------------------


;(use-package dap-mode
;  :ensure t
;  :after (lsp-mode)
;  :functions dap-hydra/nil
;  :config
;  (require 'dap-java)
;  :bind (:map lsp-mode-map
;         ("<f5>" . dap-debug)
;         ("M-<f5>" . dap-hydra))
;  :hook ((dap-mode . dap-ui-mode)
;    (dap-session-created . (lambda (&_rest) (dap-hydra)))
;    (dap-terminated . (lambda (&_rest) (dap-hydra/nil)))))

;(use-package lsp-mode
;  :init
;  (setq lsp-keymap-prefix "C-c l")
;  (setq lsp-headerline-breadcrumb-segments '(file symbols))
;  (setq lsp-prefer-flymake nil)
;  :demand t
;  :bind
;  (:map lsp-mode-map
;        (("M-RET" . lsp-execute-code-action)))
;  :config
;  (setq lsp-inhibit-message t
;	lsp-completion-enable-additional-text-edit nil
;        lsp-eldoc-render-all nil
;        lsp-enable-file-watchers nil
;        lsp-enable-symbol-highlighting nil
;        lsp-headerline-breadcrumb-enable nil
;        lsp-highlight-symbol-at-point nil
;        lsp-modeline-code-actions-enable nil
;        lsp-modeline-diagnostics-enable nil)


;(defun default-java-code-style-hook()
;  (setq c-basic-offset 2
;        c-label-offset 0
;        tab-width 2
;        indent-tabs-mode nil
;        require-final-newline nil))
;(add-hook 'java-mode-hook 'default-java-code-style-hook)
;
;(defun custom-java-mode-hook ()
;  (auto-fill-mode)
;  (flycheck-mode)
;  (subword-mode)
;  (yas-minor-mode)
;  (set-fringe-style '(8 . 0))
;
;  ;; Fix indentation for anonymous classes
;  (c-set-offset 'substatement-open 0)
;  (if (assoc 'inexpr-class c-offsets-alist)
;      (c-set-offset 'inexpr-class 0))
;
;  ;; Indent arguments on the next line as indented body.
;  (c-set-offset 'arglist-intro '++))
;(add-hook 'java-mode-hook 'custom-java-mode-hook)
;
;
;(use-package lsp-java
;  :init
;  (setq lsp-java-vmargs
;        (list
;         "-noverify"
;         "-Xmx3G"
;         "-XX:+UseG1GC"
;         "-XX:+UseStringDeduplication"
;         "-javaagent:/home/martin/.m2/repository/org/projectlombok/lombok/1.18.20/lombok-1.18.20.jar"
;         )
;
;	lsp-file-watch-ignored '(".idea" "node_modules" ".git" ".hg" "build")
;	
;        ;; Don't organise imports on save
;        lsp-java-save-action-organize-imports nil
;
;        ;; Fetch less results from the Eclipse server
;        lsp-java-completion-max-results 20
;
;        lsp-java-java-path "/home/martin/.sdkman/candidates/java/current/bin/java"
;        )
;
;  :config
;  (add-hook 'java-mode-hook #'lsp))
;  :demand t
;  :after (lsp lsp-mode)

;(use-package dap-java :ensure nil
;
;  ;; The :bind here makes use-package fail to lead the dap-java block!
;  ;; :bind
;  ;; (("C-c R" . dap-java-run-test-class)
;  ;;  ("C-c d" . dap-java-debug-test-method)
;  ;;  ("C-c r" . dap-java-run-test-method)
;  ;;  )
;
;  :config
;  (global-set-key (kbd "<f7>") 'dap-step-in)
;  (global-set-key (kbd "<f8>") 'dap-next)
;  (global-set-key (kbd "<f9>") 'dap-continue)
;  )

;----------------------

;;; init.el ends here
