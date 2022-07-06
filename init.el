(setq gc-cons-percentage 0.6
      gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
    (lambda ()
      (setq gc-cons-threshold 800000
      gc-cons-percentage 0.1)))

(defun load-custom-file ()
  "Set `custom-file'."
  (interactive)
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file))

(load-custom-file)

(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(update-load-path)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq coding-system-for-read 'utf-8-unix)
(setq coding-system-for-write 'utf-8-unix)

(add-to-list 'default-frame-alist '(font . "Hack Nerd Font Mono-11"))
(fset 'yes-or-no-p 'y-or-n-p) ;; never have to type full word
(windmove-default-keybindings 'meta)
(set-default 'truncate-lines t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq initial-scratch-message nil)
(setq initial-major-mode 'fundamental-mode)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq sentence-end-double-space nil)
(setq require-final-newline t)
(setq help-window-select t)

(setq select-enable-clipboard t)
(setq select-enable-primary t)
(setq x-select-enable-clipboard-manager nil)
(setq save-interprogram-paste-before-kill t)

(setq backward-delete-char-untabify-method 'all)
(setq-default c-basic-offset 2)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(global-display-line-numbers-mode t)
;(setq-default display-line-numbers-type 'relative)
; disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

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

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "M-;") 'comment-line) ; comment-dwin by default
(global-set-key (kbd "C-M-;") 'comment-dwim) ; unbound by default
(global-set-key (kbd "C-c e b") 'eval-buffer)
(global-set-key (kbd "C-c e f") 'eval-defun)
(global-set-key (kbd "C-c n") 'next-buffer)
(global-set-key (kbd "C-c p") 'previous-buffer)

(global-set-key (kbd "M-\"") 'sn-surround-with-quotes)
(global-set-key (kbd "M-'") 'sn-surround-with-single-quotes)
(global-set-key (kbd "M-`") 'sn-surround-with-backquotes)

(add-hook 'compilation-filter-hook 'sn-ansi-colorize-buffer)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; reload if file changed on disk
(add-hook 'after-init-hook 'global-auto-revert-mode)

;; Eldoc
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'lisp-mode-hook 'eldoc-mode)

;; Electric
(add-hook 'after-init-hook 'electric-indent-mode)
(add-hook 'after-init-hook 'electric-pair-mode)

;; Show paren
(add-hook 'after-init-hook 'show-paren-mode)

(require 'package)
(require 'cl-lib)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ))
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)					; To prevent initializing twice
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

(use-package spacemacs-theme
  :ensure t
  :defer 0)

(use-package ample-theme
  :ensure t
  :defer 0)

(use-package dracula-theme
  :ensure t
  :defer 0)

(load-theme 'dracula)

(use-package iedit
  :ensure t
  :defer 0)

(use-package exec-path-from-shell :ensure t)
(exec-path-from-shell-initialize)

(use-package xclip
  :defer 0
  :config (xclip-mode 1))

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

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         ("<backtab>" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-l" . ivy-done)
         ("<backtab>" . ivy-previous-line)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :defer 0
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-M-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode))

(use-package company
  :ensure t
  :defer 0
  :bind (("C-<tab>" . company-complete)
         :map company-active-map
         ("<tab>" . company-select-next)
         ("<backtab>" . company-select-previous))
  :config
  (setq company-idle-delay nil)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-flip-when-above t)
  (setq company-tooltip-align-annotations t)
  (global-company-mode))

(use-package yasnippet
  :ensure t
  :defer 0
  :hook (prog-mode . yas-minor-mode)
  :commands (yas-reload-all))

(use-package flycheck
  :ensure t
  :defer 0
  :bind (:map flycheck-mode-map
   ("M-]" . flycheck-next-error)
   ("M-[" . flycheck-previous-error)
   )
  :init
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  :hook (after-init . global-flycheck-mode))

(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
               (display-buffer-reuse-window
               display-buffer-in-side-window)
              (side						 . bottom)
              (reusable-frames . visible)
              (window-height	 . 0.25)))

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :ensure t)

(use-package treemacs
  :ensure t
  :defer 0
  :bind (:map global-map
              ("C-c t d" . treemacs-delete-other-windows)
              ("C-c t t" . treemacs)
              ("C-c t b" . treemacs-bookmark)
              ("C-c t f" . treemacs-find-file)
        )
  :config
  (setq treemacs-no-png-images t))

(use-package dap-mode
  :after lsp-mode
  :bind (:map lsp-mode-map
        ("<f2>" . dap-debug)
              ("M-<f2>" . dap-hydra)
        ("C-c l d b" . dap-ui-breakpoints)
        :map dap-mode-map
        ("<f5>" . dap-next)
        ("<f6>" . dap-step-in)
        ("<f7>" . dap-step-out)
        ("<f8>" . dap-continue)
        ("<f9>" . dap-breakpoint-toggle)
        )
  :config
  (dap-mode t)
  ;; following only works on gui
  (dap-ui-mode t)
  (dap-tooltip-mode 1)
  (tooltip-mode 1))

(use-package lsp-ui
  :ensure t
  :defer 0
  :bind (:map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references))
  :init
  (setq lsp-ui-peek-always-show t)
  (setq	lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-delay 1.5)
  (setq	lsp-ui-doc-include-signature t)
  (setq	lsp-ui-doc-max-width 100)
  (setq	lsp-ui-doc-show-with-cursor nil)
  (setq	lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-sideline-ignore-duplicate t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-code-actions nil)
  :config
  (define-key lsp-ui-mode-map (kbd "C-c l k") 'lsp-ui-doc-show)
  (define-key lsp-ui-mode-map (kbd "C-c l s") 'lsp-ui-doc-hide))

(use-package lsp-mode
  :ensure t
  :defer 0
  :bind
  (:map lsp-mode-map
        ("M-RET" . lsp-execute-code-action)
        ("C-c l f b" . lsp-format-buffer)
        ("C-c l f r" . lsp-format-region))
  :hook (
         (lsp-mode . lsp-enable-which-key-integration)
         (java-mode . #'lsp-deferred))
  :init
  (setq lsp-keymap-prefix "C-c l") ; this is for which-key integration documentation, need to use lsp-mode-map
  (setq lsp-enable-file-watchers nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-lens-enable nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-modeline-diagnostics-enable nil) ; flycheck handle this
  (setq lsp-log-io nil)
  (setq read-process-output-max (* 1024 1024))	; 1 mb
  (setq lsp-idle-delay 0.500)
  (setq gc-cons-threshold 100000000)
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

(use-package lsp-java
  :ensure t
  :defer 0
  :init
  (setq lsp-java-java-path "/home/martin/.sdkman/candidates/java/current/bin/java")
  (setq lsp-java-vmargs
        (list
         "-Declipse.application=org.eclipse.jdt.ls.core.id1"
         "-Dosgi.bundles.defaultStartLevel=4"
         "-Declipse.product=org.eclipse.jdt.ls.core.product"
         "-Dlog.protocol=true"
         "-Dlog.level=ALL"
         "-noverify"
         "-Xmx1G"
         "-XX:+UseG1GC"
         "-XX:+UseStringDeduplication"
         "-javaagent:/home/martin/.m2/repository/org/projectlombok/lombok/1.18.24/lombok-1.18.24.jar"))
  (setq lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")
  (setq lsp-java-content-provider-preferred "fernflower")
  (setq lsp-java-configuration-runtimes '[(:name "Java 11" :path "/home/martin/.sdkman/candidates/java/11.0.15-tem")
                                          (:name "Java 17" :path "/home/martin/.sdkman/candidates/java/17.0.3-tem")
                                          ])
  :config
  (add-hook 'java-mode-hook 'lsp)
  (add-hook 'java-mode-hook 'flycheck-mode)
  (add-hook 'java-mode-hook 'company-mode))

(use-package dap-java
  :ensure nil
  :after (lsp-java))

(use-package web-mode)

(use-package typescript-mode
  :ensure t
  :defer 0
  :config
  (add-hook 'typescript-mode-hook 'lsp)
  (setq typescript-indent-level 2))

(setq lsp-clients-angular-node-get-prefix-command 'nil)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp)))
  :config
  (setq lsp-pyright-log-level "error"))

;; There's also blacken if you like it better.
(use-package yapfify
  :ensure t
  :defer t
  :hook (python-mode . yapf-mode))
