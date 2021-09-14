;;; init.el --- init setup

;;; Commentary:
;;; Init config

;;; Code:
(add-to-list 'load-path "~/.emacs.d/settings")

(setq inhibit-startup-message t)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)
(set-fringe-mode 0)
(menu-bar-mode 0)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(show-paren-mode 1)
(column-number-mode)
(global-display-line-numbers-mode t)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font Mono-10"))

(load-theme 'wombat t)

(setq scroll-step 4)
(setq scroll-margin 2)
(setq scroll-conservatively 4)
(setq scroll-preserve-screen-position t)

(fset 'yes-or-no-p 'y-or-n-p) ;; never have to type full word
(setq confirm-kill-emacs 'y-or-n-p)

(global-set-key (kbd "<escape>") 'keyboard-scape-quit)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
			  ("org" . "https://orgmode.org/elpa/")
))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package) (package-install 'use-package))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-java-java-path "/home/martin/.sdkman/candidates/java/current/bin/java")
 '(package-selected-packages
   '(lsp-pyright exec-path-from-shell all-the-icons dash doom-modeline company-lsp xclip lsp-ui company evil-collection flycheck lsp-treemacs lsp-ivy which-key helpful ivy-rich counsel dap-mode lsp-java lsp-mode use-package cl-libify undo-tree s)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
(setq auto-save-timeout 5)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t) ;; copy files, don't rename them.
(setq delete-old-versions t)
(setq kept-new-versions 12)
(setq kept-old-versions 12)

(require 'use-package)
(setq use-package-always-ensure t)

(use-package exec-path-from-shell :ensure t)
(exec-path-from-shell-initialize)

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("C-l" . ivy-alt-done)
         ("TAB" . ivy-next-line)
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
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-M-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

;(use-package helpful
;  :commands (helpful-callable helpful-variable helpful-command helpful-key)
;  :custom
;  (counsel-describe-function-function #'helpful-callable)
;  (counsel-describe-variable-function #'helpful-variable)
;  :bind
;  ([remap describe-function] . counsel-describe-function)
;  ([remap describe-command] . helpful-command)
;  ([remap describe-variable] . counsel-describe-variable)
;  ([remap describe-key] . helpful-key))

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


(defun ansi-colorize-buffer ()
  "This will help eliminate weird escape sequences during compilation of projects."
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))

(use-package ansi-color
  :ensure t
  :config
  (add-hook 'compilation-filter-hook 'ansi-colorize-buffer))

;----------------------
(use-package company
  :bind (:map company-active-map
              ("TAB" . company-complete-common-or-cycle)
	      ("<backtab>" . company-select-previous))
  :custom
  (company-idle-delay 0.0)
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (company-mode))

(use-package yasnippet :config (yas-global-mode))
(use-package yasnippet-snippets :ensure t)

(use-package flycheck :ensure t :init (global-flycheck-mode))

(use-package dap-mode
  :after lsp-mode
  :bind (:map lsp-mode-map
            ("<f5>" . dap-debug)
            ("M-<f5>" . dap-hydra))
  :config
  (dap-mode t)
  (dap-ui-mode t)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-register-debug-template
   "localhost:5005"
   (list :type "java"
         :request "attach"
         :hostName "localhost"
         :port 5005))
  )
(use-package dap-java
  :ensure nil
  :after (lsp-java))


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

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :ensure t
  :commands lsp-treemacs-errors-list
  :bind (:map lsp-mode-map
         ("M-9" . lsp-treemacs-errors-list)))

(use-package treemacs
  :ensure t
  :commands (treemacs)
  :after (lsp-mode))

(use-package lsp-ui
  :ensure t
  :requires lsp-mode flycheck
  :bind (:map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references))
  :init
  (setq lsp-ui-peek-always-show t)
  (setq lsp-ui-doc-delay 1.5
	lsp-ui-doc-show-with-cursor nil
	lsp-ui-doc-show-with-mouse nil
	lsp-ui-doc-include-signature t
	lsp-ui-doc-position 'bottom
	lsp-ui-doc-max-width 100)
  (setq lsp-ui-sideline-ignore-duplicate t
	lsp-ui-sideline-show-diagnostics t)
  :config
  (define-key lsp-ui-mode-map (kbd "C-c l k") #'lsp-ui-doc-show)
  (define-key lsp-ui-mode-map (kbd "C-c l s") #'lsp-ui-doc-hide))

(use-package lsp-mode
  :ensure t
  :hook (
   (lsp-mode . lsp-enable-which-key-integration)
   (java-mode . #'lsp-deferred))
  :init
  (setq
    lsp-keymap-prefix "C-c l" ; this is for which-key integration documentation, need to use lsp-mode-map
    lsp-enable-file-watchers nil
    read-process-output-max (* 1024 1024)  ; 1 mb
    lsp-idle-delay 0.500)
  (setq lsp-headerline-breadcrumb-enable nil)
  :config
  (setq lsp-enable-on-type-formatting nil)
  (setq gc-cons-threshold 100000000)
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

(use-package lsp-java
  :ensure t
  :init
  (setq lsp-java-java-path "/home/martin/.sdkman/candidates/java/current/bin/java")
  (setq lsp-java-vmargs
        (list
         "-noverify"
         "-Xmx2G"
         "-XX:+UseG1GC"
         "-XX:+UseStringDeduplication"
         "-javaagent:/home/martin/.m2/repository/org/projectlombok/lombok/1.18.20/lombok-1.18.20.jar"))
  :config
  (add-hook 'java-mode-hook 'lsp)
  (add-hook 'java-mode-hook 'flycheck-mode)
  (add-hook 'java-mode-hook 'company-mode)
  (add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 2
                                  tab-width 2
                                  indent-tabs-mode t))))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp)))
  :config (setq lsp-pyright-typechecking-mode "strict")
  (setq lsp-pyright-log-level "error"))

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

;(use-package lsp-ui
;  :bind (:map lsp-ui-mode-map
;         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
;         ([remap xref-find-references] . lsp-ui-peek-find-references))
;  :init
;  (setq lsp-prefer-flymake nil
;	lsp-ui-doc-position 'bottom
;	lsp-ui-doc-max-width 100
;        lsp-ui-doc-delay 5.0
;        lsp-ui-sideline-enable nil
;        lsp-ui-sideline-show-symbol nil
;	lsp-ui-flycheck-enable t)
;  :after (lsp-mode))

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

;(use-package dap-mode
;  :after lsp-mode
;  :config
;  (dap-mode t)
;  (dap-ui-mode t)
;  (dap-tooltip-mode 1)
;  (tooltip-mode 1)
;  (dap-register-debug-template
;   "localhost:5005"
;   (list :type "java"
;         :request "attach"
;         :hostName "localhost"
;         :port 5005))
;  ;(dap-register-debug-template
;  ; "lxd"
;  ; (list :type "java"
;  ;       :request "attach"
;  ;       :hostName "x.x.x.x"
;  ;       :port 5005))
;  )
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
;
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

(use-package xclip
  :config (xclip-mode 1))

;;; init.el ends here
