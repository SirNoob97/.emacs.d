;;; init-java.el --- Load java configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; Load lsp java configuration

;;; Code:

(use-package lsp-java
  :ensure t
  :defer 0
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
  (add-hook 'java-mode-hook 'company-mode))

(provide 'init-java)

;;; init-java.el ends here
