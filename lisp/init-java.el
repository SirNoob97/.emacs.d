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

(provide 'init-java)

;;; init-java.el ends here
