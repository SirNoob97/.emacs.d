;;; init-angular.el --- Set angular language server and client correct path -*- lexical-binding: t -*-

;;; Commentary:
;;; Set angular paths

;;; Code:
;;; init-angular.el --- Set angular correct paths -*- lexical-binding: t -*-

;;; Commentary:
;;; Angular language server and client paths

;;; Code:

(setq lsp-clients-angular-language-server-command
      '("node"
        "/home/martin/.nvm/versions/node/v16.15.0/lib/node_modules/@angular/language-server"
        "--ngProbeLocations"
        "/home/martin/.nvm/versions/node/v16.15.0/lib/node_modules"
        "--tsProbeLocations"
        "/home/martin/.nvm/versions/node/v16.15.0/lib/node_modules"
        "--stdio"))

(provide 'init-angular)

;;; init-angular.el ends here
