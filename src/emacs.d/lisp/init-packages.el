;;; init-packages.el --- Package Management

;;; Commentary:
;;
;; Package management tools
;;
;; All the settings are managed using use-package.
;; We first make sure it is installed.
;; Other packages are installed through use-package if necessary.
;;
;;;

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-verbose t)
(setq use-package-compute-statistics t)
(setq use-package-enable-imenu-support t)

(use-package try
  :ensure t)

(provide 'init-packages)

;;; init-packages.el ends here
