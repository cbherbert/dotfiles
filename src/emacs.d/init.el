;;; init.el --- Emacs Configuration File

;;; Commentary:
;;
;; Configuration files are organized in several modules,
;; grouped by general type of features.
;;


;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-packages)

;; core configuration (vanilla emacs settings)
(require 'init-core)

;; configuration for user interface (theme, mode-line, etc)
(require 'init-ui)

;; configuration for general text editing tools
(require 'init-edit)

;; configuration for file management tools (dired, projectile, treemacs)
(require 'init-files)

;; configuration for completion frameworks (vertico, marginalia, orderless, company, yasnippet)
(require 'init-completion)

;; configuration for navigation frameworks (consult, embark)
(require 'init-nav)

;; configuration for programming languages:
(require 'init-code)

;; org-mode configuration:
(require 'init-org)

;;; init.el ends here
