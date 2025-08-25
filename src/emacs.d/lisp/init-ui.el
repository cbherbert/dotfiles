;;; init-ui.el --- User Interface

;;; Commentary:
;;
;; User Interface configuration
;;

;;; Code:

;;;
;;; Theme
;;;
(if (display-graphic-p)
    ;; doom-themes do not appear properly in iTerm.
    ;; therefore, we only use it if running with a GUI
    (progn
      (tool-bar-mode -1)
      (setq custom-theme-directory (expand-file-name "themes" user-emacs-directory))
      (setq custom-file (expand-file-name "custom-gui.el" user-emacs-directory))
      (load custom-file)
      (defun ch/disable-all-themes (orig-fun &rest r)
	"Disable all currently enabled themes, as defined by `custom-enabled-themes'."
	(mapc #'disable-theme custom-enabled-themes)
	)
      (advice-add 'load-theme :before #'ch/disable-all-themes)
      (defun custom-theme-faces-upon-enable (theme)
	(when (eq theme 'catppuccin)
	  (custom-theme-faces-upon-enable-catppuccin)
	  )
	(when (eq theme 'zenburn)
	  (custom-theme-faces-upon-enable-zenburn)
	  )
	(when (eq theme 'doom-solarized-dark-custom)
	  (custom-theme-faces-upon-enable-solarized)
	  )
	)
      (add-hook 'enable-theme-functions #'custom-theme-faces-upon-enable)
      (use-package doom-themes
	:ensure t
	:config
	(load-theme 'doom-solarized-dark-custom t)
	(doom-themes-visual-bell-config)
	)
      (use-package doom-solarized-dark-custom-theme
	:after all-the-icons
	:preface
	(defun custom-theme-faces-upon-enable-solarized ()
	  (let ((custom--inhibit-theme-enable nil))
	    (custom-theme-set-faces
	     'doom-solarized-dark-custom
	     '(vertico-current ((t (:foreground "#859900"))))
	     ;;`(vertico-current ((t (:foreground ,(face-foreground 'success) :background ,(face-background 'region) :extend: t))))
	     ;; '(calendar-today :foreground violet)
	     ;; '(consult-bookmark :inherit 'bookmark-menu-bookmark)
	     ;; '(dashboard-banner-logo-title :inherit 'default)
	     ;; `(epa-validity-high ((t (:foreground ,(face-foreground 'success)))))
	     ;; `(epa-validity-medium ((t (:foreground ,(face-foreground 'warning) :slant normal))))
	     ;; `(epa-validity-disabled ((t (:foreground ,(face-foreground 'error) :inverse-video nil))))
	     ;; `(flycheck-error-list-highlight ((t (:weight bold :background ,(face-background 'region)))))
	     ;; `(lsp-face-highlight-textual ((t (:foreground ,(face-foreground 'all-the-icons-lsilver)))))
	     ;; `(mu4e-highlight-face ((t (:inherit default :weight bold :foreground ,(face-foreground 'success)))))
	     ;; '(mu4e-header-face ((t (:inherit font-lock-comment-face))))
	     ;; '(mu4e-header-key-face ((t (:inherit gnus-header-content))))
	     ;; '(mu4e-related-face ((t (:inherit mu4e-header-face :slant normal))))
	     ;; '(mu4e-forwarded-face ((t (:inherit mu4e-header-face :slant normal :weight normal))))
	     ;; '(mu4e-replied-face ((t (:inherit mu4e-header-face :slant normal :weight normal))))
	     ;; '(mu4e-unread-face ((t (:inherit default :weight bold))))
	     ;; `(mu4e-header-highlight-face ((t (:inherit hl-line :weight bold :extend t :underline t :foreground ,(face-foreground 'success)))))
	     ;; '(mu4e-thread-fold-face ((t (:inherit which-key-group-description-face))))
	     ;; '(org-latex-and-related :inherit 'font-latex-math-face)
	     ;; `(org-super-agenda-header ((t (:foreground ,(face-foreground 'calendar-today)))))
	     )
	    )
	  )
	)
      (use-package catppuccin-theme
	:custom
	(catppuccin-flavor 'frappe)
	:config
	(defun custom-theme-faces-upon-enable-catppuccin ()
	  (let ((custom--inhibit-theme-enable nil))
	    (custom-theme-set-faces
	     'catppuccin
	     `(vertico-current ((t (:foreground ,(catppuccin-color 'green))))))
	    )
	  )
	)
      (use-package zenburn-theme
	:preface
	(setq ch/zenburn-color-alist '((fg . "#DCDCCC") (bg . "#1C1C1C") (green . "#5F7F5F") (cyan . "#93E0E3")))
	:config
	(defun custom-theme-faces-upon-enable-zenburn ()
	  (let ((custom--inhibit-theme-enable nil))
	    (custom-theme-set-faces
	     'zenburn
	     `(vertico-current ((t (:foreground ,(alist-get 'green ch/zenburn-color-alist))))))
	    )
	  )
	)
      (when (eq system-type 'darwin)
	(set-face-attribute 'default nil :family "Hack")
	;; default font size (point * 10)
	;;
	;; WARNING!  Depending on the default font,
	;; if the size is not supported very well, the frame will be clipped
	;; so that the beginning of the buffer may not be visible correctly.
	(set-face-attribute 'default nil :height 141))
      )
  ;; otherwise, fall back to the 'old' solarized theme
      (use-package color-theme-solarized
	:ensure t
	:custom
	(frame-background-mode 'dark)
	:config
	(load-theme 'solarized t)
	;; Show current function name in mode-line
	(which-function-mode 1)
	(setq custom-file "~/.emacs.d/custom-terminal.el")
	(load custom-file)
	))
;;;
;;; Modeline
;;;

(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-enable-word-count 1)
  (doom-modeline-minor-modes t)
  (doom-modeline-major-mode-icon (display-graphic-p))
  (doom-modeline-time-icon (display-graphic-p))
  (display-time-default-load-average nil)
  :hook (after-init . doom-modeline-mode)
  :config
  (display-time-mode 1)
  (line-number-mode -1)
  (column-number-mode -1)
  (when (eq system-type 'darwin) (setq doom-modeline-mu4e t))
  )

(use-package minions
  :ensure t
  :if (display-graphic-p)
  :config (minions-mode 1))

;;;
;;; Icons
;;;

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-ibuffer
  :ensure t
  :if (display-graphic-p)
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

;;;
;;; Buffer appearance
;;;

(use-package solaire-mode
  :ensure t
  :if (display-graphic-p)
  :config
  (solaire-global-mode +1))

(use-package dimmer
  :ensure t
  :config
  (dimmer-configure-which-key)
  (dimmer-configure-hydra)
  (dimmer-configure-magit)
  (dimmer-configure-org)
  (dimmer-mode t))

;;;
;;; Dashboard
;;;

(use-package dashboard
  :ensure t
  :after all-the-icons
  :if (display-graphic-p)
  :custom
  (dashboard-banner-logo-title (concat "GNU Emacs " emacs-version))
  (dashboard-projects-backend 'projectile)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-center-content t)
  (dashboard-set-init-info t)
  (dashboard-items '((recents  . 10) (bookmarks . 10) (projects . 10)))
  (dashboard-week-agenda nil)
  (dashboard-set-navigator t)
  (dashboard-navigator-buttons `((
				  (,(when (display-graphic-p)
				      (all-the-icons-material "restore" :height 1.35 :v-adjust -0.24))
				   "Restore" "Restore previous session"
				   (lambda (&rest _) (recover-session)))
				  (,(when (display-graphic-p)
				      (all-the-icons-octicon "tools" :height 1.0 :v-adjust 0.0))
				   "Settings" "Open custom file"
				   (lambda (&rest _) (find-file custom-file)))
				  (,(when (display-graphic-p)
				      (all-the-icons-fileicon "lisp" :height 1.0 :v-adjust -0.1))
				   "Scratch" "Open scratch buffer (lisp interpreter)"
				   (lambda (&rest _) (switch-to-buffer "*scratch*")))
				  (,(when (display-graphic-p)
				      (all-the-icons-faicon "question" :height 1.2 :v-adjust -0.1)
				      "?")
				   "Info" "Browse Manuals"
				   (lambda (&rest _) (info)))
				  )))
  :config
  (dashboard-setup-startup-hook))

(provide 'init-ui)

;;; init-ui.el ends here
