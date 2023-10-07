;;; init-ui.el --- User Interface

;;; Commentary:
;;
;; User Interface configuration
;;

;;; Code:

;;;
;;    Theme and mode line
;;;
(if (display-graphic-p)
    ;; doom-themes do not appear properly in iTerm.
    ;; therefore, we only use it if running with a GUI
    (progn
      (tool-bar-mode -1)
      (use-package all-the-icons
	:ensure t)
      (use-package all-the-icons-ibuffer
	:ensure t
	:hook (ibuffer-mode . all-the-icons-ibuffer-mode))
      (use-package doom-themes
	:ensure t
	:config
	(setq custom-theme-directory (expand-file-name "themes" user-emacs-directory))
	(load-theme 'doom-solarized-dark-custom t)
	(setq custom-file (expand-file-name "custom-gui.el" user-emacs-directory))
	(load custom-file)
	(doom-themes-visual-bell-config)
	)
      (use-package doom-modeline
	:ensure t
	:custom
	(doom-modeline-enable-word-count 1)
	(display-time-default-load-average nil)
	:hook (after-init . doom-modeline-mode)
	:config
	(display-time-mode 1)
	(size-indication-mode)
	)
      (use-package solaire-mode
	:ensure t
	:config
	(solaire-global-mode +1))
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
  :custom-face
  (dashboard-banner-logo-title :inherit 'default)
  :config
  (dashboard-setup-startup-hook))

(provide 'init-ui)

;;; init-ui.el ends here
