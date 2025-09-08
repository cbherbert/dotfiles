;;; init-ui.el --- User Interface

;;; Commentary:
;;
;; User Interface configuration
;;

;;; Code:

(use-package custom
  :custom
  (custom-theme-directory (expand-file-name "themes" user-emacs-directory))
  (custom-file (expand-file-name (if (display-graphic-p) "custom-gui.el" "custom-terminal.el") user-emacs-directory))
  :config
  (load custom-file t)
  )

;;;
;;; Theme
;;;
;;;
;;; Theme customization and switching
;;;
;; Below is some code to customize some faces easily for different themes
;; and for smooth theme switching. In particular I want to disable all
;; themes before enabling a new one to avoid any side effects and to
;; apply my custom face definitions each time the theme is enabled.
;;
;; Related approaches can be found online:
;; https://github.com/alphapapa/unpackaged.el#customize-theme-faces
;; https://www.unwoundstack.com/blog/switching-emacs-themes.html
;; https://emacsredux.com/blog/2025/02/13/customizing-color-themes/
(defun ch/disable-all-themes (orig-fun &rest r)
  "Disable all currently enabled themes, as defined by `custom-enabled-themes'."
  (mapc #'disable-theme custom-enabled-themes)
  )
(advice-add 'load-theme :before #'ch/disable-all-themes)
(defvar ch/theme-faces-customization nil
  "Association list storing all the custom faces for each theme I use.")
(defun ch/define-theme-faces-customization (theme &rest faces)
  "Store custom faces to be applied each time the theme is enabled."
  (setq ch/theme-faces-customization (assq-delete-all theme ch/theme-faces-customization))
  (push (cons theme (list faces)) ch/theme-faces-customization))
(defun ch/apply-theme-faces-customization (theme)
  "Apply custom faces for given theme."
  (let ((custom--inhibit-theme-enable nil)
	(faces (car (alist-get theme ch/theme-faces-customization))))
    (apply #'custom-theme-set-faces theme faces)
    )
  )
(add-hook 'enable-theme-functions #'ch/apply-theme-faces-customization)
;; To make sure that the face specifications for custom faces in
;; `doom-solarized-dark' are correct (and because they rely on existing
;; faces through `face-foreground' and `face-background'), I need to load
;; twice: first before defining the face customizations and a second time
;; after they are defined to apply them.  Alternatively I could define all
;; faces directly.
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-solarized-dark t)
  (doom-themes-visual-bell-config)
  )
(use-package doom-solarized-dark-theme
  :after all-the-icons doom-themes
  :config
  (ch/define-theme-faces-customization
   'doom-solarized-dark
   ;; some basic faces
   `(vertico-current ((t (:foreground ,(face-foreground 'success) :background ,(face-background 'region) :extend: t))))
   ;; an issue is that vertico-current is overriden by faces like
   ;; consult-file or consult-bookmark for instance in the consult-buffer
   ;; command. This also affects consult-project-buffer,
   ;; consult-outline. The color is also the same as the one used by
   ;; consult-grep and consult-ripgrep to show matches, and the same as
   ;; key-bindings in M-x.
   `(bookmark-menu-bookmark ((t (:weight bold :foreground ,(doom-color 'magenta)))))
   `(calendar-today ((t (:foreground ,(doom-color 'violet)))))
   `(show-paren-match ((t (:foreground ,(doom-color 'magenta) :background ,(face-background 'default)))))
   '(consult-bookmark ((t (:inherit 'bookmark-menu-bookmark))))
   `(completions-common-part ((t (:foreground ,(doom-color 'magenta) :background ,(doom-color 'base0)))))
   `(help-key-binding ((t (:inherit 'fixed-pitch :background ,(doom-color 'bg-alt) :foreground ,(doom-color 'violet)
				    :box (:line-width 1 :color ,(doom-color 'base5))))))
   '(dashboard-banner-logo-title ((t (:inherit 'default))))
   `(doom-modeline-host ((t (:foreground ,(doom-color 'magenta)))))
   `(markdown-code-face ((t (:background ,(doom-color 'bg-alt) :extend t))))
   ;; dired
   `(dired-directory ((t (:foreground ,(doom-color 'violet) :weight bold))))
   `(dired-symlink ((t (:foreground ,(doom-color 'magenta)))))
   `(dired-special ((t (:foreground ,(doom-color 'cyan)))))
   `(dired-marked ((t (:foreground ,(doom-color 'red)))))
   `(dired-broken-symlink ((t (:background ,(doom-color 'orange) :foreground "#fff6e3"))))
   `(all-the-icons-dired-dir-face ((t (:foreground ,(doom-color 'cyan)))))
   ;; GPG
   `(epa-validity-high ((t (:foreground ,(face-foreground 'success)))))
   `(epa-validity-medium ((t (:foreground ,(face-foreground 'warning) :slant normal))))
   `(epa-validity-disabled ((t (:foreground ,(face-foreground 'error) :inverse-video nil))))
   ;; flycheck
   `(flycheck-error-list-line-number ((t (:foreground ,(doom-color 'violet)))))
   `(flycheck-error-list-column-number ((t (:foreground ,(doom-color 'violet)))))
   `(flycheck-error-list-filename ((t (:foreground ,(doom-color 'cyan)))))
   `(flycheck-error-list-highlight ((t (:weight bold :background ,(face-background 'region)))))
   ;; latex
   `(font-latex-sectioning-2-face ((t (:inherit 'font-latex-sectioning-3-face :foreground ,(doom-color 'orange) :height 1.1))))
   `(font-latex-sectioning-3-face ((t (:inherit 'font-latex-sectioning-4-face :foreground ,(doom-color 'yellow) :height 1.1))))
   `(font-latex-sectioning-4-face ((t (:inherit 'font-latex-sectioning-5-face :foreground ,(doom-color 'blue) :height 1.1))))
   ;; lsp
   `(lsp-face-highlight-textual ((t (:foreground ,(face-foreground 'all-the-icons-lsilver)))))
   `(lsp-installation-buffer-face ((t (:foreground ,(doom-color 'green)))))
   `(lsp-ui-sideline-current-symbol ((t (:foreground ,(doom-color 'red) :box (:line-width 1 :color ,(doom-color 'red)) :height 0.99))))
   `(lsp-ui-sideline-symbol-info ((t (:inherit 'default :foreground ,(doom-color 'green)))))
   `(lsp-ui-doc-highlight-hover ((t (:background ,(doom-color 'bg-alt)))))
   ;; magit
   '(magit-header-line ((t (:foreground "#d75f00" :background "#eee8d5" :weight bold
					:box (:line-width 3 :color "#93a1a1")))))
   `(magit-diff-file-heading ((t (:foreground ,(doom-color 'yellow) :weight bold))))
   `(magit-branch-local ((t (:foreground ,(doom-color 'blue)))))
   `(magit-branch-remote ((t (:foreground ,(doom-color 'magenta)))))
   ;; mu4
   `(mu4e-highlight-face ((t (:inherit default :weight bold :foreground ,(face-foreground 'success)))))
   ;; perhaps better to customize `highlight' instead and inherit from it as per default?
   '(mu4e-header-face ((t (:inherit font-lock-comment-face))))
   '(mu4e-header-key-face ((t (:inherit gnus-header-content))))
   '(mu4e-related-face ((t (:inherit mu4e-header-face :slant normal))))
   '(mu4e-forwarded-face ((t (:inherit mu4e-header-face :slant normal :weight normal))))
   '(mu4e-replied-face ((t (:inherit mu4e-header-face :slant normal :weight normal))))
   '(mu4e-unread-face ((t (:inherit default :weight bold))))
   `(mu4e-header-highlight-face ((t (:inherit hl-line :weight bold :extend t :underline t :foreground ,(face-foreground 'success)))))
   '(mu4e-thread-fold-face ((t (:inherit which-key-group-description-face))))
   ;; orderless
   `(orderless-match-face-0 ((t (:foreground ,(doom-color 'magenta) :background ,(doom-color 'base0)))))
   `(orderless-match-face-1 ((t (:foreground ,(doom-color 'cyan) :background ,(doom-color 'base0)))))
   `(orderless-match-face-2 ((t (:foreground ,(doom-color 'yellow) :background ,(doom-color 'base0)))))
   `(orderless-match-face-3 ((t (:foreground ,(doom-color 'green) :background ,(doom-color 'base0)))))
   ;; org mode
   `(org-mode-line-clock ((t (:foreground ,(doom-color 'green)))))
   '(org-mode-line-clock-overrun ((t (:foreground "#d75f00" :weight bold))))
   `(org-scheduled-previously ((t (:foreground ,(doom-color 'red) :weight normal))))
   `(org-scheduled-today ((t (:foreground ,(doom-color 'yellow) :weight normal))))
   `(org-date-selected ((t (:background ,(doom-color 'yellow) :foreground "#fff6e3"))))
   '(org-latex-and-related ((t (:inherit 'font-latex-math-face))))
   '(org-meta-line ((t (:inherit 'org-drawer))))
   '(org-special-keyword ((t (:inherit 'org-drawer))))
   `(org-super-agenda-header ((t (:foreground ,(doom-color 'violet)))))
   ;; org-roam
   `(org-roam-header-line ((t (:foreground ,(doom-color 'violet) :weight bold))))
   `(org-roam-title ((t (:foreground ,(doom-color 'magenta) :weight bold))))
   `(org-roam-olp ((t (:foreground ,(doom-color 'base1)))))
   ;; transient
   `(transient-key-exit ((t (:foreground ,(doom-color 'red)))))
   `(transient-key-stay ((t (:foreground ,(doom-color 'cyan)))))
   `(transient-key-stack ((t (:foreground ,(doom-color 'magenta)))))
   `(transient-key-return ((t (:foreground ,(doom-color 'violet)))))
   `(transient-key-recurse ((t (:foreground ,(doom-color 'blue)))))
   `(transient-key-noop ((t (:foreground ,(doom-color 'base5)))))
   ;; treemacs
   '(treemacs-git-modified-face ((t (:inherit magit-diff-file-heading))))
   '(treemacs-git-untracked-face ((t (:inherit magit-filename))))
   )
  (load-theme 'doom-solarized-dark t)
  )
(use-package catppuccin-theme
  :if (display-graphic-p)
  :custom
  (catppuccin-flavor 'frappe)
  :config
  (ch/define-theme-faces-customization
   'catppuccin
   `(company-tooltip-common ((t (:foreground ,(catppuccin-color 'blue) :weight bold))))
   `(company-tooltip-common-selection ((t (:inherit company-tooltip-common))))
   `(custom-state ((t (:foreground ,(catppuccin-color 'green)))))
   `(diff-context ((t (:foreground ,(catppuccin-color 'overlay0)))))
   `(diff-file-header ((t (:foreground ,(catppuccin-color 'sapphire)))))
   `(dired-marked ((t (:foreground ,(catppuccin-color 'maroon) :weight bold))))
   `(doom-modeline-host ((t (:foreground ,(catppuccin-color 'pink)))))
   '(elfeed-search-title-face ((t (:inherit font-lock-comment-face))))
   `(elfeed-search-feed-face ((t (:foreground ,(catppuccin-color 'blue) :weight bold))))
   `(elfeed-search-last-update-face ((t (:foreground ,(catppuccin-color 'lavender) :weight bold))))
   `(epa-validity-high ((t (:foreground ,(face-foreground 'success)))))
   `(epa-validity-medium ((t (:foreground ,(face-foreground 'warning) :slant normal))))
   `(epa-validity-disabled ((t (:foreground ,(face-foreground 'error) :inverse-video nil))))
   `(mode-line-emphasis ((t (:foreground ,(catppuccin-color 'sky)))))
   `(vertico-current ((t (:foreground ,(catppuccin-color 'green) :background ,(face-background 'region) :extend: t))))
   `(flycheck-info  ((t (:underline  (:style wave :color ,(catppuccin-color 'green))))))
   `(flycheck-warning  ((t (:underline  (:style wave :color ,(catppuccin-color 'peach))))))
   `(flycheck-error  ((t (:underline  (:style wave :color ,(catppuccin-color 'red))))))
   `(hydra-face-amaranth ((t (:foreground ,(catppuccin-color 'maroon) :weight bold))))
   `(hydra-face-blue ((t (:foreground ,(catppuccin-color 'blue) :weight bold))))
   `(hydra-face-pink ((t (:foreground ,(catppuccin-color 'pink) :weight bold))))
   `(hydra-face-red ((t (:foreground ,(catppuccin-color 'red) :weight bold))))
   `(hydra-face-teal ((t (:foreground ,(catppuccin-color 'teal) :weight bold))))
   `(markdown-code-face ((t (:background ,(face-background 'region) :extend t))))
   ;; mu4e faces: beware that mu4e:view uses gnus faces and mu4e:compose uses message faces
   `(mu4e-highlight-face ((t (:inherit default :weight bold :foreground ,(catppuccin-color 'green)))))
   '(message-mml ((t (:inherit message-header-other))))
   `(message-header-name ((t (:foreground ,(catppuccin-color 'lavender) :weight bold))))
   `(message-header-to ((t (:foreground ,(catppuccin-color 'maroon) :weight bold))))
   `(message-header-subject ((t (:foreground ,(catppuccin-color 'flamingo) :weight bold))))
   `(message-header-other ((t (:foreground ,(catppuccin-color 'rosewater)))))
   '(message-cited-text-1 ((t (:inherit gnus-cite-1))))
   '(message-cited-text-2 ((t (:inherit gnus-cite-2))))
   '(message-cited-text-3 ((t (:inherit gnus-cite-3))))
   '(message-cited-text-4 ((t (:inherit gnus-cite-4))))
   '(gnus-header-name ((t (:inherit message-header-name))))
   '(gnus-header-from ((t (:inherit message-header-to))))
   '(gnus-header-subject ((t (:inherit message-header-subject))))
   '(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-cite-attribution ((t (:foreground ,(catppuccin-color 'blue)))))
   `(gnus-cite-1 ((t (:foreground ,(catppuccin-color 'sapphire)))))
   `(gnus-cite-2 ((t (:foreground ,(catppuccin-color 'subtext1)))))
   `(gnus-cite-3 ((t (:foreground ,(catppuccin-color 'subtext0)))))
   `(gnus-cite-4 ((t (:foreground ,(catppuccin-color 'overlay2)))))
   `(gnus-cite-5 ((t (:foreground ,(catppuccin-color 'overlay1)))))
   `(gnus-cite-6 ((t (:foreground ,(catppuccin-color 'overlay0)))))
   `(gnus-cite-7 ((t (:foreground ,(catppuccin-color 'surface2)))))
   `(gnus-cite-8 ((t (:foreground ,(catppuccin-color 'surface1)))))
   `(gnus-cite-9 ((t (:foreground ,(catppuccin-color 'surface0)))))
   `(gnus-cite-10 ((t (:foreground ,(catppuccin-color 'surface0)))))
   `(gnus-cite-11 ((t (:foreground ,(catppuccin-color 'surface0)))))
   `(gnus-button ((t (:foreground ,(catppuccin-color 'lavender) :weight bold))))
   `(gnus-signature ((t (:foreground ,(catppuccin-color 'flamingo)))))
   '(mu4e-header-face ((t (:inherit font-lock-comment-face))))
   `(mu4e-draft-face ((t (:foreground ,(catppuccin-color 'sapphire)))))
   '(mu4e-replied-face ((t (:inherit mu4e-header-face :slant normal :weight normal))))
   '(mu4e-related-face ((t (:inherit mu4e-header-face :slant normal))))
   '(mu4e-thread-fold-face ((t (:inherit which-key-group-description-face))))
   ;; org faces
   `(org-hide ((t (:foreground ,(face-background 'default)))))
   `(org-todo ((t (:foreground ,(catppuccin-color 'peach) :weight bold))))
   `(org-done ((t (:inherit font-lock-comment-face :weight bold))))
   `(org-headline-done ((t (:inherit org-done :weight normal))))
   '(org-checkbox ((t (:inherit 'org-todo))))
   `(org-tag ((t (:foreground ,(catppuccin-color 'sapphire)))))
   `(org-mode-line-clock ((t (:foreground ,(catppuccin-color 'green)))))
   `(org-agenda-date-today ((t (:foreground ,(catppuccin-color 'blue) :weight bold))))
   `(org-agenda-done ((t (:inherit org-headline-done))))
   `(org-scheduled-previously ((t (:foreground ,(catppuccin-color 'maroon) :weight normal))))
   '(org-scheduled-today ((t (:inherit org-warning))))
   '(org-scheduled ((t (:inherit default))))
   `(org-upcoming-deadline ((t (:foreground ,(catppuccin-color 'subtext0)))))
   `(org-upcoming-distant-deadline ((t (:foreground ,(catppuccin-color 'surface2)))))
   `(org-time-grid ((t (:foreground ,(catppuccin-color 'lavender)))))
   `(org-habit-overdue-face ((t (:background ,(catppuccin-color 'red) :foreground ,(catppuccin-color 'surface0)))))
   `(org-habit-overdue-future-face ((t (:background ,(catppuccin-color 'maroon) :foreground ,(catppuccin-color 'surface0)))))
   `(org-habit-alert-future-face ((t (:background ,(catppuccin-color 'pink) :foreground ,(catppuccin-color 'surface0)))))
   `(org-habit-ready-face ((t (:background ,(catppuccin-color 'flamingo) :foreground ,(catppuccin-color 'surface0)))))
   `(org-habit-ready-future-face ((t (:background ,(catppuccin-color 'flamingo) :foreground ,(catppuccin-color 'surface0)))))
   `(org-habit-clear-face ((t (:background ,(catppuccin-color 'rosewater) :foreground ,(catppuccin-color 'surface0)))))
   `(org-habit-clear-future-face ((t (:background ,(catppuccin-color 'rosewater) :foreground ,(catppuccin-color 'surface0)))))
   `(org-super-agenda-header ((t (:foreground ,(catppuccin-color 'mauve)))))
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
(tool-bar-mode -1)

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
  (doom-modeline-buffer-encoding nil)
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
  (solaire-global-mode 1))

(use-package dimmer
  :ensure t
  :config
  (dimmer-configure-which-key)
  (dimmer-configure-hydra)
  (dimmer-configure-magit)
  (dimmer-configure-org)
  (dimmer-mode 1))

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
