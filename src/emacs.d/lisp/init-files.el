;;; init-files.el --- File Management

;;; Commentary:
;;
;; Configuration for file management tools
;;

;;; Code:

;; Color dired mode
(use-package diredful
  :ensure t
  :config
  (unless (display-graphic-p)
    (setq diredful-init-file "~/dotfiles/src/diredful-conf.el"))
  (diredful-mode 1)
  )

(use-package all-the-icons-dired
  :ensure t
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package projectile
  :ensure t
  :hook (after-init . projectile-mode)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(use-package treemacs
  :ensure t
  :defer t
  :custom
  (treemacs-follow-mode t)
  (treemacs-project-follow-mode t)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-all-the-icons
  :ensure t
  :if (display-graphic-p)
  :after treemacs
  :config
  (treemacs-load-theme "all-the-icons"))

(provide 'init-files)

;;; init-files.el ends here
