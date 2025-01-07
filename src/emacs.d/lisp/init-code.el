;;; init-code.el --- Programming Languages and Code Edition

;;; Commentary:
;;
;; configuration for programming languages
;;

;;; Code:

;;;
;;; Basic settings for code edition
;;;

(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode))

(use-package hl-line
  :hook (prog-mode . hl-line-mode))

;;;
;;; LSP
;;;

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :custom-face
  (lsp-face-highlight-textual ((t (:foreground ,(face-foreground 'all-the-icons-lsilver)))))
  :init
  (setq lsp-keymap-prefix "C-c s")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :after lsp-mode
  :ensure t)

(use-package consult-lsp
  :after consul lsp-mode
  :ensure t)
;;;
;;; Modes for code
;;;

(add-to-list 'auto-mode-alist '("\\.fpp" . f90-mode))
(add-to-list 'auto-mode-alist '("\\.f90_*" . f90-mode))
;; Fortran namelist mode:
(when (file-exists-p (expand-file-name "f90-namelist-mode/f90-namelist-mode.el" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "f90-namelist-mode/f90-namelist-mode.el" user-emacs-directory))
  (require 'f90-namelist-mode))


(use-package lua-mode
  :ensure t)

(use-package tuareg
  :ensure t)

(use-package cuda-mode
  :ensure t
  :mode ("\\.hcu" . cuda-mode)
  )

(use-package matlab
  :ensure matlab-mode
  :mode ("\\.m\\'" . matlab-mode)
  :custom
  (matlab-indent-function t)
  (matlab-shell-command "matlab")
  )

;;;
;;; Python
;;;
(use-package pydoc
  :ensure t)
(use-package ein
  :ensure t)
(use-package python
  :ensure nil
  :custom
  (python-shell-interpreter "python3"))
(use-package conda
  :ensure t
  :hook
  (find-file . ch/conda-activate-on-opening)
  :config
  (defun ch/conda-activate-on-opening ()
    (when (bound-and-true-p conda-project-env-path)
      (conda-env-activate-for-buffer)))
  )

(use-package lsp-pyright
  :ensure t)

(use-package snakemake-mode
  :ensure t)

;;;
;;; Julia
;;;
(use-package julia-mode
  :ensure t)

(use-package lsp-julia
  :ensure t
  :hook (julia-mode . lsp-mode)
  :config
  (setq lsp-julia-default-environment "~/.julia/environments/v1.10")
  )

(provide 'init-code)

;;; init-code.el ends here
