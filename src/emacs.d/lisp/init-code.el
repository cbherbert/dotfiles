;;; init-code.el --- Programming Languages and Code Edition

;;; Commentary:
;;
;; configuration for programming languages
;;

;;; Code:

(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode))

(use-package hl-line
  :hook (prog-mode . hl-line-mode))

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
;;  Python
;;;
(use-package pydoc
  :ensure t)
(use-package ein
  :ensure t)
(use-package python
  :ensure nil
  :custom
  (python-shell-interpreter "python3"))
(use-package snakemake-mode
  :ensure t)

(provide 'init-code)

;;; init-code.el ends here
