;;; init-code.el --- Programming Languages and Code Edition

;;; Commentary:
;;
;; configuration for programming languages
;;

;;; Code:

(add-to-list 'auto-mode-alist '("\\.fpp" . f90-mode))
(add-to-list 'auto-mode-alist '("\\.f90_*" . f90-mode))
;; Fortran namelist mode:
(when (file-exists-p "~/.emacs.d/f90-namelist-mode/f90-namelist-mode.el")
  (add-to-list 'load-path "~/.emacs.d/f90-namelist-mode/")
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
  :config
  (setq matlab-indent-function t)
  (setq matlab-shell-command "matlab")
  )

;;;
;;  Python
;;;
(use-package pydoc
  :ensure t)
(use-package ein
  :ensure t)
(use-package python-mode
  :ensure t
  :config
  ;; use IPython
  (setq-default py-shell-name "ipython")
  (setq py-force-py-shell-name-p t)
  (setq-default py-which-bufname "IPython")
  ;; use the wx backend, for both mayavi and matplotlib
  (setq py-python-command-args '("--pylab=TkAgg"))
  ;;      '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
  ;; switch to the interpreter after executing code
  (setq py-shell-switch-buffers-on-execute-p t)
  (setq py-switch-buffers-on-execute-p nil)
  ;; split windows
  (setq py-split-windows-on-execute-p t)
  (setq py-split-window-on-execute-threshold 4)
  (setq py-split-windows-on-execute-function 'split-window-horizontally)
  ;; try to automagically figure out indentation
  (setq py-smart-indentation t)
  )

(provide 'init-code)

;;; init-code.el ends here
