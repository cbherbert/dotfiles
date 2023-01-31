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
      (display-time-mode 1)
      (size-indication-mode)
      (use-package all-the-icons
	:ensure t)
      (use-package all-the-icons-ibuffer
	:ensure t
	:hook (ibuffer-mode . all-the-icons-ibuffer-mode))
      (use-package doom-themes
	:ensure t
	:config
	(setq custom-theme-directory "~/.emacs.d/themes/")
	(load-theme 'doom-solarized-dark-custom t)
	(setq custom-file "~/.emacs.d/custom-gui.el")
	(load custom-file)
	(doom-themes-visual-bell-config)
	)
      (use-package doom-modeline
	:ensure t
	:custom
	(doom-modeline-enable-word-count 1)
	:hook (after-init . doom-modeline-mode))
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

(provide 'init-ui)

;;; init-ui.el ends here
