; -*-mode: emacs-lisp-*-

;;;
;;  All the settings are managed using use-package.
;;  We first make sure it is installed.
;;  Other packages are installed through use-package if necessary.
;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;;
;;   Vanilla emacs settings
;;;
(use-package emacs
  :custom
  (fill-column 80)
  (require-final-newline t)
  (vc-follow-symlinks t)
  (calendar-today-visible-hook '(calendar-mark-today))
  (calendar-week-start-day 1)
  ;;(Buffer-menu-name-width 30)
  (eol-mnemonic-dos "\\")
  (eol-mnemonic-mac "/")
  (epg-pinentry-mode 'loopback)
  ;; Compilation mode:
  (compilation-scroll-output 'first-error)
  (recentf-max-menu-items 25)
  (recentf-max-saved-items 25)
  :init
  (put 'narrow-to-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  ;; Remove trailing whitespace upon saving, except for markdown where two spaces at the end of the line indicate line break:
  (add-hook 'before-save-hook
	    (lambda ()
	      (unless (derived-mode-p 'markdown-mode)
		(delete-trailing-whitespace))))
  ;; Automatically wrap lines for text modes:
  (add-hook 'text-mode-hook 'turn-on-visual-line-mode)

  (defun format-sentence-lines ()
    "Break a long line or text block into multiple lines by ending period.
Work on text selection if there is one, else the current text block.
URL `http://ergoemacs.org/emacs/elisp_reformat_to_sentence_lines.html'
Version 2020-12-02 2021-04-14 2021-08-01"
    (interactive)
    (let ($p1 $p2)
      (if (use-region-p)
          (setq $p1 (region-beginning) $p2 (region-end))
	(progn
          (if (re-search-backward "\n[ \t]*\n+" nil "move")
              (progn (re-search-forward "\n[ \t]*\n+")
                     (setq $p1 (point)))
            (setq $p1 (point)))
          (re-search-forward "\n[ \t]*\n" nil "move")
          (setq $p2 (point))))
      (save-restriction
	(narrow-to-region $p1 $p2)
	(progn (goto-char (point-min)) (while (search-forward "\n" nil t) (replace-match " " )))
	(progn (goto-char (point-min)) (while (re-search-forward "  +" nil t) (replace-match " " )))
	(progn (goto-char (point-min)) (while (re-search-forward "\\. +\\([0-9A-Za-z]+\\)" nil t) (replace-match ".\n\\1" )))
	(progn (goto-char (point-min)) (while (search-forward " <a " nil t) (replace-match "\n<a " )))
	(progn (goto-char (point-min)) (while (search-forward "</a>" nil t) (replace-match "</a>\n" )))
	(goto-char (point-max))
	(while (eq (char-before ) 32) (delete-char -1))
	(insert "\n\n"))))

  ;; Line and column numbers:
  ;;(global-linum-mode t)
  ;;(setq linum-format "%6d ")
  (line-number-mode t)
  (column-number-mode t)
  (global-eldoc-mode -1)

  ;; Highlight matching parenthesis
  (show-paren-mode 1)
  (setq show-paren-style 'parenthesis) ; highlight brackets - can also be expression or mixed
  (set-face-background 'show-paren-match (face-background 'default))
  (set-face-foreground 'show-paren-match "magenta")
  (set-face-attribute 'show-paren-match nil :weight 'normal)

  ;; Idutils:
  (autoload 'gid "idutils" nil t)

  ;; Define path to bibtex file for various tools (auctex, org-mode, etc)
  (setq bibtexfile (if (eq system-type 'darwin)
		       '("~/Library/texmf/bibtex/bib/bibtexlib2.bib")
		     '("~/texmf/bibtex/bib/bibtexlib2.bib")))

  (recentf-mode 1)

  ;;;
  ;;   Buffers and Windows
  ;;;

  ;; Buffer list
  (defalias 'list-buffers 'ibuffer-other-window)

  ;; Custom automatic window splitting: try to split horizontally first
  (defun split-window-hfirst (&optional window)
    (let ((window (or window (selected-window))))
      (or (and (window-splittable-p window t)
               ;; Split window horizontally.
               (with-selected-window window
		 (split-window-right)))
          (and (window-splittable-p window)
               ;; Split window vertically.
               (with-selected-window window
		 (split-window-below)))
          (and (eq window (frame-root-window (window-frame window)))
               (not (window-minibuffer-p window))
               ;; If WINDOW is the only window on its frame and is not the
               ;; minibuffer window, try to split it horizontally disregarding
               ;; the value of `split-width-threshold'.
               (let ((split-width-threshold 0))
		 (when (window-splittable-p window t)
                   (with-selected-window window
                     (split-window-right))))))))

  (setq split-window-preferred-function 'split-window-hfirst)
  (setq split-width-threshold 190)

  ;; Custom command to rotate windows
  ;; see https://www.emacswiki.org/emacs/TransposeWindows
  (defun rotate-windows (arg)
    "Rotate your windows; use the prefix argument ARG to rotate the other direction."
    (interactive "P")
    (if (not (> (count-windows) 1))
	(message "You can't rotate a single window!")
      (let* ((rotate-times (prefix-numeric-value arg))
             (direction (if (or (< rotate-times 0) (equal arg '(4)))
                            'reverse 'identity)))
	(dotimes (_ (abs rotate-times))
          (dotimes (i (- (count-windows) 1))
            (let* ((w1 (elt (funcall direction (window-list)) i))
                   (w2 (elt (funcall direction (window-list)) (+ i 1)))
                   (b1 (window-buffer w1))
                   (b2 (window-buffer w2))
                   (s1 (window-start w1))
                   (s2 (window-start w2))
                   (p1 (window-point w1))
                   (p2 (window-point w2)))
              (set-window-buffer-start-and-point w1 b2 s2 p2)
              (set-window-buffer-start-and-point w2 b1 s1 p1)))))))

  ; Move through past window configuration:
  (winner-mode 1)

  :bind
  ("C-c r" . 'replace-string)
  ("C-c q" . 'query-replace)
  ("C-c TAB" . 'indent-region)
  ("C-x +" . 'enlarge-window)
  ("C-x -" . 'balance-windows)
  ("C-x ^" . 'shrink-window-if-larger-than-buffer)
  )

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
	(setq custom-file "~/.emacs-custom-gui")
	(load custom-file)
	)
      (use-package doom-modeline
	:ensure t
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
	(setq custom-file "~/.emacs-custom-terminal")
	(load custom-file)
	))

;;;
;;    Text editing and spelling
;;;
(use-package comint
  :config
  (define-key comint-mode-map [down] 'comint-next-matching-input-from-input)
  (define-key comint-mode-map [up] 'comint-previous-matching-input-from-input)
  )

(use-package flycheck
  :ensure t
  :config
  (setq flycheck-global-modes '(not dir-locals-mode
                                    text-mode
                                    org-mode
                                    vterm-mode))
  (global-flycheck-mode))

(use-package flycheck-aspell
  :ensure t
  :config
  (add-to-list 'flycheck-checkers 'c-aspell-dynamic)
  (add-to-list 'flycheck-checkers 'html-aspell-dynamic)
  (add-to-list 'flycheck-checkers 'mail-aspell-dynamic)
  (add-to-list 'flycheck-checkers 'markdown-aspell-dynamic)
  (add-to-list 'flycheck-checkers 'nroff-aspell-dynamic)
  (add-to-list 'flycheck-checkers 'tex-aspell-dynamic)
  (add-to-list 'flycheck-checkers 'texinfo-aspell-dynamic)
  (add-to-list 'flycheck-checkers 'xml-aspell-dynamic)
  )

(use-package markdown-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package csv-mode
  :ensure t
  :mode ("\\.tsv$" . csv-mode)
  :hook ((csv-mode . hl-line-mode)
	 (csv-mode . csv-align-mode))
  )

;; Color dired mode
(use-package diredful
  :ensure t
  :config
  (unless (display-graphic-p)
    (setq diredful-init-file "~/dotfiles/src/diredful-conf.el"))
  (diredful-mode 1)
  )

(when (display-graphic-p)
  (use-package all-the-icons-dired
    :ensure t
    :hook (dired-mode . all-the-icons-dired-mode)))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

;;;
;;    Completion
;;;

(use-package vertico
  :ensure t
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(when (display-graphic-p)
  (use-package all-the-icons-completion
    :after (marginalia all-the-icons)
    :ensure t
    :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
    :init
    (all-the-icons-completion-mode)))

(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ;; ("M-#" . consult-register-load)
         ;; ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ;; ("C-M-#" . consult-register)
         ;; Other custom bindings
         ;; ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ;; ("M-g e" . consult-compile-error)
         ;; ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ;; ("M-g m" . consult-mark)
         ;; ("M-g k" . consult-global-mark)
         ;; ("M-g i" . consult-imenu)
         ;; ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ;; ("M-s d" . consult-find)
         ;; ("M-s D" . consult-locate)
         ;; ("M-s g" . consult-grep)
         ;; ("M-s G" . consult-git-grep)
         ;; ("M-s r" . consult-ripgrep)
         ;; ("M-s l" . consult-line)
	 ("C-s" . consult-line)
         ;; ("M-s L" . consult-line-multi)
         ;; ("M-s m" . consult-multi-occur)
         ;; ("M-s k" . consult-keep-lines)
         ;; ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ;; ("M-s e" . consult-isearch-history)
         ;; :map isearch-mode-map
         ;; ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ;; ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ;; ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ;; ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         ;; :map minibuffer-local-map
         ;; ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ;; ("M-r" . consult-history)                 ;; orig. previous-matching-history-element
	 :map org-mode-map
	 ("M-g o" . consult-org-heading)
	 )

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  ;; (setq register-preview-delay 0.5
  ;;       register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  ;; (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  ;; (setq xref-show-xrefs-function #'consult-xref
  ;;       xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  ;; (consult-customize
  ;;  consult-theme
  ;;  :preview-key '(:debounce 0.2 any)
  ;;  consult-ripgrep consult-git-grep consult-grep
  ;;  consult-bookmark consult-recent-file consult-xref
  ;;  consult--source-bookmark consult--source-recent-file
  ;;  consult--source-project-recent-file
  ;;  :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  ;; (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
)

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; projectile
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  )


(use-package treemacs
  :ensure t
  :defer t
  :config
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

(when (display-graphic-p)
  (use-package treemacs-all-the-icons
    :ensure t
    :config
    (treemacs-load-theme "all-the-icons")))

;;;
;;   AUCTeX
;;;
(use-package auctex
  :ensure t
  :mode (("\\.lbx" . latex-mode) ("\\.bbx" . latex-mode) ("\\.cbx" . latex-mode))
  :hook ((LaTeX-mode . visual-line-mode)
	 (LaTeX-mode . flyspell-mode)
	 (LaTeX-mode . latex-math-mode)
	 (LaTeX-mode . reftex-mode))
  :custom
  (reftex-default-bibliography bibtexfile)
  :config
  (progn
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq-default TeX-master nil)
    (setq reftex-plug-into-AUCTeX t)
    (setq TeX-PDF-mode t)
    )
  )

(defun tex-green ()
  (interactive)
  (save-excursion
    (goto-char (region-beginning))
    (insert "\\textcolor{green}{"))
  (goto-char (region-end))
  (insert "}\n"))

;;;
;;   YASnippet
;;;
(use-package yasnippet
  :ensure t
  :config
  (progn
    (yas-global-mode 1)
    (setq yas-wrap-around-region t)
    (setq yas-snippet-dirs (append '("~/dotfiles/src/yas-snippets") yas-snippet-dirs))
    (yas-reload-all)
    )
  )

;;;
;;    Git
;;;
(use-package gitconfig-mode
  :ensure t)

(use-package gitignore-mode
  :ensure t)

(use-package magit
  :ensure t
  :bind ("C-x g" . 'magit-status)
  )

;;;
;;    Programming languages
;;;
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


;;;
;;   Org mode
;;;
(use-package org
  :custom
  (org-agenda-files '("~/org/todo.org" "~/org/projects.org" "~/owncloud/org/meetings.org" "~/owncloud/org/seminars.org" "~/owncloud/org/holidays.org" "~/owncloud/org/discussions"))
  (org-agenda-skip-deadline-prewarning-if-scheduled t)
  (org-agenda-todo-ignore-deadlines 'near)
  (org-agenda-todo-ignore-scheduled 'all)
  ;;(org-cite-global-bibliography '("~/Library/texmf/bibtex/bib/bibtexlib.bib"))
  (org-cite-global-bibliography bibtexfile)
  (org-export-backends '(ascii beamer html icalendar latex man md odt texinfo))
  (org-modules '(org-habit))
  (org-n-level-faces 5)
  (org-todo-keywords
   '((sequence "TODO" "INPROGRESS" "WAIT" "|" "DONE" "CANCELLED")))
  (org-hide-emphasis-markers t)
  (org-refile-targets '((org-agenda-files :maxlevel . 5)))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t) (latex . t) (python . t)))
  (add-to-list 'org-src-lang-modes '("latex" . latex))
  (setq org-startup-indented t)
  (defun ch/org-narrow-to-subtree-up (arg &optional invisible-ok)
    "Narrow buffer to parent subtree of current heading"
    (interactive "p")
    (save-excursion
      (widen)
      (outline-up-heading arg invisible-ok)
      (org-narrow-to-subtree)))
  (setq org-agenda-custom-commands
	'(("c" "Agenda and TODO by priority"
           ((agenda "")
	    (alltodo ""
		     ((org-agenda-files '("~/org/projects.org"))
		      (org-agenda-overriding-header "Projects:")))
	    (alltodo ""
		     ((org-agenda-files '("~/org/todo.org"))
		      (org-agenda-overriding-header "Tasks:")))))
	  ("w" "Agenda and WAIT items"
	   ((agenda "")
	    (todo "WAIT")))
	  ("r" "Reading list" alltodo ""
	   ((org-agenda-files '("~/owncloud/org/roam/reads.org"))))
	  ))
  (setq org-capture-templates
	'(("m" "meetings" entry (file "~/owncloud/org/meetings.org") "* %?")
	  ("h" "holidays" entry (file "~/owncloud/org/holidays.org") "* %?\n%^{Beginning}t--%^{End}t")
	  ("d" "Templates for Discussions")
	  ("da" "Discussions Alessandro" plain (file "~/owncloud/org/discussions/discussions-alessandro.org") "%^t%?")
	  ("db" "Discussions Bastien" plain (file "~/owncloud/org/discussions/discussions-bastien.org") "%^t%?")
	  ("dt" "Discussions Tim" plain (file "~/owncloud/org/discussions/discussions-tim.org") "%^t%?")
	  ("s" "Templates for Seminars")
	  ("sm" "MathInFluids" entry (file+olp "~/owncloud/org/seminars.org" "MathInFluids")
	   "* %?\n%^t")
	  ("sg" "GFDiscussions" entry (file+olp "~/owncloud/org/seminars.org" "GFDiscussions")
	   "* %?\n%^t")
	  ("t" "TODO list" entry (file+olp "~/owncloud/org/todo.org" "Tasks") "* TODO %?\n")
	  ("r" "reviews" entry (file+olp "~/owncloud/org/todo.org" "Reviews & Editorial Work") "* TODO %?\n")
	  ("l" "reading list" entry (file+olp "~/owncloud/org/roam/reads.org" "Topics to read about") "* TODO %?\n")
	  ))
  (add-hook 'org-mode-hook (lambda ()
			     (setq-local time-stamp-active t
					 time-stamp-start "last_modified:[ \t]*"
					 time-stamp-end "$"
					 time-stamp-format "\[%Y-%02m-%02d %a %02H:%02M\]")
			     (add-hook 'before-save-hook 'time-stamp nil 'local)))
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link)
   ("C-x n u" . ch/org-narrow-to-subtree-up))
  )

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package citar
  :ensure t
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  (citar-at-point-function 'embark-act)
  (citar-symbol-separator " ")
  (citar-templates
   '((main . "${author editor:30}     ${date year issued:4}     ${journal journaltitle publisher container-title collection-title:20}     ${title:48}")
     (suffix . "     ${=type=:12}    ${tags keywords:*}")
     (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
     (note . "Notes on '${title}', ${author editor}")))
  (citar-notes-paths '("~/owncloud/org/notes"))
  :config
  ;; note: the symbols defined below are only visible when using vertico as the completion framework
  (when (display-graphic-p)
    (setq citar-symbols
	  `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
            (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
            (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " "))))
  (add-to-list 'savehist-additional-variables 'citar-history)
  (defun ch/citar-org-format-note (key entry)
    (let* ((title-template (citar--get-template 'note))
           (note-meta (when title-template
			(citar-format--entry title-template entry)))
	   (author (citar-format--entry "${author editor}" key))
	   (year (citar-format--entry "${year issued date}" key))
	   (filepath (expand-file-name
                      (concat key ".org")
                      (car citar-notes-paths)))
	   (buffer (find-file filepath))
	   (template (with-temp-buffer
		       (insert-file-contents (string-join (list (file-name-as-directory (car citar-notes-paths)) "template.org")))
		       (buffer-string)))
	   (now (format-time-string "\[%Y-%02m-%02d %a %02H:%02M\]"))
	   )
      (with-current-buffer buffer
	(erase-buffer)
	(citar-org-roam-make-preamble key)
	(insert (format template
			note-meta
			user-full-name
			now
			now
			author
			year))
	(search-backward "@")
	(delete-char 1)
	(citar-insert-citation (list key))
	(search-forward "|")
	(delete-char -1)
	(when (fboundp 'evil-insert)
          (evil-insert 1)))))
  (setq citar-note-format-function 'ch/citar-org-format-note)
  ;; optional: org-cite-insert is also bound to C-c C-x C-@
  :bind
  (("C-c n n" . citar-open-notes)
   ("C-c n o" . citar-open-files)
   :map org-mode-map :package org ("C-c b" . #'org-cite-insert)))

(use-package citar-embark
  :ensure t
  :after citar embark
  :config
  (citar-embark-mode)
  )

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/owncloud/org/roam"))
  (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:20}" 'face 'org-tag)))
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates '(
				("d" "default" plain "%?" :target (file+head "${slug}-%<%Y%m%d%H%M%S>.org" "#+title: ${title}\n#+author: Corentin Herbert\n#+date_added: %u\n#+last_modified: %U")
				 :unnarrowed t)
				("b" "book" entry "" :target (file "reads.org") :unnarrowed t)
				))
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n a" . org-roam-alias-add))
  :config
  (org-roam-db-autosync-mode)
  )

(use-package consult-org-roam
   :ensure t
   :init
   (require 'consult-org-roam)
   ;; Activate the minor-mode
   (consult-org-roam-mode 1)
   :custom
   (consult-org-roam-grep-func #'consult-ripgrep)
   ;; :config
   ;; ;; Eventually suppress previewing for certain functions
   ;; (consult-customize
   ;;  consult-org-roam-forward-links
   ;;  :preview-key (kbd "M-."))
   :bind
   ("C-c n e" . consult-org-roam-file-find)
   ("C-c n b" . consult-org-roam-backlinks)
   ("C-c n r" . consult-org-roam-search))

(require 'appt)
(appt-activate t)
;; Add org-mode TODOs to appointments: at startup, every day at midnight, and when saving todo file
(org-agenda-to-appt)
(run-at-time "12:05am" (* 24 3600) 'org-agenda-to-appt)
(add-hook 'after-save-hook
          '(lambda ()
             (if (string= (buffer-file-name) (concat (getenv "HOME") "/org/todo.org"))
                 (org-agenda-to-appt))))
;; Display warning in a notification window
(use-package alert
  :ensure t
  :config
  (setq alert-default-style 'notifier)
  (defun appt-alert (min-to-app new-time msg) (alert msg :title "Reminder"))
  (setq appt-disp-window-function 'appt-alert)
  (setq appt-delete-window-function (lambda () t))
  (setq appt-display-mode-line nil)
  ;; Send one persistent warning 5 minutes before the event
  (setq appt-message-warning-time 5)
  (setq appt-display-interval appt-message-warning-time)
  (setq appt-display-duration (* appt-message-warning-time 60))
  (setq alert-fade-time (* appt-message-warning-time 60)))

(use-package try
  :ensure t)