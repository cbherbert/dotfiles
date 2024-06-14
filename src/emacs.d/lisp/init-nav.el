;;; init-nav.el --- Navigation, etc

;;; Commentary:
;;
;; Configuration for tools used for navigating buffers, files, commands...
;; For now this is mostly consult and embark.
;;

;;; Code:

(use-package hydra
  :ensure t
  :bind
  ("C-c C-h z" . hydra-font-size/body)
  ("C-c C-h s" . hydra-move-splitter/body)
  ("C-c C-h w" . hydra-windows/body)
  ("C-c C-h r" . hydra-region/body)
  ("C-c C-h p" . hydra-point/body)
  :config
  (defhydra hydra-font-size nil
    "Font size"
    ("+" text-scale-increase "increase")
    ("-" text-scale-decrease "decrease")
    ("=" (text-scale-increase 0) "restore"))
  (require 'hydra-examples)
  (defhydra hydra-move-splitter nil
    "Move splitter"
    ("<up>" hydra-move-splitter-up "up")
    ("<down>" hydra-move-splitter-down "down")
    ("<left>" hydra-move-splitter-left "left")
    ("<right>" hydra-move-splitter-right "right"))
  (defhydra hydra-windows nil
    "Arrange windows and buffers within"
    ("0" delete-window "Delete selected")
    ("1" delete-other-windows "Delete others")
    ("2" split-window-below "Split below")
    ("3" split-window-right "Split right")
    ("o" other-window "Other window")
    ("<left>" windmove-left "Move left")
    ("<right>" windmove-right "Move right")
    ("<up>" windmove-up "Move up")
    ("<down>" windmove-down "Move down")
    ("f" find-file "Find file")
    ("k" kill-this-buffer "Kill buffer")
    ("b" (if (fboundp 'consult-buffer)
	     (call-interactively #'consult-buffer)
	   (call-interactively #'switch-to-buffer)) "Switch buffer")
    ("B" (if (fboundp 'consult-buffer-other-window)
	     (call-interactively #'consult-buffer-other-window)
	   (call-interactively #'switch-to-buffer-other-window)) "Switch buffer other window")
    ("r" (if (fboundp 'consult-bookmark)
	     (call-interactively #'consult-bookmark)
	   (call-interactively #'bookmark-jump)) "Jump to Bookmark")
    ("p" (if (fboundp 'consult-project-buffer)
	     (call-interactively #'consult-project-buffer)
	   (call-interactively #'project-witchto-buffer)) "Switch buffer current project")
    ("<" winner-undo "Previous configuration")
    (">" winner-redo "Next configuration")
    )
  (defhydra hydra-region nil
    "Act on region"
    ("x" exchange-point-and-mark "Reactivate mark")
    ("w" kill-region "Kill")
    ("W" kill-ring-save "Copy")
    ("f" fill-region "Fill")
    ("\\" indent-region "Indent")
    ("<tab>" indent-rigidly "Indent rigidly")
    ("l" downcase-region "Downcase")
    ("u" upcase-region "Upcase")
    ("n" narrow-to-region "Narrow")
    ("w" widen "Widen")
    ("%" query-replace "Query replace")
    (";" comment-dwim "Comment")
    ("$" ispell-region "Check Spelling")
    ("!" eval-region "Evaluate as Lisp code")
    )
  (defhydra hydra-point nil
    "Move point"
    ;; line, screen, buffer:
    ("<left>" left-char "Left")
    ("<right>" right-char "Right")
    ("a" move-beginning-of-line "Beginning of line")
    ("e" move-end-of-line "End of line")
    ("u" scroll-up-command "Scroll one screen up")
    ("d" scroll-down-command "Scroll one screen down")
    ("r" move-to-window-line-top-bottom "line-top-bottom")
    ("<" beginning-of-buffer "Beginning of buffer")
    (">" end-of-buffer "End of buffer")
    ;; words, sentences and paragraphs:
    ("B" backward-word "Backward word")
    ("F" forward-word "Forward word")
    ("A" backward-sentence "Beginning of sentence")
    ("E" forward-sentence "End of sentence")
    ("{" backward-paragraph "Backward paragraph")
    ("}" forward-paragraph "Forward paragraph")
    ("h" mark-paragraph "Mark paragraph")
    )
  )

(use-package consult-org
  :after consult org
  :bind (:map org-mode-map
	      ("M-g o" . consult-org-heading)))

(use-package consult
  :ensure t
  :custom-face
  (consult-bookmark :inherit 'bookmark-menu-bookmark)
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
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
	 ("C-s" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
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

(use-package consult-dir
  :ensure t
  :custom
  (consult-dir-project-list-function #'consult-dir-projectile-dirs)
  :bind
  ("C-x C-d" . consult-dir)
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

(provide 'init-nav)

;;; init-nav.el ends here)
