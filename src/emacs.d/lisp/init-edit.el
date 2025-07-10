;;; init-edit.el --- General Text Editing

;;; Commentary:
;;
;; Configuration for general text edition
;;

;;; Code:

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

;;;
;;; Spell checking
;;;

(use-package ispell
  :custom
  (ispell-program-name "aspell") ;; check that it exists, otherwise fallback to ispell
  (ispell-silently-savep t)
  :bind ("C-$" . 'ispell-word) ;; defautl is M-$, which is also screenshot on macOS when using Command key for Meta
  )

(use-package flyspell
  :hook
  (text-mode . flyspell-mode)
  :config
  (keymap-unset flyspell-mode-map "C-.") ;; avoid clash with `embark-act`
  ;; Default bindings:
  ;; C-; is bound to flyspell-auto-correct-previous-word
  ;; C-, is bound to flyspell-goto-next-error
  ;; C-c $ is bound to flyspell-correct-word-before-point
  ;; C-M-i is bound to flyspell-auto-correct-word
  )

(use-package abbrev
  :custom
  (save-abbrevs 'silently)
  (abbrev-file-name (concat user-emacs-directory "abbrev_defs"))
  :hook
  (text-mode . abbrev-mode))

(use-package flyspell-correct
  :ensure t
  :after flyspell abbrev
  :bind (:map flyspell-mode-map ("C-:" . flyspell-correct-wrapper))
  :config
  (defun ch/add-abbrev-flyspell-correct (orig-fun &rest args)
    (let ((word (nth 1 args))
	  (repl (apply orig-fun args)))
      (if (and word repl (not (equal word repl)))
        (let ((word (downcase word))
              (repl (downcase repl)))
          (define-abbrev global-abbrev-table word repl)
          (message "Abbrev: \"%s\" now expands to \"%s\"" word repl))
	(user-error "No typo at or before point"))
      repl))
  (advice-add 'flyspell-correct-completing-read :around #'ch/add-abbrev-flyspell-correct)
  )

(use-package guess-language
  :hook
  (text-mode . guess-language-mode)
  :custom
  (guess-language-min-paragraph-length 35)
  (guess-language-languages '(en fr))
  )

;;;
;;; Syntax checking
;;;
(use-package flycheck
  :ensure t
  :custom
  (flycheck-emacs-lisp-load-path 'inherit) ;; avoid errors with emacs init files for instance
  (flycheck-error-list-format [("File" 10) ("Line" 5 flycheck-error-list-entry-< :right-align t) ("Col" 3 nil :right-align t) ("Level" 8 flycheck-error-list-entry-level-<) ("ID" 6 t) (#("Message (Checker)" 0 7 (face flycheck-error-list-error-message) 9 16 (face flycheck-error-list-checker-name)) 0 t)])
  :config
  (setq flycheck-global-modes '(not dir-locals-mode
                                    text-mode
                                    org-mode
                                    vterm-mode))
  (global-flycheck-mode))

(use-package flycheck-aspell
  :ensure t
  :after flycheck
  :config
  (add-to-list 'flycheck-checkers 'c-aspell-dynamic)
  (add-to-list 'flycheck-checkers 'html-aspell-dynamic)
  (add-to-list 'flycheck-checkers 'mail-aspell-dynamic)
  (add-to-list 'flycheck-checkers 'markdown-aspell-dynamic)
  (add-to-list 'flycheck-checkers 'nroff-aspell-dynamic)
  (add-to-list 'flycheck-checkers 'tex-aspell-dynamic)
  (add-to-list 'flycheck-checkers 'texinfo-aspell-dynamic)
  (add-to-list 'flycheck-checkers 'xml-aspell-dynamic)
  (defun flycheck-maybe-recheck (_)
    (when (bound-and-true-p flycheck-mode)
      (flycheck-buffer)))
  (advice-add #'ispell-pdict-save :after #'flycheck-maybe-recheck)
  )

(use-package consult-flycheck
  :after flycheck consult
  :ensure t
  )

;;;
;;; Git
;;;

(use-package git-modes
  :ensure t)

(use-package magit
  :ensure t
  :bind ("C-x g" . 'magit-status)
  )

(use-package git-gutter
  :ensure t
  :after hydra
  :custom
  (git-gutter:disabled-modes '(fundamental-mode image-mode pdf-view-mode))
  (git-gutter:ask-p nil)
  :bind
  ("C-x v n" . git-gutter:next-hunk)
  ("C-x v p" . git-gutter:previous-hunk)
  ("C-x v t" . vs-create-tag)
  ("C-x v s" . git-gutter:stage-hunk)
  ("C-c C-h v" . hydra-git-gutter/body)
  :config
  (setq git-gutter:update-interval 0.02)
  ;; The following code is taken from doom-emacs, it allows git-gutter-mode to switch on
  ;; automatically when opening files under version control
  (defvar +vc-gutter-in-remote-files nil
    "If non-nil, enable the vc gutter in remote files (e.g. open through TRAMP).")
  (add-hook 'find-file-hook
    (defun +vc-gutter-init-maybe-h ()
      "Enable `git-gutter-mode' in the current buffer.
If the buffer doesn't represent an existing file, `git-gutter-mode's activation
is deferred until the file is saved. Respects `git-gutter:disabled-modes'."
      (let ((file-name (buffer-file-name (buffer-base-buffer))))
        (cond
         ((and (file-remote-p (or file-name default-directory))
               (not +vc-gutter-in-remote-files)))
         ;; UX: If not a valid file, wait until it is written/saved to activate
         ;;   git-gutter.
         ((not (and file-name (vc-backend file-name)))
          (add-hook 'after-save-hook #'+vc-gutter-init-maybe-h nil 'local))
         ;; UX: Allow git-gutter or git-gutter-fringe to activate based on the
         ;;   type of frame we're in. This allows git-gutter to work for silly
         ;;   geese who open both tty and gui frames from the daemon.
         ((if (and (display-graphic-p)
                   (require 'git-gutter-fringe nil t))
              (setq-local git-gutter:init-function      #'git-gutter-fr:init
                          git-gutter:view-diff-function #'git-gutter-fr:view-diff-infos
                          git-gutter:clear-function     #'git-gutter-fr:clear
                          git-gutter:window-width -1)
            (setq-local git-gutter:init-function      'nil
                        git-gutter:view-diff-function #'git-gutter:view-diff-infos
                        git-gutter:clear-function     #'git-gutter:clear-diff-infos
                        git-gutter:window-width 1))
          (unless (memq major-mode git-gutter:disabled-modes)
            (git-gutter-mode +1)
            (remove-hook 'after-save-hook #'+vc-gutter-init-maybe-h 'local)))))))
  ;; UX: update git-gutter on focus (in case I was using git externally)
  (add-hook 'focus-in-hook #'git-gutter:update-all-windows)

  (defun ch/git-gutter:toggle-popup-hunk ()
    "Toggle git-gutter diff buffer"
    (interactive)
    (if (window-live-p (git-gutter:popup-buffer-window))
	(delete-window (git-gutter:popup-buffer-window))
      (git-gutter:popup-hunk)))

  (defhydra hydra-git-gutter nil
    "Hunk:"
    ("p" git-gutter:previous-hunk "previous")
    ("n" git-gutter:next-hunk "next")
    ("s" git-gutter:stage-hunk "stage")
    ("r" git-gutter:revert-hunk "revert")
    ("m" git-gutter:mark-hunk "mark")
    ("d" ch/git-gutter:toggle-popup-hunk "toggle diff"))
  )

(use-package git-gutter-fringe
  :ensure t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package comint
  :config
  (define-key comint-mode-map [down] 'comint-next-matching-input-from-input)
  (define-key comint-mode-map [up] 'comint-previous-matching-input-from-input)
  )

(use-package wgrep
  :ensure t
  :custom
  (wgrep-auto-save-buffer t))

;;;
;;; Specific modes
;;;

(use-package markdown-mode
  :defer t
  :mode ("README\\.md\\'" . gfm-mode)
  :ensure t)

(use-package yaml-mode
  :defer t
  :ensure t)

(use-package csv-mode
  :ensure t
  :mode ("\\.tsv$" . csv-mode)
  :hook ((csv-mode . hl-line-mode)
	 (csv-mode . csv-align-mode))
  )

(use-package xml-mode
  :mode ("\\.plist$" . xml-mode)
  )

;;;
;;; LaTeX mode
;;;
(use-package latex
  :ensure auctex
  :after citar
  :mode (("\\.tex\\'" . latex-mode) ("\\.lbx" . latex-mode) ("\\.bbx" . latex-mode) ("\\.cbx" . latex-mode))
  :hook ((LaTeX-mode . visual-line-mode)
	 (LaTeX-mode . latex-math-mode)
	 (LaTeX-mode . reftex-mode))
  :custom
  (reftex-default-bibliography bibtexfile)
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-PDF-mode t)
  (reftex-plug-into-AUCTeX t)
  :config
  (setq-default TeX-master nil)
  (defun tex-green-region ()
    "Change region color to green for comments/deletion."
    (interactive)
    (save-excursion
      (goto-char (region-beginning))
      (insert "\\textcolor{green}{"))
    (goto-char (region-end))
    (insert "}\n"))
  :bind
  (:map LaTeX-mode-map ("C-c i" . citar-insert-citation))
  )

(use-package lsp-latex
  :ensure t
  :after latex
  :hook ((TeX-mode . lsp-mode)
	 (LaTeX-mode . lsp-mode))
  )

(provide 'init-edit)

;;; init-edit.el ends here
