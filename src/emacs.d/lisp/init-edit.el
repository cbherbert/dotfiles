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
;;    Spell checking
;;;
(use-package ispell
  :custom
  (ispell-program-name "aspell") ;; check that it exists, otherwise fallback to ispell
  (ispell-silently-savep t)
  :bind ("C-$" . 'ispell-word) ;; defautl is M-$, which is also screenshot on macOS when using Command key for Meta
  )

;;;
;;    Git
;;;
(use-package git-modes
  :ensure t)

(use-package magit
  :ensure t
  :bind ("C-x g" . 'magit-status)
  )

(use-package git-gutter
  :ensure t
  :hook (prog-mode . git-gutter-mode)
  :bind
  ("C-x v n" . git-gutter:next-hunk)
  ("C-x v p" . git-gutter:previous-hunk)
  ("C-x v t" . vs-create-tag)
  ("C-x v s" . git-gutter:stage-hunk)
  :config
  (setq git-gutter:update-interval 0.02))

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

(use-package flycheck
  :ensure t
  :custom-face
  (flycheck-error-list-highlight ((t (:weight bold :background ,(face-background 'region)))))
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

;;;
;;   AUCTeX
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

(provide 'init-edit)

;;; init-edit.el ends here
