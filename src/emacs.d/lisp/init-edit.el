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
;;    Git
;;;
(use-package git-modes
  :ensure t)

(use-package magit
  :ensure t
  :bind ("C-x g" . 'magit-status)
  )

(use-package comint
  :config
  (define-key comint-mode-map [down] 'comint-next-matching-input-from-input)
  (define-key comint-mode-map [up] 'comint-previous-matching-input-from-input)
  )

(use-package flycheck
  :ensure t
  :custom
  (flycheck-emacs-lisp-load-path 'inherit) ;; avoid errors with emacs init files for instance
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
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-PDF-mode t)
  (reftex-plug-into-AUCTeX t)
  :config
  (setq-default TeX-master nil)
  )

(defun tex-green-region ()
  "Change region color to green for comments/deletion."
  (interactive)
  (save-excursion
    (goto-char (region-beginning))
    (insert "\\textcolor{green}{"))
  (goto-char (region-end))
  (insert "}\n"))

(provide 'init-edit)

;;; init-edit.el ends here
