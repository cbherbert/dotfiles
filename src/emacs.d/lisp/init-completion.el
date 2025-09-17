;;; init-completion.el --- Completion

;;; Commentary:
;;
;; Configuration for completion framework
;;

;;; Code:

;;;
;;; Minibuffer completion
;;;

(use-package vertico
  :ensure t
  :custom
  ;; Different scroll margin
  ;; (vertico-scroll-margin 0)
  ;; Show more candidates
  ;; (vertico-count 20)
  ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-resize t)
  ;; enable cycling for `vertico-next' and `vertico-previous'.
  (vertico-cycle t)
  :config
  ;; Add arrow prefix to current candidate:
  (defvar +vertico-current-arrow t)
  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((and +vertico-current-arrow
                                                   (not (bound-and-true-p vertico-flat-mode)))
                                              (eql t)))
    (setq cand (cl-call-next-method cand prefix suffix index start))
    (if (bound-and-true-p vertico-grid-mode)
	(if (= vertico--index index)
            (concat #("▶" 0 1 (face vertico-current)) cand)
          (concat #("_" 0 1 (display " ")) cand))
      (if (= vertico--index index)
          (concat #(" " 0 1 (display (left-fringe right-triangle vertico-current))) cand)
	cand)))
  :init
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
	      ;; vertico-directory-delete-word is a small improvement for paths over backward-kill-word, still available as M-DEL
	      ;; vertico-directory-enter completes directory instead of opening dired when pressing RET
	      ("<backspace>" . vertico-directory-delete-char)
	      ("C-<backspace>" . vertico-directory-delete-word)
	      ("RET" . vertico-directory-enter)
	      )
  :hook
  ;; Cleanup the path when searching in HOME or /:
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  )

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
  :custom (marginalia-align 'right)
  :hook (after-init . marginalia-mode))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :ensure t
  :vc (:url "https://github.com/maxecharel/all-the-icons-completion.git"
	    :branch "contrib"
	    :rev :newest)
  :if (display-graphic-p)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))

;;;
;;; Completion at point
;;;

(use-package company
  :ensure t
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 3)
  (company-tooltip-align-annotations t)
  (company-icon-margin 3)
  :hook (after-init . global-company-mode))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

;;;
;;; Templates
;;;

(use-package yasnippet
  :ensure t
  :custom
  (yas-snippet-dirs `(,(expand-file-name "snippets" user-emacs-directory)))
  (yas-wrap-around-region t)
  :hook ((text-mode
          prog-mode
          conf-mode
          snippet-mode) . yas-minor-mode-on)
  ; because the scratch buffer is prog-mode this leads to yasnippet being loaded
  ; immediately, however the overhead does not seem to be large so I will keep
  ; it that way for now
  :config
  (yas-reload-all)
  ; this is necessary, otherwise the snippets in `yas-snippet-dirs` are not
  ; loaded. Using `(yas-global-mode 1)` would be another option.
  )

(provide 'init-completion)

;;; init-completion.el ends here
