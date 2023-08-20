;;; init-completion.el --- Completion

;;; Commentary:
;;
;; Configuration for completion framework
;;

;;; Code:

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
  :custom-face
  (vertico-current ((t (:foreground ,(face-foreground 'success) :background ,(face-background 'region) :extend: t))))
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
          (concat
           #(" " 0 1 (display (left-fringe right-triangle vertico-current)))
           cand)
	cand)))
  :init
  (vertico-mode))

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

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :ensure t
  :if (display-graphic-p)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

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

(use-package yasnippet
  :ensure t
  :custom
  (yas-snippet-dirs '((expand-file-name "snippets" user-emacs-directory)))
  (yas-wrap-around-region t)
  :hook ((text-mode
          prog-mode
          conf-mode
          snippet-mode) . yas-minor-mode-on)
  )

(provide 'init-completion)

;;; init-completion.el ends here
