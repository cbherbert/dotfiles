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

(when (display-graphic-p)
  (use-package all-the-icons-completion
    :after (marginalia all-the-icons)
    :ensure t
    :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
    :init
    (all-the-icons-completion-mode)))

(use-package company
  :ensure t
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 3)
  (company-tooltip-align-annotations t)
  (company-icon-margin 3)
  :hook (after-init . global-company-mode))

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
