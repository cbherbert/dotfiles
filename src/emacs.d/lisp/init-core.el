;;; init-core.el --- Core Configuration

;;; Commentary:
;;
;; Core configuration
;;

;;; Code:

(use-package emacs
  :custom
  (fill-column 80)
  (vc-follow-symlinks t)
  (calendar-today-visible-hook '(calendar-mark-today))
  (calendar-week-start-day 1)
  ;;(Buffer-menu-name-width 30)
  (eol-mnemonic-dos "\\")
  (eol-mnemonic-mac "/")
  ;; Compilation mode:
  (compilation-scroll-output 'first-error)
  :init
  (put 'narrow-to-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
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

  ;; Idutils:
  (autoload 'gid "idutils" nil t)

  ;; Define path to bibtex file for various tools (auctex, org-mode, etc)
  (setq bibtexfile (if (eq system-type 'darwin)
		       '("~/Library/texmf/bibtex/bib/bibtexlib2.bib")
		     '("~/texmf/bibtex/bib/bibtexlib2.bib")))

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

  (add-hook 'window-setup-hook 'toggle-frame-fullscreen t)

  :bind
  ("C-c r" . 'replace-string)
  ("C-c q" . 'query-replace)
  ("C-c TAB" . 'indent-region)
  ("C-x +" . 'enlarge-window)
  ("C-x -" . 'balance-windows)
  ("C-x ^" . 'shrink-window-if-larger-than-buffer)
(use-package recentf
  :custom
  (recentf-max-menu-items 25)
  (recentf-max-saved-items 25)
  :init
  (recentf-mode 1)
  )

(use-package files
  :custom
  (require-final-newline t)
  (backup-directory-alist       ; File name patterns and backup directory names.
      `(("." . ,(expand-file-name "backups" user-emacs-directory))))
  (auto-save-list-file-prefix ; Prefix for generating auto-save-list-file-name
   (expand-file-name "auto-save-list/.saves-" user-emacs-directory))
  :init
  (add-to-list 'auto-save-file-name-transforms `(".*" ,(expand-file-name "auto-save/" user-emacs-directory) t) t)
  ;; Remove trailing whitespace upon saving, except for markdown where two spaces at the end of the line indicate line break:
  (add-hook 'before-save-hook
	    (lambda ()
	      (unless (derived-mode-p 'markdown-mode)
		(delete-trailing-whitespace))))
  )

;; Highlight matching parens
;; code for off-screen parens from centaur emacs
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :init (setq show-paren-when-point-inside-paren t
              show-paren-when-point-in-periphery t
	      show-paren-style 'parenthesis ; highlight brackets - can also be expression or mixed
	      )
  :config
  (with-no-warnings
    ;; Display matching line for off-screen paren.
    (defun display-line-overlay (pos str &optional face)
      "Display line at POS as STR with FACE.

FACE defaults to inheriting from default and show-paren-match."
      (let ((ol (save-excursion
                  (goto-char pos)
                  (make-overlay (line-beginning-position)
                                (line-end-position)))))
        (overlay-put ol 'display str)
        (overlay-put ol 'face
                     (or face '(:inherit show-paren-match)))
        ol))

    (defvar-local show-paren--off-screen-overlay nil)
    (defun show-paren-off-screen (&rest _args)
      "Display matching line for off-screen paren."
      (when (overlayp show-paren--off-screen-overlay)
        (delete-overlay show-paren--off-screen-overlay))
      ;; Check if it's appropriate to show match info,
      (when (and (overlay-buffer show-paren--overlay)
                 (not (or cursor-in-echo-area
                          executing-kbd-macro
                          noninteractive
                          (minibufferp)
                          this-command))
                 (and (not (bobp))
                      (memq (char-syntax (char-before)) '(?\) ?\$)))
                 (= 1 (logand 1 (- (point)
                                   (save-excursion
                                     (forward-char -1)
                                     (skip-syntax-backward "/\\")
                                     (point))))))
        ;; Rebind `minibuffer-message' called by `blink-matching-open'
        ;; to handle the overlay display.
        (cl-letf (((symbol-function #'minibuffer-message)
                   (lambda (msg &rest args)
                     (let ((msg (apply #'format-message msg args)))
                       (setq show-paren--off-screen-overlay
                             (display-line-overlay
                              (window-start) msg ))))))
          (blink-matching-open))))
    (advice-add #'show-paren-function :after #'show-paren-off-screen '((name . "show-paren-off-screen")))))

(use-package savehist
  :init
  (savehist-mode))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package exec-path-from-shell
  :ensure t
  :custom
  (exec-path-from-shell-arguments (list "-l"))
  :config
  (add-to-list 'exec-path-from-shell-variables "GNUPGHOME")
  (exec-path-from-shell-initialize))

(use-package epg
  :custom
  (epg-pinentry-mode 'loopback)
  (epg-gpg-home-directory (getenv "GNUPGHOME"))
  :custom-face
  (epa-validity-high ((t (:foreground ,(face-foreground 'success)))))
  (epa-validity-medium ((t (:foreground ,(face-foreground 'warning) :slant normal))))
  (epa-validity-disabled ((t (:foreground ,(face-foreground 'error) :inverse-video nil))))
  )

(provide 'init-core)

;;; init-core.el ends here
