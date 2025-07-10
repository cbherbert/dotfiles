;;; init-mail.el --- Mail configuration

;;; Commentary:
;;
;; Mail configuration
;;

;;; Code:

(when (eq system-type 'gnu/linux)
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e"))

(use-package mu4e
  :commands mu4e
  :bind
  (:map mu4e-headers-mode-map
	("~" . ch/mu4e-toggle-search-include-related))
  (:map mu4e-compose-minor-mode-map
	("G" . mu4e-compose-supersede))
  :custom
  (mail-user-agent 'mu4e-user-agent)
  (read-mail-command 'mu4e)
  (mu4e-completing-read-function 'completing-read)
  (mu4e-notification-support (if (eq system-type 'gnu/linux) t nil))
  (mu4e-user-mail-address-list '("corentin.herbert@ens-lyon.fr"
				 "corentin.herbert@cnrs.fr"))
  (mu4e-root-maildir "~/.maildir")
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-attachment-dir "~/Downloads")
  (mu4e-update-interval 300) ;in seconds
  ;; rename files when moving - needed for mbsync:
  (mu4e-change-filenames-when-moving t)
  (sendmail-program "msmtp")
  (send-mail-function 'sendmail-send-it)
  (message-send-mail-function 'sendmail-send-it)
  (message-citation-line-function 'message-insert-formatted-citation-line)
  (message-citation-line-format "On %a, %b %d %Y %R, %N wrote:\n")
  (message-kill-buffer-on-exit t)
  (mu4e-headers-fields '((:flags . 6) (:subject . 100) (:from . 22) (:human-date . 12) (:mailing-list . 10)))
  (mu4e-maildir-shortcuts '((:name "Drafts" :maildir "/enslyon/Drafts" :key ?d)
			    (:name "Sent" :maildir "/enslyon/Sent" :key ?s)
			    (:name "Computing" :maildir "/enslyon/Computing Centers" :key ?C)
			    (:name "Conferences" :maildir "/enslyon/Conferences" :key ?c)
			    (:name "Ecologie" :maildir "/enslyon/Ecologie" :key ?e)
			    (:name "Funding" :maildir "/enslyon/Funding" :key ?f)
			    (:name "GDR" :maildir "/enslyon/GDR Theorie Climat" :key ?G)
			    (:name "Group" :maildir "/enslyon/Group Lyon" :key ?g)
			    (:name "Institutions" :maildir "/enslyon/Institutions" :key ?i)
			    (:name "Labo" :maildir "/enslyon/Labo" :key ?l)
			    (:name "Manuscripts" :maildir "/enslyon/Manuscripts" :key ?m)
			    (:name "Reviews" :maildir "/enslyon/Reviews" :key ?r)
			    (:name "Teaching" :maildir "/enslyon/Teaching" :key ?t)
			    ))
  (mu4e-bookmarks
   '((:name "INBOX" :query "maildir:/enslyon/INBOX OR maildir:/cnrs/INBOX" :key ?i)
     (:name "Unread messages" :query "flag:unread AND NOT flag:trashed AND NOT maildir:/enslyon/Junk" :key ?u)
     (:name "Today's messages" :query "date:today..now AND NOT maildir:/enslyon/Trash AND NOT maildir:/enslyon/Junk" :key ?t)
     (:name "Last 7 days" :query "date:7d..now AND NOT maildir:/enslyon/Trash AND NOT maildir:/enslyon/Junk" :key ?w)
     (:name "Messages with images" :query "mime:image/*" :key ?p)
     ))
  (mu4e-context-policy 'pick-first)
  (mu4e-compose-context-policy 'pick-first)
  (message-signature nil)
  (message-signature-file "~/dotfiles/private/signature")
  (mu4e-compose-signature-auto-include nil)
  (mu4e-thread-fold-unread t)
  (mu4e-use-fancy-chars t)
  :hook
  (mu4e-view-mode . visual-line-mode)
  (mu4e-compose-mode . ch/mu4e-compose-hook)
  :config
  (setq mu4e-contexts `(,(make-mu4e-context
			  :name "enslyon"
			  :enter-func
			  (lambda () (mu4e-message "Enter ENS Lyon context"))
			  :leave-func
			  (lambda () (mu4e-message "Leave ENS Lyon context"))
			  :match-func
			  (lambda (msg)
			    (when msg
			      (mu4e-message-contact-field-matches msg :to "corentin.herbert@ens-lyon.fr")))
			  :vars '((user-mail-address . "corentin.herbert@ens-lyon.fr")
				  (user-full-name . "Corentin Herbert")
				  (mu4e-drafts-folder . "/enslyon/Drafts")
				  (mu4e-refile-folder . "/enslyon/Archives")
				  (mu4e-sent-folder . "/enslyon/Sent")
				  (mu4e-trash-folder . "/enslyon/Trash")))
			,(make-mu4e-context
			  :name "cnrs"
			  :enter-func
			  (lambda () (mu4e-message "Enter CNRS context"))
			  :leave-func
			  (lambda () (mu4e-message "Leave CNRS context"))
			  :match-func
			  (lambda (msg)
			    (when msg
			      (mu4e-message-contact-field-matches msg :to "corentin.herbert@cnrs.fr")))
			  :vars '((user-mail-address . "corentin.herbert@cnrs.fr")
				  (user-full-name . "Corentin Herbert")
				  (mu4e-drafts-folder . "/cnrs/Drafts")
				  (mu4e-refile-folder . "/cnrs/Archive")
				  (mu4e-sent-folder . "/cnrs/Sent Items")
				  (mu4e-trash-folder . "/cnrs/Trash")))
			))
  ;; The following two functions are taken from doom emacs, they make sure the
  ;; icons appear nicely with appropriate spacing
  (defun +mu4e--get-string-width (str)
    "Return the width in pixels of a string in the current
window's default font. If the font is mono-spaced, this
will also be the width of all other printable characters."
    (let ((window (selected-window))
          (remapping face-remapping-alist))
      (with-temp-buffer
	(make-local-variable 'face-remapping-alist)
	(setq face-remapping-alist remapping)
	(set-window-buffer window (current-buffer))
	(insert str)
	(car (window-text-pixel-size)))))

  (cl-defun +mu4e-normalised-icon (name &key set color height v-adjust space-right)
    "Convert :icon declaration to icon"
    (let* ((icon-set (intern (concat "nerd-icons-" (or set "faicon"))))
           (v-adjust (or v-adjust 0.02))
           (height (or height 0.8))
           (icon (if color
                     (apply icon-set `(,name :face ,(intern (concat "nerd-icons-" color)) :height ,height :v-adjust ,v-adjust))
                   (apply icon-set `(,name  :height ,height :v-adjust ,v-adjust))))
           (icon-width (+mu4e--get-string-width icon))
           (space-width (+mu4e--get-string-width " "))
           (space-factor (- 2 (/ (float icon-width) space-width)))
           ;; always pad the left
           (space-left (propertize " " 'display `(space . (:width ,space-factor))))
           ;; optionally pad the right
           (space-right (if space-right space-left "")))
      (format "%s%s%s" space-left icon space-right)))

  (setq mu4e-headers-personal-mark (cons "p" (+mu4e-normalised-icon "nf-fa-user"))
	mu4e-headers-list-mark       (cons "l" (+mu4e-normalised-icon "nf-fa-sitemap" :set "faicon"))
	mu4e-headers-attach-mark     (cons "a" (+mu4e-normalised-icon "nf-fa-file_text_o" :color "silver"))
	mu4e-headers-flagged-mark    (cons "F" (+mu4e-normalised-icon "nf-fa-flag"))
	mu4e-headers-unread-mark     (cons "u" (+mu4e-normalised-icon "nf-fa-eye_slash" :v-adjust 0.05))
	mu4e-headers-new-mark        (cons "N" (+mu4e-normalised-icon "nf-md-sync" :set "mdicon" :v-adjust -0.10))
	mu4e-headers-replied-mark    (cons "R" (+mu4e-normalised-icon "nf-fa-reply"))
	mu4e-headers-passed-mark     (cons "P" (+mu4e-normalised-icon "nf-fa-arrow_right"))
	mu4e-headers-encrypted-mark  (cons "x" (+mu4e-normalised-icon "nf-fa-lock"))
	mu4e-headers-signed-mark     (cons "s" (+mu4e-normalised-icon "nf-fa-certificate" :height 0.7 :color "dpurple"))
	mu4e-headers-trashed-mark    (cons "T" (+mu4e-normalised-icon "nf-fa-trash"))
	mu4e-headers-draft-mark      (cons "D" (+mu4e-normalised-icon "nf-fa-pencil"))
	mu4e-headers-calendar-mark   (cons "c" (+mu4e-normalised-icon "nf-fa-calendar"))
	)


  (defun ch/mu4e-ask-maildir-level2 (orig-fun &rest args)
    " Access second-level maildirs "
    (let ((response (apply orig-fun (list (car args))))
	  (flag (car (cdr args))))
      (cond ((and (string-equal "/enslyon/Teaching" response) (not (string-equal response flag)))
	     (let ((mu4e-maildir-shortcuts '((:name "Teaching" :maildir "/enslyon/Teaching" :key ?t)
					     (:name "Turbulence M2" :maildir "/enslyon/Teaching/M2 Turbulence ENSL" :key ?T)
					     (:name "Turbulence M2 Lille" :maildir "/enslyon/Teaching/M2 Turbulence Lille" :key ?L)
					     (:name "AOFD M2" :maildir "/enslyon/Teaching/M2 AOFD ENSL" :key ?a)
					     (:name "GFD M2 2023"  :maildir "/enslyon/Teaching/GFD Week M2 2023" :key ?g)
					     (:name "Climate M2" :maildir "/enslyon/Teaching/M2 Climat ENSL" :key ?c)
					     (:name "Climate Change and Energy Transition M1" :maildir "/enslyon/Teaching/M1 Climat-Energie" :key ?C)
					     (:name "Environnement L3"  :maildir "/enslyon/Teaching/L3 Environnement" :key ?e)
					     (:name "Stages ENS" :maildir "/enslyon/Teaching/Stages ENS" :key ?s))))
	       (mu4e-ask-maildir "" response)))
	    ((and (string-equal "/enslyon/Group Lyon" response) (not (string-equal response flag)))
	     (let ((mu4e-maildir-shortcuts '((:name "Group" :maildir "/enslyon/Group Lyon" :key ?g)
					     (:name "Dario" :maildir "/enslyon/Group Lyon/Dario" :key ?d)
					     (:name "Bastien" :maildir "/enslyon/Group Lyon/Bastien" :key ?b)
					     (:name "Alessandro" :maildir "/enslyon/Group Lyon/Alessandro" :key ?a)
					     (:name "Tim" :maildir "/enslyon/Group Lyon/Tim" :key ?t)
					     (:name "Louis" :maildir "/enslyon/Group Lyon/Louis" :key ?l)
					     (:name "Valentine" :maildir "/enslyon/Group Lyon/Valentine" :key ?v))))
	       (mu4e-ask-maildir "" response)))
	    (t response))))
  (advice-add 'mu4e-ask-maildir :around #'ch/mu4e-ask-maildir-level2)
  (defun ch/mu4e-compose-hook ()
    "Setup flyspell for message writing"
    (setq flyspell-generic-check-word-predicate 'mail-mode-flyspell-verify)
    (flyspell-mode)
    )
  (defun ch/mu4e-toggle-search-include-related ()
    " Switch mu4e-search-include-related on and off "
    (interactive)
    (setq mu4e-search-include-related (not mu4e-search-include-related))
    (mu4e-search-rerun)
    (message "Include related messages: %s" mu4e-search-include-related)
    )
  (defvar ch/mu4e-thread-fold-status mu4e-thread--fold-status "Store global thread fold status to allow restoring it later")
  (defun ch/mu4e-search-remember-fold-status (orig-fun &rest args)
    " Store thread folding state to restore it after action "
    (setq ch/mu4e-thread-fold-status mu4e-thread--fold-status))
  (advice-add 'mu4e--search-execute :before #'ch/mu4e-search-remember-fold-status)
  (add-hook 'mu4e-headers-found-hook (lambda ()
				       (if ch/mu4e-thread-fold-status
					   (mu4e-thread-fold-all)
					 (mu4e-thread-unfold-all))))
  (defun ch/capture-mail-answer-later (msg)
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "a"))
  (defun ch/capture-mail-todo (msg)
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "t"))
  (add-to-list 'mu4e-headers-actions
	       '("answer later" . ch/capture-mail-answer-later) t)
  (add-to-list 'mu4e-view-actions
	       '("answer later" . ch/capture-mail-answer-later) t)
  (add-to-list 'mu4e-headers-actions
	       '("todo entry from mail" . ch/capture-mail-todo) t)
  (add-to-list 'mu4e-view-actions
	       '("todo entry from mail" . ch/capture-mail-todo) t)
  )

(use-package mu4e-alert
  :if (eq system-type 'darwin)
  :ensure t
  :after mu4e alert
  :hook
  (after-init . mu4e-alert-enable-notifications)
  :config
  (mu4e-alert-enable-mode-line-display)
  (mu4e-alert-set-default-style alert-default-style)
  )

(provide 'init-mail)

;;; init-mail.el ends here
