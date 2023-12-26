;;; init-mail.el --- Mail configuration

;;; Commentary:
;;
;; Mail configuration
;;

;;; Code:

(when (eq system-type 'gnu/linux)
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e"))
(require 'mu4e)

(use-package mu4e
  :custom
  (mail-user-agent 'mu4e-user-agent)
  (read-mail-command 'mu4e)
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
  (mu4e-headers-fields '((:flags . 6) (:subject . 100) (:from . 22) (:human-date . 12) (:mailing-list . 10)))
  (mu4e-maildir-shortcuts '((:name "Drafts" :maildir "/enslyon/Drafts" :key ?d)
			    (:name "Sent" :maildir "/enslyon/Sent" :key ?s)
			    (:name "Manuscripts" :maildir "/enslyon/Manuscripts" :key ?m)
			    (:name "Reviews" :maildir "/enslyon/Reviews" :key ?r)
			    (:name "Teaching" :maildir "/enslyon/Teaching" :key ?T)
			    (:name "Bastien" :maildir "/enslyon/Group Lyon/Bastien" :key ?b)
			    (:name "Alessandro" :maildir "/enslyon/Group Lyon/Alessandro" :key ?a)
			    (:name "Tim" :maildir "/enslyon/Group Lyon/Tim" :key ?t)
			    (:name "Louis" :maildir "/enslyon/Group Lyon/Louis" :key ?l)
			    ))
  (mu4e-bookmarks
   '((:name "INBOX" :query "maildir:/enslyon/INBOX OR maildir:/cnrs/INBOX" :key ?i)
     (:name "Unread messages" :query "flag:unread AND NOT flag:trashed AND NOT maildir:/enslyon/Junk" :key ?u)
     (:name "Today's messages" :query "date:today..now AND NOT maildir:/enslyon/Trash AND NOT maildir:/enslyon/Junk" :key ?t)
     (:name "Last 7 days" :query "date:7d..now AND NOT maildir:/enslyon/Trash AND NOT maildir:/enslyon/Junk" :hide-unread t :key ?w)
     (:name "Messages with images" :query "mime:image/*" :key ?p)
     ))
  (mu4e-contexts `(,(make-mu4e-context
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
			     (mu4e-refile-folder . "/enslyon/Archive")
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
			     (mu4e-sent-folder . "/cnrs/Sent")
			     (mu4e-trash-folder . "/cnrs/Trash")))
		   ))
  (mu4e-context-policy 'pick-first)
  (mu4e-compose-context-policy 'pick-first)
  (mu4e-compose-signature "Corentin Herbert\nCNRS Researcher\nLaboratoire de Physique, ENS de Lyon\n46, allée d'Italie\n69364 Lyon cedex 07\n+33 4 26 23 39 60\nhttp://perso.ens-lyon.fr/corentin.herbert/")
  (mu4e-compose-signature-auto-include nil)
  :custom-face
  (mu4e-highlight-face ((t (:inherit default :weight bold :foreground ,(face-foreground 'success)))))
  (mu4e-header-face ((t (:inherit font-lock-comment-face))))
  (mu4e-header-key-face ((t (:inherit gnus-header-content))))
  (mu4e-related-face ((t (:inherit mu4e-header-face :slant normal))))
  (mu4e-forwarded-face ((t (:inherit mu4e-header-face :slant normal :weight normal))))
  (mu4e-replied-face ((t (:inherit mu4e-header-face :slant normal :weight normal))))
  (mu4e-unread-face ((t (:inherit default :weight bold))))
  (mu4e-header-highlight-face ((t (:inherit hl-line :weight bold :extend t :underline t :foreground ,(face-foreground 'success)))))
  :config
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
	       '("answer-later" . ch/capture-mail-answer-later) t)
  (add-to-list 'mu4e-headers-actions
	       '("todo entry from mail" . ch/capture-mail-todo) t)
  (add-to-list 'mu4e-view-actions
	       '("todo entry from mail" . ch/capture-mail-todo) t)
  )

(provide 'init-mail)

;;; init-mail.el ends here
