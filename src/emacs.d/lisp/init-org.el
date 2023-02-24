;;; init-org.el --- Org mode

;;; Commentary:
;;
;; org mode configuration
;;

;;; Code:

(use-package org
  :custom
  (org-agenda-files '("~/org/todo.org" "~/org/projects.org" "~/owncloud/org/meetings.org" "~/owncloud/org/teaching.org" "~/owncloud/org/conferences.org" "~/owncloud/org/seminars.org" "~/owncloud/org/holidays.org" "~/owncloud/org/discussions"))
  (org-agenda-skip-deadline-prewarning-if-scheduled t)
  (org-agenda-todo-ignore-deadlines 'near)
  (org-agenda-todo-ignore-scheduled 'all)
  ;;(org-cite-global-bibliography '("~/Library/texmf/bibtex/bib/bibtexlib.bib"))
  (org-cite-global-bibliography bibtexfile)
  (org-export-backends '(ascii beamer html icalendar latex man md odt texinfo))
  (org-modules '(org-habit))
  (org-n-level-faces 5)
  (org-todo-keywords
   '((sequence "TODO" "INPROGRESS" "WAIT" "|" "DONE" "CANCELLED")))
  (org-hide-emphasis-markers t)
  (org-refile-targets '((org-agenda-files :maxlevel . 5)))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-attach-store-link-p 'attach)
  (org-agenda-prefix-format '((agenda . " %i %-15:c%?-12t% s") (todo . " %i %-15:c") (tags . " %i %-15:c") (search . " %i %-15:c")))
  (org-habit-graph-column 60)
  (org-agenda-custom-commands
   '(("f" "Agenda and TODO by priority"
      ((agenda "")
       (alltodo ""
		((org-agenda-files '("~/org/projects.org"))
		 (org-agenda-overriding-header "Projects:")))
       (alltodo ""
		((org-agenda-files '("~/org/todo.org"))
		 (org-agenda-overriding-header "Tasks:")))))
     ("w" "Agenda and WAIT items"
      ((agenda "")
       (todo "WAIT")))
     ("r" "Reading list" alltodo ""
      ((org-agenda-files '("~/owncloud/org/roam/reads.org" "~/owncloud/org/notes/"))))
     ("c" "Conferences Overview" agenda ""
      ((org-agenda-files '("~/owncloud/org/conferences.org"))
       (org-agenda-show-all-dates 'nil)
       (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))
       (org-agenda-span 'year)
       ))
     ))
  (org-capture-templates
   '(("m" "meetings" entry (file "~/owncloud/org/meetings.org") "* %?")
     ("h" "holidays" entry (file "~/owncloud/org/holidays.org") "* %?\n%^{Beginning}t--%^{End}t")
     ("d" "Templates for Discussions")
     ("da" "Discussions Alessandro" plain (file "~/owncloud/org/discussions/discussions-alessandro.org") "%^t%?")
     ("db" "Discussions Bastien" plain (file "~/owncloud/org/discussions/discussions-bastien.org") "%^t%?")
     ("dt" "Discussions Tim" plain (file "~/owncloud/org/discussions/discussions-tim.org") "%^t%?")
     ("s" "Templates for Seminars")
     ("sm" "MathInFluids" entry (file+olp "~/owncloud/org/seminars.org" "MathInFluids")
      "* %?\n%^t")
     ("sg" "GFDiscussions" entry (file+olp "~/owncloud/org/seminars.org" "GFDiscussions")
      "* %?\n%^t")
     ("t" "TODO list" entry (file+olp "~/owncloud/org/todo.org" "Tasks") "* TODO %?\n")
     ("r" "reviews" entry (file+olp "~/owncloud/org/todo.org" "Reviews & Editorial Work") "* TODO %?\n")
     ("l" "reading list" entry (file+olp "~/owncloud/org/roam/reads.org" "Topics to read about") "* TODO %?\n")
     ))
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t) (latex . t) (python . t)))
  (add-to-list 'org-src-lang-modes '("latex" . latex))
  (setq org-startup-indented t)
  (defun ch/org-narrow-to-subtree-up (arg &optional invisible-ok)
    "Narrow buffer to parent subtree of current heading"
    (interactive "p")
    (save-excursion
      (widen)
      (outline-up-heading arg invisible-ok)
      (org-narrow-to-subtree)))
  (add-hook 'org-mode-hook (lambda ()
			     (setq-local time-stamp-active t
					 time-stamp-start "last_modified:[ \t]*"
					 time-stamp-end "$"
					 time-stamp-format "\[%Y-%02m-%02d %a %02H:%02M\]")
			     (add-hook 'before-save-hook 'time-stamp nil 'local)))
  (when (display-graphic-p)
    (setq org-agenda-category-icon-alist `(("todo" ,(list (all-the-icons-material "check_box" :height 1.1)) nil nil :ascent center)
					   ("tools" ,(list (all-the-icons-material "settings" :height 1.1)) nil nil :ascent center)
					   ("teaching" ,(list (all-the-icons-material "school" :height 1.1)) nil nil :ascent center)
					   ("conferences" ,(list (all-the-icons-material "explore" :height 1.1)) nil nil :ascent center)
					   ("grants" ,(list (all-the-icons-material "attach_money" :height 1.1)) nil nil :ascent center)
					   ;;("grants" ,(list (all-the-icons-material "account_balance" :height 1.1)) nil nil :ascent center)
					   ("reviews" ,(list (all-the-icons-material "rate_review" :height 1.1)) nil nil :ascent center)
					   ("projects" ,(list (all-the-icons-faicon "flask" :height 1.1)) nil nil :ascent center)
					   ("meetings" ,(list (all-the-icons-material "group" :height 1.1)) nil nil :ascent center)
					   ("discussions" ,(list (all-the-icons-material "chat" :height 1.1)) nil nil :ascent center)
					   ("reads" ,(list (all-the-icons-faicon "book" :height 1.1)) nil nil :ascent center)
					   ("GFDiscussions" ,(list (all-the-icons-wicon "day-cloudy-gusts" :height 0.8)) nil nil :ascent center)
					   ("MathInFluids" ,(list (all-the-icons-material "functions" :height 1.1)) nil nil :ascent center)
					   ("Holidays" ,(list (all-the-icons-faicon "home" :height 1.1)) nil nil :ascent center)
					   )))
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link)
   ("C-x n u" . ch/org-narrow-to-subtree-up))
  )

(use-package org-agenda-property
  :ensure t
  :custom
  (org-agenda-property-list '("LOCATION"))
  (org-agenda-property-position 'next-line)
  )

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-download
  :ensure t
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "data")
  (org-download-heading-lvl 0)
  (org-download-timestamp "%Y-%m-%d_%H-%M-%S_")
  (org-download-screenshot-method "screencapture -i %s")
  :bind
  (:map org-mode-map :package org ("C-c d c" . #'org-download-clipboard))
  (:map org-mode-map :package org ("C-c d s" . #'org-download-screenshot))
  )

(use-package appt
  ;; Add org-mode TODOs to appointments: at startup, every day at midnight, and when saving todo file
  :custom
  (appt-delete-window-function (lambda () t))
  (appt-display-mode-line nil)
  ;; Send one persistent warning 5 minutes before the event
  (appt-message-warning-time 5)
  (appt-display-interval appt-message-warning-time)
  (appt-display-duration (* appt-message-warning-time 60))
  :config
  (appt-activate t)
  (org-agenda-to-appt)
  (run-at-time "12:05am" (* 24 3600) 'org-agenda-to-appt)
  ;; the following code does not capture agenda files if they are in a directory which is in org-agenda-files.
  (add-hook 'after-save-hook '(lambda ()
			       (if (member buffer-file-name (mapcar #'expand-file-name org-agenda-files))
				   (org-agenda-to-appt))))
  )

;; Display warning in a notification window
(use-package alert
  :after appt
  :ensure t
  :custom
  (alert-default-style 'notifier)
  (appt-disp-window-function 'appt-alert)
  (alert-fade-time (* appt-message-warning-time 60))
  :config
  (defun appt-alert (min-to-app new-time msg) (alert msg :title "Reminder")))

(use-package citar
  :ensure t
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  (citar-at-point-function 'embark-act)
  (citar-symbol-separator " ")
  (citar-templates
   '((main . "${author editor:30}     ${date year issued:4}     ${journal journaltitle publisher container-title collection-title:20}     ${title:48}")
     (suffix . "     ${=type=:12}    ${tags keywords:*}")
     (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
     (note . "Notes on '${title}', ${author editor}")))
  (citar-notes-paths '("~/owncloud/org/notes"))
  :config
  ;; note: the symbols defined below are only visible when using vertico as the completion framework
  (when (display-graphic-p)
    (setq citar-symbols
	  `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
            (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
            (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " "))))
  (add-to-list 'savehist-additional-variables 'citar-history)
  (defun ch/citar-org-format-note (key entry)
    (let* ((title-template (citar--get-template 'note))
           (note-meta (when title-template
			(citar-format--entry title-template entry)))
	   (author (citar-format--entry "${author editor}" key))
	   (year (citar-format--entry "${year issued date}" key))
	   (filepath (expand-file-name
                      (concat key ".org")
                      (car citar-notes-paths)))
	   (buffer (find-file filepath))
	   (template (with-temp-buffer
		       (insert-file-contents (string-join (list (file-name-as-directory (car citar-notes-paths)) "template.org")))
		       (buffer-string)))
	   (now (format-time-string "\[%Y-%02m-%02d %a %02H:%02M\]"))
	   )
      (with-current-buffer buffer
	(erase-buffer)
	(citar-org-roam-make-preamble key)
	(insert (format template
			note-meta
			user-full-name
			now
			now
			author
			year))
	(search-backward "@")
	(delete-char 1)
	(citar-insert-citation (list key))
	(search-forward "|")
	(delete-char -1)
	(when (fboundp 'evil-insert)
          (evil-insert 1)))))
  (setq citar-note-format-function 'ch/citar-org-format-note)
  ;; optional: org-cite-insert is also bound to C-c C-x C-@
  :bind
  (("C-c n n" . citar-open-notes)
   ("C-c n o" . citar-open-files)
   :map org-mode-map :package org ("C-c b" . #'org-cite-insert)))

(use-package citar-embark
  :ensure t
  :after citar embark
  :config
  (citar-embark-mode)
  )

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/owncloud/org/roam"))
  (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:20}" 'face 'org-tag)))
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates '(
				("d" "default" plain "%?" :target (file+head "${slug}-%<%Y%m%d%H%M%S>.org" "#+title: ${title}\n#+author: Corentin Herbert\n#+date_added: %u\n#+last_modified: %U")
				 :unnarrowed t)
				("b" "book" entry "" :target (file "reads.org") :unnarrowed t)
				))
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n a" . org-roam-alias-add))
  :config
  (org-roam-db-autosync-mode)
  (setq org-roam-db-node-include-function
      (lambda ()
        (not (member "ATTACH" (org-get-tags)))))
  )

(use-package consult-org-roam
   :ensure t
   :init
   (require 'consult-org-roam)
   ;; Activate the minor-mode
   (consult-org-roam-mode 1)
   :custom
   (consult-org-roam-grep-func #'consult-ripgrep)
   ;; :config
   ;; ;; Eventually suppress previewing for certain functions
   ;; (consult-customize
   ;;  consult-org-roam-forward-links
   ;;  :preview-key (kbd "M-."))
   :bind
   ("C-c n e" . consult-org-roam-file-find)
   ("C-c n b" . consult-org-roam-backlinks)
   ("C-c n r" . consult-org-roam-search))

(provide 'init-org)

;;; init-org.el ends here
