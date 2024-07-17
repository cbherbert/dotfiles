;;; init-org.el --- Org mode

;;; Commentary:
;;
;; org mode configuration
;;

;;; Code:

(use-package appt
  :after alert
  :custom
  (appt-delete-window-function (lambda () t))
  (appt-disp-window-function 'appt-alert)
  (appt-display-mode-line nil)
  ;; Send one persistent warning 5 minutes before the event
  (appt-message-warning-time 5)
  (appt-display-interval appt-message-warning-time)
  (appt-display-duration (* appt-message-warning-time 60))
  :config
  (defun appt-alert (min-to-app new-time msg) (alert msg :title "Reminder"))
  (appt-activate t)
  )

;; Display warning in a notification window
(use-package alert
  :ensure t
  :custom
  (alert-default-style (if (eq system-type 'darwin) 'osx-notifier 'libnotify))
  ;;(alert-fade-time (* appt-message-warning-time 60))
  )

(use-package org
  :after appt
  :custom-face
  (org-latex-and-related :inherit 'font-latex-math-face)
  :custom
  (org-list-allow-alphabetical t)
  (org-pretty-entities-include-sub-superscripts t)
  (org-pretty-entities t)
  (org-use-sub-superscripts "{}")
  (org-highlight-latex-and-related '(native entities)) ;;including 'script' here creates problem with underscores used outside of latex math mode, for instance paths in links
  (org-catch-invisible-edits 'smart)
  (org-hide-emphasis-markers t)
  (org-link-descriptive t)
  (org-startup-indented t)
  (org-startup-with-inline-images t)
  (org-log-done 'time)
  ;;(org-cite-global-bibliography '("~/Library/texmf/bibtex/bib/bibtexlib.bib"))
  (org-cite-global-bibliography bibtexfile)
  (org-export-backends '(ascii beamer html icalendar latex man md odt texinfo))
  (org-modules '(org-habit))
  (org-n-level-faces 5)
  (org-todo-keywords
   '((sequence "TODO" "INPROGRESS" "WAIT" "|" "DONE" "CANCELLED")))
  (org-attach-store-link-p 'attach)
  (org-habit-graph-column 60)
  (org-capture-templates
   '(("m" "meetings" entry (file "~/owncloud/org/core/meetings.org") "* %?\n%^t")
     ("h" "holidays" entry (file "~/owncloud/org/core/holidays.org") "* %?\n%^{Beginning}t--%^{End}t")
     ("d" "Templates for Discussions")
     ("da" "Discussions Alessandro" plain (file "~/owncloud/org/discussions/discussions-alessandro.org") "%^t%?")
     ("db" "Discussions Bastien" plain (file "~/owncloud/org/discussions/discussions-bastien.org") "%^t%?")
     ("dt" "Discussions Tim" plain (file "~/owncloud/org/discussions/discussions-tim.org") "%^t%?")
     ("dl" "Discussions Louis" plain (file "~/owncloud/org/discussions/discussions-louis.org") "%^t%?")
     ("s" "Templates for Seminars")
     ("sm" "MathInFluids" entry (file+olp "~/owncloud/org/core/seminars.org" "MathInFluids")
      "* %(i-nonempty)%?\n%^t\n%a")
     ("sg" "GFDiscussions" entry (file+olp "~/owncloud/org/core/seminars.org" "GFDiscussions")
      "* %?\n%^t")
     ("t" "TODO list" entry (file+olp "~/owncloud/org/core/todo.org" "Tasks") "* TODO %?\n%(i-nonempty)%a")
     ("a" "answer mail" entry (file "~/owncloud/org/core/mail.org")
      "* TODO Answer %:fromname: %a\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))" :immediate-finish t)
     ("c" "conference" entry (file "~/owncloud/org/core/conferences.org") "* %:subject%?\n%^{LOCATION}p%^{Beginning}t--%^{End}t\n%(i-nonempty)%a")
     ;;("c" "conference" entry (file+datetree+prompt "~/owncloud/org/core/conferences.org") "* %:subject%?\n%^{LOCATION}p%^{Beginning}t--%^{End}t\n%(i-nonempty)%a" :tree-type 'month)
     ("r" "reviews" entry (file "~/owncloud/org/core/reviews.org") "* TODO Review %?\nDEADLINE: %^t\n%a")
     ("l" "reading list" entry (file "~/owncloud/org/core/reads.org") "* TODO %?\n")
     ("?" "random questions" entry (file "~/owncloud/org/core/questions.org") "* TODO %?\n")
     ("~" "working from home" plain (file "~/owncloud/org/core/wfh.org") "%^t%?\n")
     ;;("#" "used by gnus-icalendar-org" entry (file+olp "~/owncloud/org/core/meetings.org" "iCalendar Invites") "* %i\n" :immediate-finish t)
     ))
  :config
  (defun i-nonempty ()
    (let ((initial (plist-get org-store-link-plist :initial)))
    (if (equal initial "")
        ""
      (concat initial "\n"))))
  (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t) (latex . t) (python . t)))
  (add-to-list 'org-src-lang-modes '("latex" . latex))
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
  (defun ch/org-archive-mail-reply-tasks (_)
    "Archive all tasks marked as DONE in mail.org file" ;; in the future perhaps only do so for tasks which are more than a week old
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
     "/DONE|CANCELLED" '("~/owncloud/org/core/mail.org")))
  (advice-add 'save-buffers-kill-emacs :before 'ch/org-archive-mail-reply-tasks)
  :bind
  (("C-c b" . org-switchb)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link)
   :map org-mode-map
   ("C-x n u" . ch/org-narrow-to-subtree-up))
  )

(use-package org-agenda
  :custom
  (org-agenda-files '("~/owncloud/org/core" "~/owncloud/org/manuscripts" "~/owncloud/org/projects" "~/owncloud/org/discussions" "~/owncloud/org/external"))
  (org-refile-targets '((org-agenda-files :maxlevel . 5)))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-agenda-skip-deadline-prewarning-if-scheduled t)
  (org-agenda-todo-ignore-deadlines 'near)
  (org-agenda-todo-ignore-scheduled 'all)
  (org-agenda-prefix-format '((agenda . " %i %-15:c%?-12t% s") (todo . " %i %-15:c") (tags . " %i %-15:c") (search . " %i %-15:c")))
  (org-agenda-custom-commands
   '(("f" "Agenda and TODO by priority"
      ((agenda "")
       (alltodo ""
		((org-agenda-files '("~/owncloud/org/projects"))
		 (org-agenda-overriding-header "Projects:")))
       (alltodo ""
		((org-agenda-files '("~/owncloud/org/core/todo.org"))
		 (org-agenda-overriding-header "Tasks:")))))
     ("w" "Agenda and WAIT items"
      ((agenda "")
       (todo "WAIT")))
     ("r" "Reading list" alltodo ""
      ((org-agenda-files '("~/owncloud/org/core/reads.org" "~/owncloud/org/notes/"))))
     ("c" "Conferences Overview" agenda ""
      ((org-agenda-files '("~/owncloud/org/core/conferences.org"))
       (org-agenda-show-all-dates 'nil)
       (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))
       (org-agenda-span 'year)
       ))
     ))
  (org-agenda-day-face-function
   (defun ch/org-agenda-day-face-holidays-function (date)
     "Use weekend face if DATE is a holiday."
     (let ((holiday nil))
       (dolist (entry (org-agenda-get-day-entries "~/owncloud/org/core/holidays.org" date))
	 (let ((category (get-text-property 0 'org-category entry)))
           (when (string= "Holidays" category)
	     (setq holiday t))))
       (when holiday 'org-agenda-date-weekend))))
  (org-agenda-cmp-user-defined
   (defun ch/org-agenda-heading-entries (entrya entryb)
     "Define a comparison to put some entries first in agenda view"
     (let ((catlist '("WorkFromHome" "Holidays"))
	   (categorya (get-text-property 0 'org-category entrya))
	   (categoryb (get-text-property 0 'org-category entryb)))
       (cond ((member categorya catlist)
	      (cond ((not (member categoryb catlist)) 1)
		    (t 'nil)))
	     ((member categoryb catlist)
	      (cond ((not (member categorya catlist)) -1)
		    (t 'nil)))
	     (t 'nil)))))
  (org-agenda-sorting-strategy
   '((agenda user-defined-down habit-down time-up priority-down category-keep)
     (todo priority-down category-keep)
     (tags priority-down category-keep)
     (search category-keep)))
  :config
  ;; Add org-mode TODOs to appointments: at startup, every day at midnight, and when saving todo file
  (add-hook 'org-agenda-mode-hook #'org-agenda-to-appt)
  (run-at-time "12:05am" (* 24 3600) 'org-agenda-to-appt)
  ;; the following code does not capture agenda files if they are in a directory which is in org-agenda-files.
  (add-hook 'after-save-hook (lambda ()
			       (if (member buffer-file-name (mapcar #'expand-file-name org-agenda-files))
				   (org-agenda-to-appt))))
  (when (display-graphic-p)
    (setq org-agenda-category-icon-alist `(("todo" ,(list (all-the-icons-material "check_box" :height 1.1)) nil nil :ascent center)
					   ("mail" ,(list (all-the-icons-material "mail" :height 1.1)) nil nil :ascent center)
					   ("manuscripts" ,(list (all-the-icons-faicon "pencil" :height 1.1)) nil nil :ascent center)
					   ("ecology" ,(list (all-the-icons-faicon "leaf" :height 1.1)) nil nil :ascent center)
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
					   ("WorkFromHome" ,(list (all-the-icons-faicon "home" :height 1.1)) nil nil :ascent center)
					   )))
  ;; Hydra for org agenda (adapted from https://github.com/abo-abo/hydra/wiki/Org-clock-and-timers)
  (defhydra hydra-org-agenda (:pre (setq which-key-inhibit t) :post (setq which-key-inhibit nil) :hint none)
    "
Org agenda (_q_uit)

^Clock^      ^Visit entry^              ^Date^             ^Other^
^-----^----  ^-----------^------------  ^----^-----------  ^-----^---------
_ci_ in      _SPC_ in other window      _ds_ schedule      _g_ reload
_co_ out     _TAB_ & go to location     _dd_ set deadline  _._ go to today
_cq_ cancel  _RET_ & del other windows  _dt_ timestamp     _j_ go to date
_cj_ jump    _o_   link                 _+_  do later      _s_ save
^^           ^^                         _-_  do earlier    _k_ capture
^^           ^^                         ^^                 ^^
^View^          ^Filter^                 ^Headline^         ^Toggle mode^
^----^--------  ^------^---------------  ^--------^-------  ^-----------^----
_vd_ day        _ft_ by tag              _ht_ set status    _tf_ follow
_vw_ week       _fr_ refine by tag       _hk_ kill          _tl_ log
_vt_ fortnight  _fc_ by category         _hw_ refile        _ta_ archive trees
_vm_ month      _fh_ by top headline     _hA_ archive       _tA_ archive files
_vy_ year       _fx_ by regexp           _h:_ set tags      _tr_ clock report
_vn_ next span  _fe_ by effort           _he_ set effort    _td_ diaries
_vp_ prev span  _fd_ delete all filters  _hp_ set priority  ^^
_vr_ reset      ^^                       _h+_ priority up   ^^
_vc_ column     ^^                       _h-_ priority down ^^
^^              ^^                       ^^                 ^^
"
    ;; Entry
    ("hA" org-agenda-archive-default)
    ("hk" org-agenda-kill)
    ("hp" org-agenda-priority)
    ("h+" org-agenda-priority-up) ;; original key is either "+" or "S-<up>"
    ("h-" org-agenda-priority-down) ;; original key is either "-" or "S-<down>"
    ("hw" org-agenda-refile)
    ("h:" org-agenda-set-tags)
    ("he" org-agenda-set-effort)
    ("ht" org-agenda-todo)
    ;; Visit entry
    ("o"   org-agenda-open-link :exit t)
    ("<tab>" org-agenda-goto :exit t)
    ("TAB" org-agenda-goto :exit t)
    ("SPC" org-agenda-show-and-scroll-up)
    ("RET" org-agenda-switch-to :exit t)
    ;; Date
    ("dt" org-agenda-date-prompt)
    ("dd" org-agenda-deadline)
    ("+" org-agenda-do-date-later)
    ("-" org-agenda-do-date-earlier)
    ("ds" org-agenda-schedule)
    ;; View
    ("vd" org-agenda-day-view)
    ("vw" org-agenda-week-view)
    ("vt" org-agenda-fortnight-view)
    ("vm" org-agenda-month-view)
    ("vy" org-agenda-year-view)
    ("vn" org-agenda-later) ;; default key is "f"
    ("vp" org-agenda-earlier) ;; default key is "b"
    ("vr" org-agenda-reset-view)
    ("vc" org-agenda-columns)
    ;; Toggle mode
    ("ta" org-agenda-archives-mode)
    ("tA" (org-agenda-archives-mode 'files))
    ("tr" org-agenda-clockreport-mode)
    ("tf" org-agenda-follow-mode)
    ("tl" org-agenda-log-mode)
    ("td" org-agenda-toggle-diary)
    ;; Filter
    ("fc" org-agenda-filter-by-category)
    ("fx" org-agenda-filter-by-regexp)
    ("fe" org-agenda-filter-by-effort)
    ("ft" org-agenda-filter-by-tag)
    ("fr" org-agenda-filter-by-tag-refine)
    ("fh" org-agenda-filter-by-top-headline)
    ("fd" org-agenda-filter-remove-all)
    ;; Clock
    ("cq" org-agenda-clock-cancel)
    ("cj" org-agenda-clock-goto :exit t)
    ("ci" org-agenda-clock-in :exit t)
    ("co" org-agenda-clock-out)
    ;; Other
    ("q" nil :exit t)
    ("j" org-agenda-goto-date)
    ("." org-agenda-goto-today)
    ("g" org-agenda-redo)
    ("s" org-agenda-save-all-org-buffers)
    ("k" org-agenda-capture))
  :bind
  (("C-c a" . org-agenda)
   :map org-agenda-mode-map
   ("@" . hydra-org-agenda/body))
  )

(use-package org-agenda-property
  :ensure t
  :custom
  (org-agenda-property-list '("LOCATION"))
  (org-agenda-property-position 'next-line)
  )

(use-package org-bullets
  :ensure t
  :after org
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))

(use-package org-appear
  :ensure t
  :after org
  :custom
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t)
  :hook (org-mode . org-appear-mode))

(use-package org-fragtog
  :ensure t
  :after org
  :custom
  (org-startup-with-latex-preview t)
  :hook
  (org-mode . org-fragtog-mode)
  )


(use-package org-download
  :ensure t
  :after org
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
  :hook
  (org-mode . citar-capf-setup)
  :config
  ;; note: the symbols defined below are only visible when using vertico as the completion framework
  (when (display-graphic-p)
    (defvar citar-indicator-files-icons
      (citar-indicator-create
       :symbol (all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1)
       :function #'citar-has-files
       :padding "  " ; need this because the default padding is too low for these icons
       :tag "has:files"))

    (defvar citar-indicator-links-icons
      (citar-indicator-create
       :symbol (all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01)
       :function #'citar-has-links
       :padding "  "
       :tag "has:links"))

    (defvar citar-indicator-notes-icons
      (citar-indicator-create
       :symbol (all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3)
       :function #'citar-has-notes
       :padding "  "
       :tag "has:notes"))

    (defvar citar-indicator-cited-icons
      (citar-indicator-create
       :symbol (all-the-icons-faicon "circle-o" :face 'all-the-icon-green)
       :function #'citar-is-cited
       :padding "  "
       :tag "is:cited"))
    (setq citar-indicators (list citar-indicator-files-icons citar-indicator-links-icons citar-indicator-notes-icons citar-indicator-cited-icons))
    )
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
   :map org-mode-map :package org ("C-c i" . #'org-cite-insert)
   ))

(use-package citar-embark
  :ensure t
  :after citar embark
  :config
  (citar-embark-mode)
  )

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/owncloud/org"))
  (org-roam-node-display-template (concat "${title:60} " (propertize "${tags:*}" 'face 'org-tag)))
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(
     ("d" "default" plain "%?" :target (file+head "roam/${slug}-%<%Y%m%d%H%M%S>.org" "#+title: ${title}\n#+author: Corentin Herbert\n#+date_added: %u\n#+last_modified: %U")
      :unnarrowed t)
     ("p" "project" plain "%?" :target (file+head "projects/${slug}.org" "#+title: ${title}\n#+author: Corentin Herbert\n#+category: projects\n#+date_added: %u\n#+last_modified: %U\n#+filetags: project")
      :unnarrowed t)
     ("m" "manuscript" plain "%?" :target (file+head "manuscripts/${slug}.org" "#+title: ${title}\n#+author: Corentin Herbert\n#+category: manuscripts\n#+date_added: %u\n#+last_modified: %U\n#+filetags: manuscript")
      :unnarrowed t)
     ))
  (org-roam-file-exclude-regexp '("data/" "external/" "core/" "discussions/"))
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n F p" . ch/org-roam-node-find-project)
   ("C-c n F m" . ch/org-roam-node-find-manuscript)
   ("C-c n F r" . ch/org-roam-node-find-roam)
   ("C-c n I p" . ch/org-roam-node-insert-project)
   ("C-c n I m" . ch/org-roam-node-insert-manuscript)
   ("C-c n I r" . ch/org-roam-node-insert-roam)
   ("C-c n a" . org-roam-alias-add))
  :config
  (org-roam-db-autosync-mode)
  (setq org-roam-db-node-include-function
      (lambda ()
        (not (member "ATTACH" (org-get-tags)))))
  (defun ch/org-roam-node-find-project ()
    "Find and open an Org-roam project node by its title or alias."
    (interactive)
    (org-roam-node-find nil nil (lambda (node) (member "project" (org-roam-node-tags node)))))
  (defun ch/org-roam-node-find-manuscript ()
    "Find and open an Org-roam manuscript node by its title or alias."
    (interactive)
    (org-roam-node-find nil nil (lambda (node) (member "manuscript" (org-roam-node-tags node)))))
  (defun ch/org-roam-get-parent-dir-name (node-file-path)
    (let* ((org-roam-subdir-alist (seq-map (lambda (path) (cons path (f-filename path))) (f-directories org-roam-directory))))
      (cl-loop for (path . name) in org-roam-subdir-alist
               when (s-prefix? path node-file-path)
               return name)))
  (defun ch/org-roam-node-find-roam ()
    "Find and open an Org-roam general node by its title or alias."
    (interactive)
    (org-roam-node-find nil nil (lambda (node) (string-equal "roam" (ch/org-roam-get-parent-dir-name (org-roam-node-file node)))))
    )
  (defun ch/org-roam-node-insert-project ()
    "Find an Org-roam project node and insert (where the point is) an \"id:\" link to it."
    (interactive)
    (org-roam-node-insert (lambda (node) (member "project" (org-roam-node-tags node)))))
  (defun ch/org-roam-node-insert-manuscript ()
    "Find an Org-roam manuscript node and insert (where the point is) an \"id:\" link to it."
    (interactive)
    (org-roam-node-insert (lambda (node) (member "manuscript" (org-roam-node-tags node)))))
  (defun ch/org-roam-node-insert-roam ()
    "Find an Org-roam general node and insert (where the point is) an \"id:\" link to it."
    (interactive)
    (org-roam-node-insert (lambda (node) (string-equal "roam" (ch/org-roam-get-parent-dir-name (org-roam-node-file node))))))
  )

(use-package org-roam-ui
  :ensure t
  :bind ("C-c n u" . org-roam-ui-mode)
  :if (>= emacs-major-version 27))

(use-package consult-org-roam
  :ensure t
  :after org-roam
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
