;;; doom-solarized-dark-custom-theme.el --- a dark variant of Solarized -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: July 15, 2019 (#303)
;; Author: ema2159 <https://github.com/ema2159>
;; Maintainer:
;; Source: https://github.com/bbatsov/solarized-emacs
;; Source: https://ethanschoonover.com/solarized
;;
;;; Commentary:
;; CH: customized version of the doom-solarized-dark theme.
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-solarized-dark-custom-theme nil
  "Options for the `doom-solarized-dark-custom' theme."
  :group 'doom-themes)

(defcustom doom-solarized-dark-custom-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-solarized-dark-custom-theme
  :type 'boolean)

(defcustom doom-solarized-dark-custom-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-solarized-dark-custom-theme
  :type 'boolean)

(defcustom doom-solarized-dark-custom-brighter-text nil
  "If non-nil, default text will be brighter."
  :group 'doom-solarized-dark-custom-theme
  :type 'boolean)

(defcustom doom-solarized-dark-custom-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-solarized-dark-custom-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-solarized-dark-custom
  "A dark theme inspired by VS Code Solarized Dark"

  ;; name        default   256       16
  ((bg         '("#002b36" "#002b36" "brightwhite" ))
   (fg         (if doom-solarized-dark-custom-brighter-text
                   '("#BBBBBB" "#BBBBBB" "brightwhite")
                 '("#839496" "#839496" "brightwhite")))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#00212B" "#00212B" "white"       ))
   (fg-alt     '("#657b83" "#657b83" "white"       ))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#073642" "#073642" "black"       ))
   (base1      '("#03282F" "#03282F" "brightblack" ))
   (base2      '("#00212C" "#00212C" "brightblack" ))
   (base3      '("#13383C" "#13383C" "brightblack" ))
   (base4      '("#56697A" "#56697A" "brightblack" ))
   (base5      '("#405A61" "#405A61" "brightblack" ))
   (base6      '("#96A7A9" "#96A7A9" "brightblack" ))
   (base7      '("#788484" "#788484" "brightblack" ))
   (base8      '("#626C6C" "#626C6C" "white"       ))
   (brightwhite '("#fff6e3" "#fff6e3" "brightwhite"))

   (grey       base4)
   (red        '("#dc322f" "#ff6655" "red"          ))
   (orange     '("#cb4b16" "#dd8844" "brightred"    ))
   (green      '("#859900" "#99bb66" "green"        ))
   (teal       '("#35a69c" "#33aa99" "brightgreen"  ))
   (yellow     '("#b58900" "#ECBE7B" "yellow"       ))
   (blue       '("#268bd2" "#51afef" "brightblue"   ))
   (dark-blue  '("#3F88AD" "#2257A0" "blue"         ))
   (magenta    '("#d33682" "#c678dd" "magenta"      ))
   (violet     '("#6c71c4" "#a9a1e1" "brightmagenta"))
   (cyan       '("#2aa198" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#204052" "#5699AF" "cyan"         ))

   ;; Solarized colors
   ;; base03    #002b36  8/4 brblack  234 #1c1c1c 15 -12 -12   0  43  54 193 100  21
   ;; base02    #073642  0/4 black    235 #262626 20 -12 -12   7  54  66 192  90  26
   ;; base01    #586e75 10/7 brgreen  240 #585858 45 -07 -07  88 110 117 194  25  46
   ;; base00    #657b83 11/7 bryellow 241 #626262 50 -07 -07 101 123 131 195  23  51
   ;; base0     #839496 12/6 brblue   244 #808080 60 -06 -03 131 148 150 186  13  59
   ;; base1     #93a1a1 14/4 brcyan   245 #8a8a8a 65 -05 -02 147 161 161 180   9  63
   ;; base2     #eee8d5  7/7 white    254 #e4e4e4 92 -00  10 238 232 213  44  11  93
   ;; base3     #fdf6e3 15/7 brwhite  230 #ffffd7 97  00  10 253 246 227  44  10  99
   ;; yellow    #b58900  3/3 yellow   136 #af8700 60  10  65 181 137   0  45 100  71
   ;; orange    #cb4b16  9/3 brred    166 #d75f00 50  50  55 203  75  22  18  89  80
   ;; red       #dc322f  1/1 red      160 #d70000 50  65  45 220  50  47   1  79  86
   ;; magenta   #d33682  5/5 magenta  125 #af005f 50  65 -05 211  54 130 331  74  83
   ;; violet    #6c71c4 13/5 brmagenta 61 #5f5faf 50  15 -45 108 113 196 237  45  77
   ;; blue      #268bd2  4/4 blue      33 #0087ff 55 -10 -45  38 139 210 205  82  82
   ;; cyan      #2aa198  6/6 cyan      37 #00afaf 60 -35 -05  42 161 152 175  74  63
   ;; green     #859900  2/2 green     64 #5f8700 60 -20  65 133 153   0  68 100  60

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.5))
   (selection      dark-blue)
   (builtin        blue)
   (comments       (if doom-solarized-dark-custom-brighter-comments blue base5))
   (doc-comments   teal)
   (constants      magenta)
   (functions      blue)
   (keywords       green)
   (methods        cyan)
   (operators      orange)
   (type           yellow)
   (strings        cyan)
   (variables      violet)
   (numbers        magenta)
   (region         base0)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    yellow)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (-modeline-bright doom-solarized-dark-custom-brighter-modeline)
   (-modeline-pad
    (when doom-solarized-dark-custom-padded-modeline
      (if (integerp doom-solarized-dark-custom-padded-modeline) doom-solarized-dark-custom-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        base3
      `(,(doom-darken (car bg) 0.1) ,@(cdr base0))))
   (modeline-bg-alt
    (if -modeline-bright
        base3
      `(,(doom-darken (car bg) 0.15) ,@(cdr base0))))
   (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-alt (doom-darken bg 0.1)))


  ;;;; Base theme face overrides
  (((font-lock-comment-face &override)
    :background (if doom-solarized-dark-custom-brighter-comments (doom-lighten bg 0.05)))
   ((font-lock-keyword-face &override) :weight 'bold)
   ((font-lock-constant-face &override) :weight 'bold)
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))

   (completions-common-part :foreground magenta :background base0)
   (dired-directory :foreground violet :weight 'bold)
   (dired-symlink :foreground magenta)
   (dired-special :foreground cyan)
   (dired-marked :foreground red)
   (dired-broken-symlink :background orange :foreground brightwhite)
   (all-the-icons-dired-dir-face :foreground cyan)

   ;;;; centaur-tabs
   (centaur-tabs-active-bar-face :background blue)
   (centaur-tabs-modified-marker-selected
    :inherit 'centaur-tabs-selected :foreground blue)
   (centaur-tabs-modified-marker-unselected
    :inherit 'centaur-tabs-unselected :foreground blue)
   ;;;; company
   (company-tooltip-selection     :background dark-cyan)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background blue)
   (doom-modeline-host :foreground magenta)
   (doom-modeline-evil-emacs-state  :foreground magenta)
   (doom-modeline-evil-insert-state :foreground blue)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; helm
   (helm-selection :inherit 'bold
                   :background selection
                   :distant-foreground bg
                   :extend t)
   ;;;; latex
   (font-latex-sectioning-2-face :inherit 'font-latex-sectioning-3-face :foreground orange :height 1.1)
   (font-latex-sectioning-3-face :inherit 'font-latex-sectioning-4-face :foreground yellow :height 1.1)
   (font-latex-sectioning-4-face :inherit 'font-latex-sectioning-5-face :foreground blue :height 1.1)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-url-face    :foreground teal :weight 'normal)
   (markdown-reference-face :foreground base6)
   ((markdown-bold-face &override)   :foreground fg)
   ((markdown-italic-face &override) :foreground fg-alt)
   ;;;; outline <built-in>
   ((outline-1 &override) :foreground blue)
   ((outline-2 &override) :foreground green)
   ((outline-3 &override) :foreground teal)
   ((outline-4 &override) :foreground (doom-darken blue 0.2))
   ((outline-5 &override) :foreground (doom-darken green 0.2))
   ((outline-6 &override) :foreground (doom-darken teal 0.2))
   ((outline-7 &override) :foreground (doom-darken blue 0.4))
   ((outline-8 &override) :foreground (doom-darken green 0.4))
   ;;;; org <built-in>
   ((org-block &override) :background base0)
   ((org-block-begin-line &override) :foreground comments :background base0)
   (org-mode-line-clock :foreground green)
   (org-mode-line-clock-overrun :foreground "#d75f00" :weight 'bold)
   (org-scheduled-previously :foreground red :weight 'normal)
   (org-scheduled-today :foreground yellow :weight 'normal)
   (org-date-selected :background yellow :foreground brightwhite)
   ;;;; org-roam
   (org-roam-header-line :foreground violet :weight 'bold)
   (org-roam-title :foreground magenta :weight 'bold)
   (org-roam-olp :foreground base1)
   ;;;; orderless
   (orderless-match-face-0 :foreground magenta :background region)
   (orderless-match-face-1 :foreground cyan :background region)
   (orderless-match-face-2 :foreground yellow :background region)
   (orderless-match-face-3 :foreground green :background region)
   ;;;; magit
   (magit-header-line :foreground "#d75f00" :background "#eee8d5" :weight 'bold
		      :box `(:line-width 3 :color "#93a1a1"))
   (magit-diff-file-heading :foreground yellow :weight 'bold)
   (magit-branch-local :foreground blue)
   (magit-branch-remote :foreground magenta)
   ;;;; treemacs
   (treemacs-git-modified-face :inherit 'magit-diff-file-heading)
   (treemacs-git-untracked-face :inherit 'magit-filename)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt)))

   (flycheck-error-list-line-number :foreground violet)
   (flycheck-error-list-column-number :foreground violet)
   (flycheck-error-list-filename :foreground cyan)

   (help-key-binding :inherit 'fixed-pitch :background bg-alt :foreground violet
		     :box `(:line-width 1 :color ,base5))
   (bookmark-menu-bookmark :weight 'bold :foreground magenta)

   )

  ;;;; Base theme variable overrides-
  ;; ()
  )

;;; doom-solarized-dark-custom-theme.el ends here
