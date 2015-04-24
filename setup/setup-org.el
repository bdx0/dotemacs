;;; setup-org.el --- configuration for orgmode
;; Author: dbd
;; Version: 0.0.1

;;; Commentary:
;;
;;

;;; Code:

(require 'ox-rss nil t)
(require 'ox-reveal nil t)
(require 'ox-beamer nil t)
(require 'ox-latex nil t)
(require 'ox-odt nil t)
(require 'org-gnus nil t)
(require 'ox-koma-letter nil t)
(require 'cl-lib)
(require 'bind-key)
(setq org-modules '(org-bbdb
                    org-gnus
                    org-drill
                    org-info
                    org-jsinfo
                    org-habit
                    org-irc
                    org-mouse
                    org-annotate-file
                    org-eval
                    org-expiry
                    org-interactive-query
                    org-man
                    org-panel
                    org-screen
                    org-info
                    org-timer
                    org-toc))
;; (org-load-modules-maybe t)
(setq org-expiry-inactive-timestamps t)
(bind-key "C-c o r" 'org-capture)
(bind-key "C-c o a" 'org-agenda)
(bind-key "C-c o l" 'org-store-link)
(bind-key "C-c o L" 'org-insert-link-global)
(bind-key "C-c o O" 'org-open-at-point-global)
(bind-key "<f9> <f9>" 'org-agenda-list)
(bind-key "<f9> <f8>" (lambda () (interactive) (org-capture nil "r")))
(bind-key "C-TAB" 'org-cycle)
(bind-key "C-c o s t t" 'org-show-todo-tree)
(bind-key "C-c o r" 'org-refile)
(bind-key "C-c o R" 'org-reveal)
(setq org-todo-keywords
      '((sequence
         "TODO(t)"  ; next action
         "TOBLOG(b)"  ; next action
         "STARTED(s)"
         "WAITING(w@/!)"
         "SOMEDAY(.)" "|" "DONE(x!)" "CANCELLED(c@)")
        (sequence "TOSKETCH" "SKETCHED" "|" "POSTED")
        (sequence "TODELEGATE(-)" "DELEGATED(d)" "|" "COMPLETE(x)")))
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "green" :weight bold))
        ("DONE" . (:foreground "cyan" :weight bold))
        ("WAITING" . (:foreground "red" :weight bold))
        ("SOMEDAY" . (:foreground "gray" :weight bold))))
(setq org-tag-alist '(("@work" . ?b)
                      ("@home" . ?h)
                      ("@writing" . ?w)
                      ("@errands" . ?e)
                      ("@drawing" . ?d)
                      ("@coding" . ?c)
                      ("@phone" . ?p)
                      ("@reading" . ?r)
                      ("@computer" . ?l)
                      ("quantified" . ?q)
                      ("lowenergy" . ?0)
                      ("highenergy" . ?1)))
(setq org-global-properties
      '(("Effort_ALL". "0:05 0:15 0:30 1:00 2:00 3:00 4:00")))
(global-set-key (kbd "C-M-]") (lambda () (interactive) (org-cycle t)))
(global-set-key (kbd "M-]")
                (lambda () (interactive)
                  (ignore-errors (end-of-defun) (beginning-of-defun)) (org-cycle)))
(defun new-func ()
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (sh . t)
     (dot . t)
     (clojure . t)
     (org . t)
     (ditaa . t)
     (org . t)
     (ledger . t)
     (scheme . t)
     (plantuml . t)
     (R . t)
     (gnuplot . t)))

  (org-clock-persistence-insinuate)

  (add-hook 'org-clock-in-hook (lambda() (org-todo "STRT")))

  (appt-activate t)

  (setq display-time-24hr-format t)
  (setq display-time-day-and-date t)

  (setq appt-audible nil
        appt-display-interval 10
        appt-message-warning-time 120)

  (setq org-babel-default-header-args
        '((:session . "none")
          (:results . "replace")
          (:exports . "code")
          (:cache . "no")
          (:noweb . "yes")
          (:hlines . "no")
          (:tangle . "no")
          (:padnewline . "yes")))

  (setq org-edit-src-content-indentation 0)
  (setq org-babel-clojure-backend 'cider)
  (setq org-agenda-bulk-mark-char "*")
  (setq org-agenda-diary-file "/home/guerry/org/rdv.org")
  (setq org-agenda-dim-blocked-tasks nil)
  (setq org-agenda-entry-text-maxlines 10)
  (setq org-agenda-file-regexp "\\.org\\'")
  (setq org-agenda-files '("~/org/rdv.org" "~/org/bzg.org" "~/org/libre.org"))
  (setq org-agenda-include-diary nil)
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c%?-14t%s")
          (timeline . "  % s")
          (todo . " %i %-14:c")
          (tags . " %i %-14:c")
          (search . " %i %-14:c")))
  (setq org-agenda-remove-tags t)
  (setq org-agenda-restore-windows-after-quit t)
  (setq org-agenda-show-inherited-tags nil)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-deadline-prewarning-if-scheduled t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-timestamp-if-done t)
  (setq org-agenda-sorting-strategy
        '((agenda time-up) (todo time-up) (tags time-up) (search time-up)))
  (setq org-agenda-start-on-weekday 1)
  (setq org-agenda-sticky nil)
  (setq org-agenda-tags-todo-honor-ignore-options t)
  (setq org-agenda-use-tag-inheritance nil)
  (setq org-agenda-window-frame-fractions '(0.0 . 0.5))
  (setq org-agenda-deadline-faces
        '((1.0001 . org-warning)              ; due yesterday or before
          (0.0    . org-upcoming-deadline)))  ; due today or later
  (org-agenda-to-appt)
  (setq org-agenda-custom-commands
        `(
          ;; list of tasks for today
          (" " "Aujourd'hui" agenda "List of rendez-vous and tasks for today"
           ((org-agenda-span 1)
            (org-agenda-files '("~/org/rdv.org" "~/org/bzg.org"))
            (org-deadline-warning-days 3)
            (org-agenda-effort-filter '("+=30"))
            (org-agenda-sorting-strategy
             '(todo-state-up time-up priority-up))))

          ;; list of free tasks for today
          ("+" "Aujourd'hui" agenda "List of core+extra tasks for today"
           ((org-agenda-span 1)
            (org-agenda-files '("~/org/libre.org" "~/org/extra.org"))
            (org-deadline-warning-days 3)
            (org-agenda-sorting-strategy
             '(todo-state-up time-up priority-up))))

          ;; list of WP tasks for today
          ("%" "Rendez-vous" agenda* "Week RDV"
           ((org-agenda-span 'week)
            (org-agenda-files '("~/org/rdv.org"))
            (org-deadline-warning-days 10)
            (org-agenda-sorting-strategy
             '(todo-state-up time-up priority-up))))

          ("n" "NEXT (core)"
           todo "NEXT" ((org-agenda-files '("~/org/bzg.org"))
                        (org-agenda-sorting-strategy '(timestamp-up))))
          ("N" "NEXT (all)"
           todo "NEXT" ((org-agenda-sorting-strategy '(timestamp-up))))
          ("d" "TODO (core)"
           todo "TODO" ((org-agenda-files '("~/org/bzg.org"))
                        (org-agenda-sorting-strategy '(timestamp-up))))
          ("D" "TODO (all)"
           todo "TODO" ((org-agenda-sorting-strategy '(timestamp-up))))
          ("s" "STRT (core)"
           todo "STRT" ((org-agenda-files '("~/org/bzg.org"))
                        (org-agenda-sorting-strategy '(timestamp-up))))
          ("S" "STRT (all)"
           todo "STRT" ((org-agenda-sorting-strategy '(timestamp-up))))

          ("x" "All tasks scheduled today" agenda "List of scheduled tasks for today"
           ((org-agenda-span 1)
            (org-agenda-entry-types '(:timestamp :scheduled))
            (org-agenda-sorting-strategy
             '(time-up todo-state-up priority-up))))

          ;; list of WP tasks for today
          ("X" "Upcoming deadlines" agenda "List of past and upcoming deadlines"
           ((org-agenda-span 1)
            (org-deadline-warning-days 15)
            (org-agenda-entry-types '(:deadline))
            (org-agenda-sorting-strategy
             '(time-up todo-state-up priority-up))))

          ;; list of Old deadlines
          ("Y" tags-todo "+SCHEDULED<=\"<now>\"")
          ("Z" tags-todo "+DEADLINE<=\"<now>\"")

          ("R" tags-todo "+Read+TODO={NEXT}" nil)
          ;; Everything that has a "Read" tag
          ("r" . "Read")
          ("rn" tags-todo "+Read+TODO={NEXT}" nil)
          ("rt" tags-todo "+Read+TODO={TODO}" nil)
          ("rr" tags-todo "+Read+TODO={STRT}" nil)
          ("rF" tags "+Read+@Offline" nil)

          ("W" tags-todo "+Write+TODO={NEXT}" nil)

          ;; Everything that has a "Read" tag
          ("w" . "Write")
          ("wn" tags-todo "+Write+TODO={NEXT}" nil)
          ("wt" tags-todo "+Write+TODO={TODO}" nil)
          ("ww" tags-todo "+Write+TODO={STRT}" nil)
          ("wF" tags "+Write+@Offline" nil)

          ))
  (setq org-capture-templates
        ;; for org/rdv.org
        '(

          ;; Mise, put it on top of my main .org file
          (" " "Misc" entry (file "~/org/bzg.org")
           "* TODO %a\n  :PROPERTIES:\n  :CAPTURED: %U\n  :END:\n\n%i%?"
           :prepend t :immediate-finish t)

          ;; Mise, put it on top of my main .org file
          ("c" "Misc" entry (file "~/org/bzg.org")
           "* TODO %a\n  :PROPERTIES:\n  :CAPTURED: %U\n  :END:\n\n%i%?"
           :prepend t)

          ;; for org/rdv.org
          ("r" "Bzg RDV" entry (file+headline "~/org/rdv.org" "RDV")
           "* %a\n  :PROPERTIES:\n  :CAPTURED: %U\n  :END:\n\n%i%?" :prepend t)

          ;; Basement et garden
          ("g" "Garden" entry (file+headline "~/org/garden.org" "Garden")
           "* TODO %?%a\n  :PROPERTIES:\n  :CAPTURED: %U\n  :END:\n\n%i" :prepend t)

          ;; Boite (lml) et cours
          ("b" "Boulot" entry (file+headline "~/org/bzg.org" "Boulot")
           "* TODO %?%a\n  :PROPERTIES:\n  :CAPTURED: %U\n  :END:\n\n%i" :prepend t)

          ("e" "Emacs" entry (file+headline "~/org/libre.org" "Emacs")
           "* TODO %?%a\n  :PROPERTIES:\n  :CAPTURED: %U\n  :END:\n\n%i" :prepend t)

          ("o" "Org")
          ("of" "Org FR" entry (file+headline "~/org/libre.org" "Features")
           "* TODO %?%a :Code:\n  :PROPERTIES:\n  :CAPTURED: %U\n  :END:\n\n%^{OrgVersion}p%i" :prepend t)
          ("ob" "Org Bug" entry (file+headline "~/org/libre.org" "To fix")
           "* NEXT %?%a :Bug:\n  :PROPERTIES:\n  :CAPTURED: %U\n  :END:\n\n%^{OrgVersion}p%i" :prepend t)
          ("op" "Org Patch" entry (file+headline "~/org/libre.org" "Patches")
           "* NEXT [#A] %?%a :Patch:\n  :PROPERTIES:\n  :CAPTURED: %U\n  :END:\n\n%^{OrgVersion}p%i" :prepend t)
          ("ow" "Worg" entry (file+headline "~/org/libre.org" "Worg")
           "* TODO [#A] %?%a :Worg:\n  :PROPERTIES:\n  :CAPTURED: %U\n  :END:\n\n%i" :prepend t)

          ))
  (setq org-export-default-language "fr")
  (setq org-export-backends '(latex odt icalendar html ascii rss koma-letter))
  (setq org-export-highlight-first-table-line t)
  (setq org-export-html-extension "html")
  (setq org-export-html-with-timestamp nil)
  (setq org-export-skip-text-before-1st-heading nil)
  (setq org-export-with-LaTeX-fragments t)
  (setq org-export-with-archived-trees nil)
  (setq org-export-with-drawers '("HIDE"))
  (setq org-export-with-section-numbers nil)
  (setq org-export-with-sub-superscripts '{})
  (setq org-export-with-tags 'not-in-toc)
  (setq org-export-with-timestamps t)
  (setq org-html-head "")
  (setq org-html-head-include-default-style nil)
  (setq org-export-with-toc nil)
  (setq org-export-with-priority t)
  (setq org-export-dispatch-use-expert-ui nil)
  (setq org-export-babel-evaluate t)
  (setq org-taskjuggler-default-project-duration 2000)
  (setq org-taskjuggler-target-version 3.0)

  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  ;; (add-to-list 'org-latex-packages-alist '("" "listings"))
  ;; (add-to-list 'org-latex-packages-alist '("" "color"))

  (setq org-latex-pdf-process
        '("pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f" "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f" "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"))

  (setq org-export-allow-bind-keywords t)
  (setq org-publish-list-skipped-files nil)

  (setq org-html-table-row-tags
        (cons '(cond (top-row-p "<tr class=\"tr-top\">")
                     (bottom-row-p "<tr class=\"tr-bottom\">")
                     (t (if (= (mod row-number 2) 1)
                            "<tr class=\"tr-odd\">"
                          "<tr class=\"tr-even\">")))
              "</tr>"))

  (add-to-list 'org-latex-classes
               '("my-letter"
                 "\\documentclass\{scrlttr2\}
            \\usepackage[english,frenchb]{babel}
            \[NO-DEFAULT-PACKAGES]
            \[NO-PACKAGES]
            \[EXTRA]"))

  (setq org-pretty-entities t)
  (setq org-fast-tag-selection-single-key 'expert)
  (setq org-fontify-done-headline t)
  (setq org-fontify-emphasized-text t)
  (setq org-footnote-auto-label 'confirm)
  (setq org-footnote-auto-adjust t)
  (setq org-footnote-define-inline nil)
  (setq org-hide-emphasis-markers t)
  (setq org-icalendar-include-todo 'all)
  (setq org-list-indent-offset 0)
  (setq org-link-frame-setup '((gnus . gnus) (file . find-file-other-window)))
  (setq org-link-mailto-program '(browse-url-mail "mailto:%a?subject=%s"))
  (setq org-log-note-headings
        '((done . "CLOSING NOTE %t") (state . "State %-12s %t") (clock-out . "")))
  (setq org-priority-start-cycle-with-default nil)
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 2))
                             (("~/org/garden.org") . (:maxlevel . 2))
                             (("~/org/libre.org") . (:maxlevel . 2))
                             (("~/org/extra.org") . (:maxlevel . 2))
                             ))
  (setq org-refile-use-outline-path t)
  (setq org-refile-allow-creating-parent-nodes t)
  (setq org-refile-use-cache t)
  (setq org-return-follows-link t)
  (setq org-reverse-note-order t)
  (setq org-scheduled-past-days 100)
  (setq org-show-following-heading '((default nil) (occur-tree t)))
  (setq org-show-hierarchy-above '((default nil) (tags-tree . t)))
  (setq org-special-ctrl-a/e 'reversed)
  (setq org-special-ctrl-k t)
  (setq org-stuck-projects '("+LEVEL=1" ("NEXT" "TODO" "DONE")))
  (setq org-tag-persistent-alist '(("Write" . ?w) ("Read" . ?r)))
  (setq org-tag-alist
        '((:startgroup . nil)
          ("Write" . ?w) ("Trad" . ?t) ("Read" . ?r) ("Proofread" . ?f)
          ("View" . ?v) ("Listen" . ?l)
          (:endgroup . nil)
          (:startgroup . nil) ("@Online" . ?O) ("@Offline" . ?F)
          (:endgroup . nil)
          ("Print" . ?P) ("Code" . ?c) ("Patch" . ?p) ("Bug" . ?b)
          ("Twit" . ?i) ("Tel" . ?T) ("Buy" . ?B) ("Doc" . ?d) ("Mail" . ?@)))
  (setq org-tags-column -74)
  (setq org-tags-match-list-sublevels t)
  (setq org-todo-keywords '((type "STRT" "NEXT" "TODO" "WAIT" "|" "DONE" "DELEGATED" "CANCELED")))
  (setq org-use-property-inheritance t)
  (setq org-clock-persist t)
  (setq org-clock-history-length 35)
  (setq org-clock-in-resume t)
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-clock-sound t)
  (setq org-insert-heading-respect-content t)
  (setq org-id-method 'uuidgen)
  (setq org-combined-agenda-icalendar-file "~/org/bzg.ics")
  (setq org-icalendar-combined-name "Bastien Guerry ORG")
  (setq org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo))
  (setq org-icalendar-use-deadline '(todo-due event-if-todo event-if-not-todo))
  (setq org-icalendar-timezone "Europe/Paris")
  (setq org-icalendar-store-UID t)
  (setq org-timer-default-timer 20)
  (setq org-confirm-babel-evaluate nil)
  (setq org-archive-default-command 'org-archive-to-archive-sibling)
  (setq org-clock-idle-time 15)
  (setq org-id-uuid-program "uuidgen")
  ;;    (setq org-modules '(org-bbdb org-bibtex org-docview org-gnus org-id org-protocol org-info org-jsinfo org-irc org-w3m org-taskjuggler org-learn))
  (setq org-modules '(org-bbdb org-bibtex org-docview org-gnus org-protocol org-info org-jsinfo org-irc org-w3m org-taskjuggler org-learn))
  (setq org-use-speed-commands
        (lambda nil
          (and (looking-at org-outline-regexp-bol)
               (not (org-in-src-block-p t)))))
  (setq org-src-tab-acts-natively t)
  (setq org-hide-block-startup t)
  (setq org-highlight-latex-and-related '(latex))
  (setq org-log-into-drawer "LOGBOOK")
  (setq org-goto-auto-isearch nil)
  (setq org-beamer-outline-frame-title "Survol")
  (setq org-image-actual-width 600)
  (setq org-src-fontify-natively t)
  (setq org-todo-keyword-faces '(("STRT" . "Purple")
                                 ("WAIT" . "Gray")
                                 ("CANCELED" . "Gray")))

  (setq org-plantuml-jar-path "~/bin/plantuml.jar")
  (setq org-link-abbrev-alist
        '(("bugzilla" . "http://10.1.2.9/bugzilla/show_bug.cgi?id=")
          ("google"   . "http://www.google.com/search?q=%s")
          ("gnugol"   . "shell:gnugol -o org %s")
          ("gmap"     . "http://maps.google.com/maps?q=%s")
          ("omap"     . "http://nominatim.openstreetmap.org/search?q=%s&polygon=1")))

  (setq org-attach-directory "~/org/data/")
  (setq org-link-display-descriptive nil)
  (setq org-loop-over-headlines-in-active-region t)
  (setq org-create-formula-image-program 'dvipng) ;; imagemagick
  (setq org-allow-promoting-top-level-subtree t)
  (setq org-description-max-indent 5)
  (setq org-gnus-prefer-web-links nil)
  (setq org-html-head-include-default-style nil)
  (setq org-html-head-include-scripts nil)
  (setq org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))
  (setq org-contacts-files '("~/org/contacts.org"))
  (setq org-crypt-key "Bastien Guerry")
  (setq org-enforce-todo-dependencies t)
  (setq org-mobile-directory "~/Dropbox/org/")
  (setq org-mobile-files '("~/Dropbox/org/" "~/org/from-mobile.org"))
  (setq org-fontify-whole-heading-line t)
  (setq org-file-apps
        '((auto-mode . emacs)
          ("\\.mm\\'" . default)
          ("\\.x?html?\\'" . default)
          ("\\.pdf\\'" . "mupdf %s")))
  (setq html-preamble "<div class=\"bg\">
  <a title=\"bzg.fr\" href=\"http://bzg.fr\"><img alt=\"Bastien Guerry\" src=\"u/bg.jpg\" /></a>
</div>

<div id=\"share\">
<!-- Place this tag where you want the share button to render. -->
<div class=\"g-plus\" data-action=\"share\" data-annotation=\"none\"></div>

<!-- Place this tag after the last share tag. -->
<script type=\"text/javascript\">
  (function() {
    var po = document.createElement('script'); po.type = 'text/javascript'; po.async = true;
    po.src = 'https://apis.google.com/js/platform.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(po, s);
  })();
</script>

<br/>

<a href=\"https://twitter.com/share\" class=\"twitter-share-button\" data-via=\"bzg2\">Tweet</a>
<script>!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+'://platform.twitter.com/widgets.js';fjs.parentNode.insertBefore(js,fjs);}}(document, 'script', 'twitter-wjs');</script>

<br/>

<a href=\"https://twitter.com/bzg2\" class=\"twitter-follow-button\" data-show-count=\"false\">Follow @bzg2</a>
<script>!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+'://platform.twitter.com/widgets.js';fjs.parentNode.insertBefore(js,fjs);}}(document, 'script', 'twitter-wjs');</script>
</div>

<div id=\"menu\">
  <a href=\"http://bzg.fr\"><button class=\"btn btn-success\" type=\"button\">Home</button></a>
  <a href=\"/blog.html\"><button class=\"btn btn-warning\" type=\"button\">Blog</button></a>
  <a href=\"/crowdsupport.html\"><button class=\"btn btn-danger\" type=\"button\">Donate</button></a>
  <a href=\"http://bzg.fr/talks.en.html\"><button class=\"btn btn-success\" type=\"button\">Talks</button></a>
</div>

<script src=\"http://www.google-analytics.com/urchin.js\" type=\"text/javascript\"></script>
<script type=\"text/javascript\">
 _uacct = \"UA-2658857-1\";
 urchinTracker();
</script>

<div class=\"bottomrightbutton\">
<a href=\"http://flattr.com/thing/1653281/bzg\" target=\"_blank\"><img src=\"http://api.flattr.com/button/flattr-badge-large.png\" alt=\"Flattr this\" title=\"Flattr this\" border=\"0\" /></a>

<br/>

<a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/3.0/deed.en_US\"><img class=\"flattr\" alt=\"Creative Commons License\" src=\"http://i.creativecommons.org/l/by-sa/3.0/88x31.png\" /></a>
</div>
")

  (setq html-dll-preamble
        "<script>
    \(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
    \(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
    m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
    })(window,document,'script','//www.google-analytics.com/analytics.js','ga');

    ga('create', 'UA-42064173-1', 'dunlivrelautre.net');
    ga('send', 'pageview');
    </script>

    <div class=\"toprightbutton\">
    <a href=\"blog.xml\"><img alt=\"RSS\" width=\"70px\" src=\"u/rss.jpg\" /></a>
    </div>

    <div class=\"topleftbutton\">

    <a href=\"/index.html\">Home</a><br/>

    <a href=\"http://flattr.com/thing/1654106/Dun-Livre-Lautre\" target=\"new\"><img src=\"http://api.flattr.com/button/flattr-badge-large.png\" alt=\"Flattr this\" title=\"Flattr this\" border=\"0\" /></a><br/>

    <a href=\"https://twitter.com/share\" class=\"twitter-share-button\"
    data-count=\"none\" data-via=\"bzg2\" data-lang=\"fr\">Tweeter</a><script
    type=\"text/javascript\" src=\"//platform.twitter.com/widgets.js\"></script>

    </div>

    <div class=\"bottomrightbutton\">
    <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-nc-sa/3.0/deed.en_US\"><img alt=\"Creative Commons License\" style=\"border-width:0\" src=\"http://i.creativecommons.org/l/by-nc-sa/3.0/88x31.png\" /></a>
    </div>
  ")

  (setq html-postamble "
    <script src=\"u/bootstrap.min.js\"></script>")

  (setq org-publish-project-alist
        `(
          ("homepage"
           :base-directory "~/install/git/homepage/"
           :html-extension "html"
           :base-extension "org"
           :publishing-directory "/home/guerry/public_html/org/homepage/"
           :publishing-function (org-html-publish-to-html)
           :auto-sitemap nil
           :recursive t
           :makeindex t
           :preserve-breaks nil
           :sitemap-sort-files chronologically
           :with-tasks nil
           :section-numbers nil
           :with-toc nil
           :html-head-extra
           "<link rel=\"alternate\" type=\"application/rss+xml\" href=\"http://bzg.fr/blog.xml\" title=\"RSS feed for bzg.fr\">
<link rel=\"stylesheet\" href=\"u/bootstrap.min.css\" />
<link rel=\"stylesheet\" href=\"index.css\" type=\"text/css\" />
    <script src=\"http://www.google.com/jsapi\" type=\"text/javascript\"></script>
    <script type=\"text/javascript\">
      google.load(\"jquery\", \"1.3.1\");
    </script>"
           :html-preamble ,html-preamble
           :htmlized-source t
           :html-postamble ,html-postamble)
          ("homepage-sources"
           :base-directory "~/install/git/homepage/"
           :base-extension "org"
           :publishing-directory "/home/guerry/public_html/org/homepage/"
           :publishing-function (org-org-publish-to-org)
           :recursive t
           :with-tasks nil
           :htmlized-source t)
          ("homepage-rss"
           :base-directory "~/install/git/homepage/"
           :base-extension "org"
           :html-link-home "http://bzg.fr/"
           :publishing-directory "/home/guerry/public_html/org/homepage/"
           :publishing-function (org-rss-publish-to-rss)
           :html-link-use-abs-url t
           :section-numbers nil
           :exclude ".*"
           :with-tasks nil
           :include ("blog.org")
           :with-toc nil)
          ("clojure-rss"
           :base-directory "~/install/git/homepage/"
           :base-extension "org"
           :html-link-home "http://bzg.fr/"
           :publishing-directory "/home/guerry/public_html/org/homepage/"
           :publishing-function (org-rss-publish-to-rss)
           :html-link-use-abs-url t
           :section-numbers nil
           :exclude ".*"
           :with-tasks nil
           :include ("clojure.org")
           :with-toc nil)
          ("homepage-css"
           :base-directory "~/install/git/homepage"
           :base-extension "css"
           :publishing-directory "/home/guerry/public_html/org/homepage/"
           :publishing-function org-publish-attachment)
          ("homepage-attachments"
           :base-directory "~/install/git/homepage"
           :base-extension "png\\|jpg\\|gif\\|atom"
           :publishing-directory "/home/guerry/public_html/org/homepage/u/"
           :publishing-function org-publish-attachment)

          ("dotemacs"
           :base-directory "~/install/git/dotemacs/"
           :html-extension "html"
           :base-extension "org"
           :publishing-directory "/home/guerry/public_html/org/homepage/"
           :publishing-function (org-html-publish-to-html)
           :auto-sitemap nil
           :recursive t
           :makeindex nil
           :preserve-breaks nil
           :sitemap-sort-files chronologically
           :section-numbers nil
           :with-toc nil
           :html-head-extra
           "<link rel=\"stylesheet\" href=\"u/bootstrap.min.css\" />
<link rel=\"stylesheet\" href=\"index.css\" type=\"text/css\" />"
           :html-preamble ,html-preamble
           :html-postamble ,html-postamble
           :htmlized-source nil
           :html-postamble nil)

          ("faqrel"
           :base-directory "~/install/git/faqrel/"
           :html-extension "html"
           :base-extension "org"
           :publishing-directory "/home/guerry/public_html/org/homepage/"
           :publishing-function (org-html-publish-to-html)
           :auto-sitemap nil
           :recursive t
           :makeindex nil
           :preserve-breaks nil
           :sitemap-sort-files chronologically
           :section-numbers nil
           :with-toc nil
           :html-head-extra
           "<link rel=\"stylesheet\" href=\"index.css\" type=\"text/css\" />"
           :html-preamble ,html-preamble
           :htmlized-source nil
           :html-postamble ,html-postamble)

          ("dll"
           :base-directory "~/install/git/dunlivrelautre/"
           :html-extension "html"
           :base-extension "org"
           :publishing-directory "/home/guerry/public_html/org/dunlivrelautre/"
           :publishing-function (org-html-publish-to-html)
           :auto-sitemap nil
           :recursive t
           :with-tasks nil
           :makeindex t
           :preserve-breaks nil
           :sitemap-sort-files chronologically
           :section-numbers nil
           :with-toc nil
           :html-head-extra "<link rel=\"stylesheet\" href=\"index.css\" type=\"text/css\" />"
           :html-postamble nil
           :htmlized-source nil
           :html-preamble ,html-dll-preamble)
          ("dll-rss"
           :base-directory "~/install/git/dunlivrelautre/"
           :base-extension "org"
           :html-link-home "http://www.dunlivrelautre.net"
           :publishing-directory "/home/guerry/public_html/org/dunlivrelautre/"
           :publishing-function (org-rss-publish-to-rss)
           :html-link-use-abs-url t
           :section-numbers nil
           :exclude ".*"
           :include ("blog.org")
           :with-tasks nil
           :with-toc nil)
          ("dll-css"
           :base-directory "~/install/git/dunlivrelautre"
           :base-extension "css"
           :publishing-directory "/home/guerry/public_html/org/dunlivrelautre/"
           :publishing-function org-publish-attachment)
          ("dll-attachments"
           :base-directory "~/install/git/dunlivrelautre"
           :base-extension "png\\|jpg\\|gif\\|xml\\|atom"
           :publishing-directory "/home/guerry/public_html/org/dunlivrelautre/"
           :publishing-function org-publish-attachment)

          ;; Meta projects
          ("hp" :components
           ("homepage" "homepage-sources" "homepage-attachments" "homepage-rss" "clojure-rss" "homepage-css"))
          ("dll" :components ("dll" "dll-attachments" "dll-rss"))
          ))

  (setq org-export-htmlize-output-type 'css)

  (setq org-export-filter-planning-functions
        '(my-org-html-export-planning))

  (defun my-org-html-export-planning (planning-string backend info)
    (when (string-match "<p>.+><\\([0-9]+-[0-9]+-[0-9]+\\)[^>]+><.+</p>" planning-string)
      (concat "<span class=\"planning\">" (match-string 1 planning-string) "</span>")))
  ;; Generic / unsorted
  (setq org-hide-leading-stars t)
  (setq org-reveal-theme "night")
  (setq org-global-properties
        '(("Effort_ALL" .
           "0 0:10 0:20 0:30 0:40 0:50 1:00 1:30 2:00 2:30 3:00 4:00 5:00 6:00 7:00 8:00")
          ("Progress_ALL" . "10% 20% 30% 40% 50% 60% 70% 80% 90%")
          ("Status_ALL" . "Work Leisure GTD WOT")))

  (setq org-confirm-elisp-link-function nil)
  (setq org-confirm-shell-link-function nil)
  (setq org-context-in-file-links t)
  (setq org-cycle-include-plain-lists nil)
  (setq org-deadline-warning-days 7)
  (setq org-default-notes-file "~/org/notes.org")
  (setq org-directory "~/org/")
  (setq org-ellipsis nil)
  (setq org-email-link-description-format "%c: %.50s")
  (setq org-support-shift-select t)
  (defun org-dblock-write:fb_like (params)
    (let ((url (concat "http://bzg.fr/"
                       (file-name-sans-extension (file-name-nondirectory
                                                  (buffer-file-name)))
                       ".html")))
      (insert (format
               "#+HTML: <div class=\"fb-like\" data-href=\"%s\" data-send=\"true\" data-width=\"450\" data-show-faces=\"false\"></div>"
               url))))
  )
