;;; dbd-org-mode.el -- dbd org mode define
;; follow http://pages.sachachua.com/.emacs.d/Sacha.html#unnumbered-6
;; http://members.optusnet.com.au/~charles57/GTD/gtd_workflow.html


(provide 'dbd-org-mode)
(require 'cl-lib)
(require 'org)

(defvar dbd-org-mode-hook nil)

;;;###autoload
(define-derived-mode dbd-org-mode org-mode "dbd-org-mode"
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
                      org-toc))
  (org-load-modules-maybe t)
  (setq org-expiry-inactive-timestamps t)
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
  (add-to-list 'org-global-properties
               '("Effort_ALL". "0:05 0:15 0:30 1:00 2:00 3:00 4:00"))
  )

;; (add-hook 'org-mode-hook 'dbd-org-mode)

;;; end dbd-org-mode.el
