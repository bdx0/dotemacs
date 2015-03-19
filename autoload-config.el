;;; autoload.el -- pre-define some function when load feature

(with-eval-after-load "recentf"
                      (require 'recentf)
                      (recentf-mode 1)
                      (setq recentf-exclude '("~$"))
                      (setq recentf-max-saved-items 1000)
                      ;; get rid of `find-file-read-only' and replace it with something
                      ;; more useful.
                      (global-set-key (kbd "C-S-t") 'ido-recentf-open)
                      (global-set-key (kbd "C-c r") 'query-recentf)                      )

(with-eval-after-load "ido"
                      (require 'ido nil t)
                      (ido-mode)
                      (setq ido-enable-flex-matching t)
                      )

(with-eval-after-load "tabbar"
                      (require 'tabbar)
                      (tabbar-mode 1)
                      ;; set shortcut for tabbar mode
                      (global-set-key [M-S-left] 'tabbar-backward)
                      (global-set-key [M-S-right] 'tabbar-forward)
                      (global-set-key (kbd "<C-tab>") 'bury-buffer)
                      (global-set-key [M-S-up] 'next-buffer)
                      (global-set-key [M-S-down] 'previous-buffer))

;; {{{ Helm
;; helm-recentf: Enabled by setting helm-recentf-fuzzy-match to t.
;; helm-mini: Enable by setting helm-buffers-fuzzy-matching and helm-recentf-fuzzy-match to t.
;; helm-buffers-list: Enable by setting helm-buffers-fuzzy-matching to t.
;; helm-find-files: Enabled by default.
;; helm-locate: Enable by setting helm-locate-fuzzy-match to t.
;; helm-M-x: Enabled by setting helm-M-x-fuzzy-match to t.
;; helm-semantic: Enabled by setting helm-semantic-fuzzy-match to t.
;; helm-imenu: Enabled by setting helm-imenu-fuzzy-match to t.
;; helm-apropos: Enabled by setting helm-apropos-fuzzy-match to t.
;; helm-lisp-completion-at-point: Enabled by setting helm-lisp-fuzzy-completion to t.
;; ----------------------------------------------------------
(with-eval-after-load "helm"
                      (require 'helm-config)
                      (global-set-key (kbd "M-x") 'helm-M-x)
                      (global-set-key (kbd "C-x C-f") 'helm-find-files)
                      (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
                      (global-set-key (kbd "M-y") 'helm-show-kill-ring)
                      (global-set-key (kbd "C-c h o") 'helm-occur)
                      (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
                      (setq helm-quick-update                     t ; do not display invisible candidates
                            helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
                            helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
                            helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
                            helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
                            helm-ff-file-name-history-use-recentf t
                            helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
                            helm-M-x-fuzzy-match                  t ; optional fuzzy matching for helm-M-x
                            helm-recentf-fuzzy-match              t
                            helm-semantic-fuzzy-match             t
                            helm-imenu-fuzzy-match                t
                            helm-locate-fuzzy-match               t
                            helm-lisp-fuzzy-completion            t
                            )
                      (when (executable-find "curl")
                        (setq helm-google-suggest-use-curl-p t))

                      (when (executable-find "ack-grep")
                        (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
                              helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))
                      (helm-mode 1)
                      ;; (helm-autoresize-mode 1)
                      (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
                      (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
                      (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
                      (define-key global-map [remap find-file] 'helm-find-files)
                      (define-key global-map [remap occur] 'helm-occur)
                      (define-key global-map [remap list-buffers] 'helm-buffers-list)
                      (define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
                      (unless (boundp 'completion-in-region-function)
                        (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
                        (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))
                      ;; (add-hook 'kill-emacs-hook #'(lambda () (and (file-exists-p "$TMP") (delete-file "$TMP"))))
                      )
;; }}}


(autoload 'anything "anything" "load anything" t)
(with-eval-after-load "anything"
                      (require 'anything)
                      (require 'anything-config)
                      (setq anything-sources
                            (list anything-c-source-buffers
                                  anything-c-source-file-name-history
                                  anything-c-source-info-pages
                                  anything-c-source-man-pages
                                  anything-c-source-file-cache
                                  anything-c-source-emacs-commands
                                  anything-c-source-find-files
                                  anything-c-source-files-in-current-dir+
                                  anything-c-source-files-in-all-dired
                                  anything-c-source-man-pages
                                  anything-c-source-emacs-process))
                      (message "loaded anything configuration xin chào gấu yêu :\)")
                      )

(autoload 'smex "smex" "load smex" t)
(with-eval-after-load "smex"
                      (require 'smex) ; Not needed if you use package.el
                      (smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                                        ; when Smex is auto-initialized on its first run.
                      (global-set-key (kbd "M-x") 'smex)
                      )

(with-eval-after-load "org"
                      (require 'bind-key)
                      (require 'org)
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
                      (bind-key "C-c r" 'org-capture)
                      (bind-key "C-c a" 'org-agenda)
                      (bind-key "C-c l" 'org-store-link)
                      (bind-key "C-c L" 'org-insert-link-global)
                      (bind-key "C-c O" 'org-open-at-point-global)
                      (bind-key "<f9> <f9>" 'org-agenda-list)
                      (bind-key "<f9> <f8>" (lambda () (interactive) (org-capture nil "r")))
                      (bind-key "C-TAB" 'org-cycle org-mode-map)
                      (bind-key "C-c v" 'org-show-todo-tree org-mode-map)
                      (bind-key "C-c C-r" 'org-refile org-mode-map)
                      (bind-key "C-c R" 'org-reveal org-mode-map)
                      )

(with-eval-after-load "powerline"
                      (require 'powerline)
                      (powerline-default-theme)
                      (message "load powerline"))


(with-eval-after-load "el-get"
                      (setq el-get-sources
                            '((:name el-get :branch "master")                   ; self-hosting
                              (:name cups
                                     :type elpa)
                              (:name chess
                                     :description "Play chess in Emacs"
                                     :type elpa)
                              (:name buffer-move           ; have to add your own keys
                                     :after (progn
                                              (global-set-key (kbd "<C-S-up>")     'buf-move-up)
                                              (global-set-key (kbd "<C-S-down>")   'buf-move-down)
                                              (global-set-key (kbd "<C-S-left>")   'buf-move-left)
                                              (global-set-key (kbd "<C-S-right>")  'buf-move-right)))

                              (:name smex              ; a better (ido like) M-x
                                     :after (progn
                                              (setq smex-save-file "~/.emacs.d/.smex-items")
                                              (global-set-key (kbd "M-x") 'smex)
                                              (global-set-key (kbd "M-X") 'smex-major-mode-commands)))
                              (:name goto-last-change      ; move pointer back to last change
                                     :after (progn
                                              ;; when using AZERTY keyboard, consider C-x C-_
                                              (global-set-key (kbd "C-x C-/") 'goto-last-change)))
                              (:name anything
                                     :type elpa)
                              (:name yasnippet
                                     :type elpa)
                              (:name org2blog
                                     :type elpa)
                              (:name magit
                                     :type elpa
                                     :after (progn ()
                                                   (global-set-key (kbd "C-c gt") 'magit-status)))
                              (:name google-maps
                                     :type elpa)
                              (:name git-modes
                                     :description "GNU Emacs modes for various Git-related files"
                                     :type github
                                     :pkgname "magit/git-modes")
                              ;; Same as "package" except that it takes the version from Emacs 24
                              (:name package
                                     :description "ELPA implementation (\"package.el\") from Emacs 24"
                                     :builtin "24"
                                     :type http
                                     :url "http://repo.or.cz/w/emacs.git/blob_plain/1a0a666f941c99882093d7bd08ced15033bc3f0c:/lisp/emacs-lisp/package.el"
                                     :shallow nil
                                     :features package
                                     :post-init
                                     (progn
                                       ;; add package.rcp's old `package-user-dir' to
                                       ;; `package-directory-list', in case there are
                                       ;; packages installed there from before
                                       (let ((old-package-user-dir
                                              (expand-file-name
                                               (convert-standard-filename
                                                (concat (file-name-as-directory
                                                         default-directory)
                                                        "elpa")))))
                                         (when (file-directory-p old-package-user-dir)
                                           (add-to-list 'package-directory-list old-package-user-dir)))
                                       ;; Ensure `package-archives' is defined
                                       (setq package-archives (bound-and-true-p package-archives))
                                       ;; Ensure needed entries are in `package-archives' without
                                       ;; clobbering what what already there.
                                       (mapc
                                        (lambda (pa)
                                          (add-to-list 'package-archives pa 'append))
                                        '(("ELPA" . "http://tromey.com/elpa/")
                                          ("melpa" . "http://melpa.org/packages/")
                                          ("gnu" . "http://elpa.gnu.org/packages/")
                                          ("marmalade" . "http://marmalade-repo.org/packages/")
                                          ("SC"   . "http://joseito.republika.pl/sunrise-commander/")))))
                              ))
                      (message  "load elget config"))


(autoload 'company-complete "company" "Load company mode" t)
(with-eval-after-load "company"
                      (require 'company)
                      (global-company-mode 1)
                      (add-to-list 'company-backends 'company-cmake))

(with-eval-after-load "company-clang"
                      (message "company-clang")
                      (setq dbd:clang-root-dir (or (getenv "DBD_CLANG_ROOT") "D:/dbd/tools/LLVM"))
                      (add-to-list 'exec-path (expand-file-name "bin" dbd:clang-root-dir))
                      (setq company-clang-arguments
                            '("-std=c++11"
                              "-stdlib=libc++"
                              "-I/usr/include/c++/v1"
                              "-I/usr/include"
                              "-I/usr/include/x86_64-linux-gnu"
                              "-I/usr/include/clang/3.4/include/"))
                      )

(with-eval-after-load "auto-complete"
                      (message "load auto-complete")

                      )
