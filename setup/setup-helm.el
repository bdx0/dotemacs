;;; setup-helm.el --- configuration for helm
;; Author: dbd
;; URL:
;; version: 0.0.1

;;; commentary:
;; this is a simple setup after load helm

;; {{{ helm
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

(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-c h y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c h Y") 'helm-all-mark-rings)

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
(require 'helm-dash nil t)
(require 'helm-projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-indexing-method 'alien)
(setq projectile-switch-project-action 'helm-projectile-find-file)
(setq projectile-enable-caching t)
(message "load setup helm")
;; (add-hook 'kill-emacs-hook #'(lambda () (and (file-exists-p "$TMP") (delete-file "$TMP"))))

;; }}}
