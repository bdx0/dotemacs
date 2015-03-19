;;; dbd-org.el --- dbd org mode pre-configuration
;;
;;; code:

;; Set the debug option when there is trouble...
;;(setq debug-on-error t)
;; http://iridia.ulb.ac.be/~manuel/dotemacs.html
(setq debug-on-error t)
(defvar *emacs-load-start* (current-time))
(defconst dbd-file-init-el (or load-file-name buffer-file-name))
(defun dbd-reload()
  (interactive)
  (load-file dbd-file-init-el))
;; {{{
;;;; Require other packages
(eval-when-compile
  (require 'cl)
  (require 'gnus-sum))

;; }}}
;; {{{
;; autoload, eval-after-load, add-hook, try-require ???
;; load init.el flow
;; 0. require load elisp lib basic-func.el & autoload.el & env.el
(load-file (concat (file-name-directory (or load-file-name buffer-file-name)) "basic-func.el"))
(load-file (concat (file-name-directory (or load-file-name buffer-file-name)) "elisp.el"))
(load-file (concat (file-name-directory (or load-file-name buffer-file-name)) "dbd-autoload.el"))
(load-file (concat (file-name-directory (or load-file-name buffer-file-name)) "modes.el"))
(load-file (concat (file-name-directory (or load-file-name buffer-file-name)) "env.el"))

;; 1. basic.el
(load-file (concat (file-name-directory (or load-file-name buffer-file-name)) "basic.el"))
;; 2. libs.el
(load-file (concat (file-name-directory (or load-file-name buffer-file-name)) "libs.el"))
;; 3. config for elget
;; (load-file (concat (file-name-directory (or load-file-name buffer-file-name)) "dbd-elget.el"))
(load-file (concat (file-name-directory (or load-file-name buffer-file-name)) "dbd-package.el"))

;; 5. keys.el
(load-file (concat (file-name-directory (or load-file-name buffer-file-name)) "keys.el"))
;; 6. style.el
(load-file (concat (file-name-directory (or load-file-name buffer-file-name)) "style.el"))
;; 7. Setup for orgmode
;; =============
;; elget config example
;; http://jonebird.com/2013/06/25/managing-my-emacs-addons-with-el-get/
;; http://www.emacswiki.org/emacs/el-get
;; http://wikemacs.org/wiki/El-get
;; ============

;; (el-get 'sync '(dired+ color-theme  yasnippet autopair switch-window rainbow-mode "org" "org2blog"))
;; my packages
(setq dbd-packages
      ;; list of packages we use straight from official recipes
      '(google-maps org org2blog org-toc org-ac org-pomodoro org-protocol-jekyll
                    orglue org-octopress org-grep org-present org-caldav
                    orgtbl-ascii-plot orgtbl-show-header orglink org-readme
                    org-vcard org-wc xml-rpc))


(dbd:packages-install dbd-packages)
(powerline-default-theme)
;; }}}

(setq frame-title-format "org-mode")

;;( message "My .emacs loaded in %d s"
;;         (destructuring-bind (hi lo ms) (current-time)
;;                             (- (+ hi lo) (+ (first *emacs-load-start*)
;;                                             (second *emacs-load-start*)))))


;; http://orgmode.org/worg/org-configs/org-customization-guide.html

