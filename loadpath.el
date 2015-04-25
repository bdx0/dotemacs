;;; loadpath.el --
;; Author: dbd
;; Version: 0.0.2

;;; Commentary:
;;
;;

;;; Code:

(message "loading loadpath.el ... ")
(add-to-list 'custom-theme-load-path (concat dbd-emacs-dir "elisp/emacs-color-theme-solarized"))
(add-to-list 'custom-theme-load-path (concat dbd-emacs-dir "elisp/zenbur-theme"))
(setq temp-list load-path)
(setq load-path (list (concat dbd-emacs-dir "elisp/emacs-color-theme-solarized")
                      (concat dbd-emacs-dir "elisp/")
                      (concat dbd-emacs-dir "elisp/emacs-async")
                      (concat dbd-emacs-dir "elisp/helm")
                      (concat dbd-emacs-dir "elisp/helm-dash")
                      (concat dbd-emacs-dir "elisp/dash-dot-el")
                      (concat dbd-emacs-dir "elisp/magit")
                      (concat dbd-emacs-dir "elisp/git-modes")
                      (concat dbd-emacs-dir "elisp/org-mode/lisp")
                      (concat dbd-emacs-dir "elisp/org-mode/contrib")
                      (concat dbd-emacs-dir "elisp/org-mode/yasnippet")
                      (concat dbd-emacs-dir "elisp/company-mode")
                      (concat dbd-emacs-dir "elisp/cedet-git")
                      (concat dbd-emacs-dir "elisp/projectile")))
(setq load-path (append load-path temp-list))
(setq dbd-elisp-dir (concat dbd-emacs-dir "elisp/"))
(setq generated-autoload-file (concat dbd-elisp-dir "loaddefs.el"))
(update-directory-autoloads dbd-elisp-dir)
(load generated-autoload-file)
(message "end load loadpath.el")


;;; end elips.el
