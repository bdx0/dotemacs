;;; elisp.el --
(message "load elisp.el")
(add-to-list 'custom-theme-load-path (concat (file-name-directory (or load-file-name buffer-file-name)) "elisp/emacs-color-theme-solarized"))
(setq temp-list load-path)
(setq load-path (list (concat (file-name-directory (or load-file-name buffer-file-name)) "elisp/emacs-color-theme-solarized")
                      (concat (file-name-directory (or load-file-name buffer-file-name)) "elisp/")
                      (concat (file-name-directory (or load-file-name buffer-file-name)) "elisp/emacs-async")
                      (concat (file-name-directory (or load-file-name buffer-file-name)) "elisp/helm")
                      (concat (file-name-directory (or load-file-name buffer-file-name)) "elisp/magit")
                      (concat (file-name-directory (or load-file-name buffer-file-name)) "elisp/git-modes")
                      (concat (file-name-directory (or load-file-name buffer-file-name)) "elisp/org-mode/lisp")
                      (concat (file-name-directory (or load-file-name buffer-file-name)) "elisp/org-mode/contrib")
                      (concat (file-name-directory (or load-file-name buffer-file-name)) "elisp/org-mode/yasnippet")))
(setq load-path (append load-path temp-list))
(setq dbd-elisp-dir (concat (file-name-directory (or load-file-name buffer-file-name)) "elisp/"))
(setq generated-autoload-file (concat dbd-elisp-dir "loaddefs.el"))
(update-directory-autoloads dbd-elisp-dir)
(load generated-autoload-file)
(message "end load elisp.el")


;;; end elips.el
