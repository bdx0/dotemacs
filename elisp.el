;;; elisp.el --

;;;###autoload
(progn
  (message "add to load path")
  (add-to-list 'custom-theme-load-path (concat (file-name-directory (or load-file-name buffer-file-name)) "elisp/emacs-color-theme-solarized"))
  (add-to-list 'load-path (concat (file-name-directory (or load-file-name buffer-file-name)) "elisp/color-theme-solarized"))
  (add-to-list 'load-path (concat (file-name-directory (or load-file-name buffer-file-name)) "elisp/"))
  (add-to-list 'load-path (concat (file-name-directory (or load-file-name buffer-file-name)) "elisp/helm"))
  (add-to-list 'load-path (concat (file-name-directory (or load-file-name buffer-file-name)) "elisp/magit"))
  (add-to-list 'load-path (concat (file-name-directory (or load-file-name buffer-file-name)) "elisp/git-modes"))
  (add-to-list 'load-path (concat (file-name-directory (or load-file-name buffer-file-name)) "elisp/org-mode"))
  (setq dbd-local-mode-dir (concat (file-name-directory (or load-file-name buffer-file-name)) "elisp/"))
  (setq generated-autoload-file (concat dbd-local-mode-dir "loaddefs.el"))
  (update-directory-autoloads dbd-local-mode-dir)
  (load generated-autoload-file)
  )


;;; end elips.el
