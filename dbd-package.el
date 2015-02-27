;;; dbd-package.el ---

;;;; package.el
(require 'package)
(setq package-user-dir "~/.emacs.d/elpa/")
(package-initialize)

;;;###autoload
(defun dbd:packages-install (dbd-list-packages)
  "Install only the sweetest of packages."
  (package-refresh-contents)
  (mapc '(lambda (package)
           (unless (package-installed-p package)
             (package-install package)))
        dbd-list-packages))
;; (dbd:packages-install '(company))
