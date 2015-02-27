;;; dbd-elget.el --- load elget config
;; =============
;; elget config example
;; http://jonebird.com/2013/06/25/managing-my-emacs-addons-with-el-get/
;; http://www.emacswiki.org/emacs/el-get
;; http://wikemacs.org/wiki/El-get
;; ============
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))
(require 'el-get-elpa)
(unless (file-directory-p el-get-recipe-path-elpa)
  (el-get-elpa-build-local-recipes))

;;;###autoload
(defun dbd:packages-install (dbd-list-packages)
  "Install only the sweetest of packages."
  (el-get 'sync dbd-list-packages))
