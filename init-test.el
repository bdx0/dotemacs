;; init.el

;; Set the debug option when there is trouble...
;;(setq debug-on-error t)
;; http://iridia.ulb.ac.be/~manuel/dotemacs.html
(defvar *emacs-load-start* (current-time))
;;(add-to-list 'load-path (concat (getenv emacs_dir) "dotfiles/dbd/elisp/")))
;; {{{
(defvar org-inhibit-highlight-removal nil) ; dynamically scoped param
(defvar org-table-formula-constants-local nil
  "Local version of `org-table-formula-constants'.")
(make-variable-buffer-local 'org-table-formula-constants-local)
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
(load-file (concat (file-name-directory (or load-file-name buffer-file-name)) "dbd-autoload.el"))
(load-file (concat (file-name-directory (or load-file-name buffer-file-name)) "env.el"))

;; 1. basic.el
(load-file (concat (file-name-directory (or load-file-name buffer-file-name)) "basic.el"))
;; 2. libs.el
(load-file (concat (file-name-directory (or load-file-name buffer-file-name)) "libs.el"))
;; 3. config for elget
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))
;; 4. modes.el
(load-file (concat (file-name-directory (or load-file-name buffer-file-name)) "modes.el"))
;; 5. keys.el
(load-file (concat (file-name-directory (or load-file-name buffer-file-name)) "keys.el"))
;; 6. style.el
(load-file (concat (file-name-directory (or load-file-name buffer-file-name)) "style.el"))
;; 7. 
;; }}}


(message "My .emacs loaded in %d s" 
         (destructuring-bind (hi lo ms) (current-time)
           (- (+ hi lo) (+ (first *emacs-load-start*)
                           (second *emacs-load-start*)))))
