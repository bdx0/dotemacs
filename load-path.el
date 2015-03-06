;; {{{ add load path
(add-to-list 'load-path (getenv "emacsd_dir"))

;; autoload, eval-after-load, add-hook, try-require ???
;; load init.el flow
;; 0. require load elisp lib basic-func.el & autoload.el & env.el

(load-file (concat (file-name-directory (or load-file-name buffer-file-name)) "basic-func.el"))
(load-file (concat (file-name-directory (or load-file-name buffer-file-name)) "elisp.el"))
(load-file (concat (file-name-directory (or load-file-name buffer-file-name)) "dbd-autoload.el"))
(load-file (concat (file-name-directory (or load-file-name buffer-file-name)) "modes.el"))
(load-file (concat (file-name-directory (or load-file-name buffer-file-name)) "env.el"))
;; }}}
