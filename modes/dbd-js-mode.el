
;;=======================================
;; some java script function from
;; http://bytes.inso.cc/2011/08/13/auto-installing-packages-in-emacs-with-elpa-and-el-get/
;;=======================================

(provide 'dbd-js-mode)

;; extra recipes for packages unknown to el-get (yet)
(setq el-get-sources
      '((:name css-mode :type elpa)
        (:name js2-mode-mooz
               :type git
               :url "git://github.com/mooz/js2-mode.git"
               :load "js2-mode.el"
               :compile ("js2-mode.el")
               :features js2-mode)))

; list all packages you want installed
(setq my-el-get-packages
      (append
       '(css-mode egg gist js2-mode-mooz)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-el-get-packages)

(require 'js2-mode)

;;;###autoload
(define-derived-mode dbd-js-mode js2-mode "dbd-js-mode"
  )

