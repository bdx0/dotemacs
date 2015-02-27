;; init.el

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
;; 7. Setup for cmake

(dbd:packages-install '(dired+
                        yasnippet
                        autopair
                        rainbow-mode
                        cmake-ide
                        cmake-mode
                        cmake-project
                        cpp
                        cpputils-cmake
                        auto-complete
                        auto-complete-clang
                        auto-complete-c-headers
                        flycheck
                        company
                        company-cmake
                        company-c-headers
                        company-irony
                        ag
                        function-args
                        c-eldoc
                        projectile
                        rtags))
(powerline-default-theme)
(defun maybe-cmake-project-hook ()
  (progn
    (message "Load cmake project hook")
    (if (file-exists-p "CMakeLists.txt") (cmake-project-mode)))
  )

(add-hook 'c-mode-common-hook 'maybe-cmake-project-hook)
(add-hook 'cmake-mode-hook 'maybe-cmake-project-hook)

(setq company-cmake-executable (getenv "CMAKE_BIN"))
(setq cmake-ide-compile-command (getenv "CMAKE_BIN"))
(setq cmake-ide-dir (getenv "CMAKE_DIR"))
;; }}}
(require 'auto-complete)
(global-auto-complete-mode +1)
(setq frame-title-format "cmake tools")

;;( message "My .emacs loaded in %d s"
;;         (destructuring-bind (hi lo ms) (current-time)
;;                             (- (+ hi lo) (+ (first *emacs-load-start*)
;;                                             (second *emacs-load-start*)))))

;; http://www.logilab.org/blogentry/173886
;; http://blog.refu.co/?p=1311x
;; cedet & ecb & auto-complete & company-clang
;; http://truongtx.me/categories.html#emacs-ref
;; https://sites.google.com/site/taubkfet/tutorials/settingupac11developmentenvironmentonlinuxwithclangandemacs
;; http://tuhdo.github.io/c-ide.html
;; cmake-mode & cmake-project & cmake-ide & cpputils-cmake
;; Malink, an Emacs C/C++ project configuration packagec
;; http://blog.refu.co/?p=1311
