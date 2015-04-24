;;; autoload.el --- pre-define some function when load feature
;; Author: dbd
;; Version: 0.0.1

;;; Commentary:
;;

;;; Code:

(with-eval-after-load "recentf"
  (recentf-mode 1)
  (setq recentf-exclude '("~$"))
  (setq recentf-max-saved-items 1000)
  ;; get rid of `find-file-read-only' and replace it with something
  ;; more useful.
  (global-set-key (kbd "C-S-t") 'ido-recentf-open)
  (global-set-key (kbd "C-c r") 'query-recentf)
  )

(with-eval-after-load "ido"
  (ido-mode)
  (setq ido-enable-flex-matching t)
  )

(with-eval-after-load "tabbar"
  (tabbar-mode 1)
  ;; set shortcut for tabbar mode
  (global-set-key [M-S-left] 'tabbar-backward)
  (global-set-key [M-S-right] 'tabbar-forward)
  (global-set-key (kbd "<C-tab>") 'bury-buffer)
  (global-set-key [M-S-up] 'next-buffer)
  (global-set-key [M-S-down] 'previous-buffer))

(with-eval-after-load "helm"
  (load-file (concat (file-name-directory dbd-init-el) "setup/setup-helm.el"))
  )

(with-eval-after-load "org"
  (load-file (concat (file-name-directory dbd-init-el) "setup/setup-org.el")) 
  )


;;;###autoload
(with-eval-after-load "anything"
  (require 'anything-config)
  (setq anything-sources
        (list anything-c-source-buffers
              anything-c-source-file-name-history
              anything-c-source-info-pages
              anything-c-source-man-pages
              anything-c-source-file-cache
              anything-c-source-emacs-commands
              anything-c-source-find-files
              anything-c-source-files-in-current-dir+
              anything-c-source-files-in-all-dired
              anything-c-source-man-pages
              anything-c-source-emacs-process))
  (message "loaded anything configuration xin chào gấu yêu :\)")
  )

;;;###autoload
(with-eval-after-load "smex"
  (smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                                        ; when Smex is auto-initialized on its first run.
  (global-set-key (kbd "M-x") 'smex)
  )

(with-eval-after-load "powerline"
  (powerline-default-theme)
  (message "load powerline"))


(with-eval-after-load "el-get"
  (setq el-get-sources
        '((:name el-get :branch "master")                   ; self-hosting
          (:name cups
                 :type elpa)
          (:name chess
                 :description "Play chess in Emacs"
                 :type elpa)
          (:name buffer-move           ; have to add your own keys
                 :after (progn
                          (global-set-key (kbd "<C-S-up>")     'buf-move-up)
                          (global-set-key (kbd "<C-S-down>")   'buf-move-down)
                          (global-set-key (kbd "<C-S-left>")   'buf-move-left)
                          (global-set-key (kbd "<C-S-right>")  'buf-move-right)))

          (:name smex              ; a better (ido like) M-x
                 :after (progn
                          (setq smex-save-file "~/.emacs.d/.smex-items")
                          (global-set-key (kbd "M-x") 'smex)
                          (global-set-key (kbd "M-X") 'smex-major-mode-commands)))
          (:name goto-last-change      ; move pointer back to last change
                 :after (progn
                          ;; when using AZERTY keyboard, consider C-x C-_
                          (global-set-key (kbd "C-x C-/") 'goto-last-change)))
          (:name anything
                 :type elpa)
          (:name yasnippet
                 :type elpa)
          (:name org2blog
                 :type elpa)
          (:name magit
                 :type elpa
                 :after (progn ()
                               (global-set-key (kbd "C-c gt") 'magit-status)))
          (:name google-maps
                 :type elpa)
          (:name git-modes
                 :description "GNU Emacs modes for various Git-related files"
                 :type github
                 :pkgname "magit/git-modes")
          ;; Same as "package" except that it takes the version from Emacs 24
          (:name package
                 :description "ELPA implementation (\"package.el\") from Emacs 24"
                 :builtin "24"
                 :type http
                 :url "http://repo.or.cz/w/emacs.git/blob_plain/1a0a666f941c99882093d7bd08ced15033bc3f0c:/lisp/emacs-lisp/package.el"
                 :shallow nil
                 :features package
                 :post-init
                 (progn
                   ;; add package.rcp's old `package-user-dir' to
                   ;; `package-directory-list', in case there are
                   ;; packages installed there from before
                   (let ((old-package-user-dir
                          (expand-file-name
                           (convert-standard-filename
                            (concat (file-name-as-directory
                                     default-directory)
                                    "elpa")))))
                     (when (file-directory-p old-package-user-dir)
                       (add-to-list 'package-directory-list old-package-user-dir)))
                   ;; Ensure `package-archives' is defined
                   (setq package-archives (bound-and-true-p package-archives))
                   ;; Ensure needed entries are in `package-archives' without
                   ;; clobbering what what already there.
                   (mapc
                    (lambda (pa)
                      (add-to-list 'package-archives pa 'append))
                    '(("ELPA" . "http://tromey.com/elpa/")
                      ("melpa" . "http://melpa.org/packages/")
                      ("gnu" . "http://elpa.gnu.org/packages/")
                      ("marmalade" . "http://marmalade-repo.org/packages/")
                      ("SC"   . "http://joseito.republika.pl/sunrise-commander/")))))
          ))
  (message  "load elget config"))


;;;###autoload
(with-eval-after-load "company"
  (global-company-mode 1)
  (add-to-list 'company-backends 'company-cmake))

(with-eval-after-load "company-clang"
  (message "company-clang")
  (setq dbd:clang-root-dir (or (getenv "DBD_CLANG_ROOT") "D:/dbd/tools/LLVM"))
  (add-to-list 'exec-path (expand-file-name "bin" dbd:clang-root-dir))
  (setq company-clang-arguments
        '("-std=c++11"
          "-stdlib=libc++"
          "-I/usr/include/c++/v1"
          "-I/usr/include"
          "-I/usr/include/x86_64-linux-gnu"
          "-I/usr/include/clang/3.4/include/"))
  )
