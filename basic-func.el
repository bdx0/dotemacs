;;; basic-func.el -- the basic function support for the loading modules in elisp (like bootstrap)

;;============================================
;; Byte-compile .emacs when saving it.
;;============================================
(defun byte-compile-user-init-file ()
  (let ((byte-compile-warnings '(unresolved)))
    ;; in case compilation fails, don't leave the old .elc around:
    (when (file-exists-p (concat user-init-file ".elc"))
      (delete-file (concat user-init-file ".elc")))
    (byte-compile-file user-init-file)
    (message "%s compiled" user-init-file)
    ))

;;============================================
;;
;;
;;============================================
(defun my-emacs-lisp-mode-hook ()
  (when (equal buffer-file-name user-init-file)
    (add-hook 'after-save-hook 'byte-compile-user-init-file t t)))

;;============================================
;;
;;
;;============================================
(defun try-require (feature)
  (condition-case nil
      (require feature)
    (error (progn
             (message "could not require %s" feature)
             nil))))
;;============================================
;;
;;
;;============================================
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;;============================================
;;
;;
;;============================================
(defmacro stante-after (feature &rest forms)
  (declare (indent 1) (debug t))
  `(,(if (or (not byte-compile-current-file)
             (if (symbolp feature)
                 (require feature nil :no-error)
               (load feature :no-message :no-error)))
         'progn
       (message "stante-after: cannot find %s" feature)
       'with-no-warnings)
    (with-eval-after-load ',feature ,@forms)))

;;===========================================
;; dbd-load-with-eval
;; This function
;;
;;===========================================
(defmacro dbd:load-with-eval (feature &rest forms)
  (declare (indent 1) (debug t))
  `(,(if (or (not byte-compile-current-file)
             (if (symbolp feature)
                 (require feature nil :no-error)
               (load feature :no-message :no-error)))
         'progn
       (message "stante-after: cannot find %s" feature)
       'with-no-warnings)
    (with-eval-after-load ',feature ,@forms)))


;;===========================================
;; define with-eval-after-load
;;
;;===========================================
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    `(eval-after-load ,file
       `(funcall (function ,(lambda () ,@body))))))

;;==================================
;; require*
;; http://curiousprogrammer.wordpress.com/2010/08/09/avoiding-the-tyranny-of-other-peoples-decisions/
;;==================================
(defun require* (feature &optional force)
  (when (or force (not (featurep feature)))
    (setq feature (symbol-name feature))
    (let ((path load-path)
          (found-filename nil)
          head el-attribs elc-attribs)
      (while (and (not found-filename) path)
        (setq head (pop path))
        (let ((el-filename (format "%s/%s.el" head feature))
              (elc-filename (format "%s/%s.elc" head feature)))
          ;; if .el and .elc both exist, pick the newest
          ;; otherwise pick the one that exists if any
          (cond ((and (file-exists-p el-filename)
                      (file-exists-p elc-filename))
                 (if (file-newer-than-file-p el-filename elc-filename)
                     (setq found-filename el-filename)
                   (setq found-filename elc-filename)))
                ((file-exists-p el-filename)
                 (setq found-filename el-filename))
                ((file-exists-p elc-filename)
                 (setq found-filename elc-filename)))
          ;; load file if found
          (when found-filename
            (message (format "Found: [%s]" found-filename))
            (let ((load-suffixes ()))
              (load found-filename)))))
      (unless found-filename (error "Unable to find %s" feature)))))

;;======================================================
;; dbd:start-emacs-load
;; start new process for emacs with configuration in current buffer
;;======================================================
(defun dbd:start-emacs-load ()
  (interactive)
  (let ((dbd:emacs-bin  "emacs"))
    (if 1
        (call-process dbd:emacs-bin
                      nil
                      0
                      0
                      "-q"
                      "-l"
                      (or load-file-name buffer-file-name))))
  (message "run dbd:start-emacs-load")
  )

;;======================================================
;; start org mode
;; start new process for org mode
;;======================================================
(defun dbd:start-org-editor ()
  (interactive)
  (let ((dbd:config-ide-file (expand-file-name "env_ide.cmd" (getenv "emacs_dir")))
        (dbd:emacs-org-bin (expand-file-name "emacs-org.cmd" (getenv "emacs_dir")))
        (dbd:org-dir (expand-file-name "toc.org" (getenv "org_dir"))))
    (if (file-exists-p dbd:emacs-org-bin)
        (start-process "dbd:org-editor" nil
                       dbd:config-ide-file
                       dbd:emacs-org-bin
                       dbd:org-dir)))
  (message "run dbd:org-editor")
  )

;;======================================================
;; dbd:start-eclipse
;; start new process for eclipse game dev tools
;;======================================================
(defun dbd:start-eclipse-ide ()
  (interactive)
  (let ((dbd:eclipse-bin (or (getenv "ECLIPSE_BIN") (expand-file-name "eclipse.exe" (getenv "ECLIPSE_ROOT")))))
    (if (file-exists-p dbd:eclipse-bin)
        (progn
          (call-process dbd:eclipse-bin
                        nil
                        0
                        0
                        "-data" (getenv "eclipse_workspace"))
          (message "run dbd:start-eclipse-ide")))
    ))

;;======================================================
;; dbd:start-mintty
;; start new process for mintty
;;======================================================
(defun dbd:start-mintty ()
  (interactive)
  (let ((dbd:mintty-bin (executable-find "mintty.exe"))
        (dbd:current-directory (file-name-directory (or buffer-file-name load-file-name default-directory))))
    (if (file-exists-p dbd:mintty-bin)
        (call-process dbd:mintty-bin
                      nil
                      0
                      0
                      "-h" "error"
                      "/bin/bash" "-l" "-c" (format "cd `cygpath -u %s`;exec bash" dbd:current-directory)))
    (message "run dbd:start-mintty-ide")
    )
  )


;;=====================================================
;; dbd:auto-complete
;; complete any thing at point
;;=====================================================
(defun dbd:config-ide ()
  (interactive)
  (let ((dbd:config-ide-file (expand-file-name "env_ide.cmd" (getenv "emacs_dir"))))
    (find-file (getenv "emacs_dir")))
  )

;;=====================================================
;; split-window-func-with-other-buffer
;;
;;=====================================================
(defun split-window-func-with-other-buffer (split-function)
  (lexical-let ((s-f split-function))
    (lambda ()
      (interactive)
      (funcall s-f)
      (set-window-buffer (next-window) (other-buffer)))))

;;=====================================================
;; split-window-horizontally-instead
;;
;;=====================================================
(defun split-window-horizontally-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-horizontally))))

;;=====================================================
;; split-window-vertically-instead
;;
;;=====================================================
(defun split-window-vertically-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-vertically))))


;;=====================================================
;; dbd:auto-complete
;; complete any thing at point
;;=====================================================
;;;###autoload
(defun dbd:auto-complete ()
  (interactive)
  ;; (if (derived-mode-p
  ;; (company-complete)
  (company-complete)
  ;; (auto-complete)
  ;; (ac-complete)
  )
;; {{{ cvs tools
;;=====================================================
;;
;; magit fix
;;=====================================================
(defun magit-expand-git-file-name--msys (args)
  "Handle Msys directory names such as /c/* by changing them to C:/*"
  (let ((filename (car args)))
    (when (string-match "^/\\([a-z]\\)/\\(.*\\)" filename)
      (setq filename (concat (match-string 1 filename) ":/"
                             (match-string 2 filename))))
    (list filename)))
;; (advice-add 'magit-expand-git-file-name :filter-args #'magit-expand-git-file-name--msys)

;;=====================================================
;;
;; for using emacs as merge tools
;;====================================================
(defvar jcl-save-and-kill-buffers-before-merge nil
  "Normally if emacs already visits any of the concerned files (local,
remote, base or merged) ediff will ask it shall save and kill the
buffer.  If you always want to answer yes to this then set this
to non-nil.")

(defun jcl-git-merge (local remote ancestor merged)
  (when jcl-save-and-kill-buffers-before-merge
    (dolist (file (list local remote ancestor merged))
      (setq file (file-truename file))
      (let ((old-buffer (and file (find-buffer-visiting file))))
        (when old-buffer
          (with-current-buffer old-buffer
            (save-buffer))
          (kill-buffer old-buffer)))))
  (prog1
      (if (string-equal ancestor merged)
          (progn
            (ediff-files local remote (list 'jcl-exit-recursive-edit-at-quit))
            (format "ediff compared %s and %s" local remote))
        (if ancestor
            (ediff-merge-files-with-ancestor local remote ancestor
                                             (list 'jcl-exit-recursive-edit-at-quit)
                                             merged)
          (ediff-merge-files local remote (list 'jcl-exit-recursive-edit-at-quit merged)))
        (format "ediff merged %s" merged))
    (recursive-edit)))

(defun jcl-exit-recursive-edit-at-quit ()
  (add-hook 'ediff-quit-hook (lambda () (throw 'exit nil)) t t))

;; }}}
