;;; basic-func.el -- the basic function support for the loading modules in elisp (like bootstrap)

(defun try-require (feature)
  (condition-case nil
      (require feature)
    (error (progn
             (message "could not require %s" feature)
             nil))))

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

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
;; start cmake ide
;; start new process for cmake ide
;;======================================================
(defun dbd:start-cmake-ide ()
  (interactive)
  (let ((dbd:config-ide-file (expand-file-name "env_ide.cmd" (getenv "emacs_dir")))
        (dbd:emacs-cmake-bin (expand-file-name "emacs-cmake.cmd" (getenv "emacs_dir"))))
    (if (file-exists-p dbd:emacs-cmake-bin)
        (start-process "dbd:cmake-ide" nil
                       dbd:config-ide-file
                       dbd:emacs-cmake-bin)))
  (message "run dbd:cmake-ide")
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
  (let ((dbd:config-ide-file (expand-file-name "env_ide.cmd" (getenv "emacs_dir")))
        (dbd:eclipse-bin (expand-file-name "eclipse.exe" (or (getenv "ECLIPSE_ROOT") "C:/Google/android/eclipse-cpp"))))
    (if (file-exists-p dbd:eclipse-bin)
        (start-process "dbd:start-eclipse-ide" nil
                       dbd:config-ide-file
                       dbd:eclipse-bin
                       "-data" (getenv "eclipse_workspace")))
    (message "run dbd:start-eclipse-ide")
    ))

;;======================================================
;; dbd:start-mintty
;; start new process for mintty
;;======================================================
(defun dbd:start-mintty ()
  (interactive)
  (let ((dbd:config-ide-file (expand-file-name "env_ide.cmd" (getenv "emacs_dir")))
        (dbd:mintty-bin (executable-find "mintty.exe"))
        (dbd:current-directory (file-name-directory (or buffer-file-name load-file-name default-directory))))
    (if (file-exists-p dbd:mintty-bin)
        (start-process "dbd:start-eclipse-ide" nil
                       dbd:config-ide-file
                       dbd:mintty-bin
                       "-h" "error"
                       "/bin/zsh" "-l" "-c" (format "cd `cygpath -u %s`;exec zsh" dbd:current-directory)))
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
