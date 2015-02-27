;;; dbd-cmake-mode.el -- describe cmake mode in emacs

(provide 'dbd-cmake-mode)

(require 'cmake-mode nil 'noerror)
(require 'cmake-ide nil 'noerror)
(require 'cpputils-cmake nil 'noerror)

(defvar dbd-cmake-mode-hook nil)

(defun cmake-rename-buffer ()
  "Renames a CMakeLists.txt buffer to cmake-<directory name>."
  (interactive)
                                        ;(print (concat "buffer-filename = " (buffer-file-name)))
                                        ;(print (concat "buffer-name     = " (buffer-name)))
  (when (and (buffer-file-name) (string-match "CMakeLists.txt" (buffer-name)))
                                        ;(setq file-name (file-name-nondirectory (buffer-file-name)))
    (setq parent-dir (file-name-nondirectory (directory-file-name (file-name-directory (buffer-file-name)))))
                                        ;(print (concat "parent-dir = " parent-dir))
    (setq new-buffer-name (concat "cmake-" parent-dir))
                                        ;(print (concat "new-buffer-name= " new-buffer-name))
    (rename-buffer new-buffer-name t)
    )
  )

(progn
  ;; (require 'cpputils-cmake)
  ;; (cppcm-reload-all)
  (global-set-key (kbd "C-c C-g")
                  '(lambda ()(interactive) (gud-gdb (concat "gdb --fullname " (cppcm-get-exe-path-current-buffer)))))
  ;; OPTIONAL, some users need specify extra flags forwarded to compiler
  (setq cppcm-extra-preprocss-flags-from-user '("-I/usr/src/linux/include" "-DNDEBUG"))
  ;; (cmake-ide-setup)
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)
                                        ;(setq uniquify-buffer-name-style 'post-forward)
  )

;; ;;;###autoload
;; (define-derived-mode dbd-cmake-mode text-mode "dbd-make-mode"
;;   )
;;;###autoload
(defun dbd-cmake-mode ()
  "Major mode for editing CMake listfiles."
  (interactive)
  (cmake-mode)
  (cmake-rename-buffer)
  )

;; (add-hook 'cmake-mode-hook 'dbd-cmake-mode)
