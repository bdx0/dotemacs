;;; dbd-cmake-mode.el -- describe cmake mode in emacs

(provide 'dbd-cc-mode)

(require 'cc-mode)

(defun dbd-cc-mode-config ()
  (message "run cc mode config")
  (setq ac-clang-flags
        (mapcar (lambda (item)
                  (progn
                    (concat "-I"
                            (replace-regexp-in-string "\n$" ""
                                                      (shell-command-to-string (format "cygpath -m %s" item))
                                                      ))
                    ))
                (split-string (getenv "DBD_INCLUDE_PATH"))
                )
        )
  (setq c-eldoc-includes ac-clang-flags)
  (setq achead:include-directories (mapcar (lambda (item)
                                             (progn
                                               (replace-regexp-in-string "\n$" ""
                                                                         (shell-command-to-string (format "cygpath -m %s" item))
                                                                         )
                                               ))
                                           (split-string (getenv "DBD_INCLUDE_PATH"))))
  (setq ac-sources '(ac-source-c-headers ac-source-clang ac-source-yasnippet ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (auto-complete-mode +1)
  )

;; (defun dbd:get-include-dirs ()
;;   (let ((dbd:current-directory
;;          (file-name-directory (or buffer-file-name load-file-name default-directory))))
;;     (mapcar (lambda (item)(concat "-I" item))
;;             (split-string (with-temp-buffer
;;                             (insert-file-contents (expand-file-name ".include" dbd:current-directory))
;;                             (buffer-string)))))
;;   )

(defun dbd:make-clang-include ()
  (interactive)
  (setenv "DBD_INCLUDE_PATH" "")
  (let ((compiler-verbose (shell-command-to-string
                           "make include"))
        (pattern "\\(\<...\> search starts here:\n\\(\\(.*\n\\)*\\)End of search list\\)")
        )
    (if (string-match pattern compiler-verbose)
        (setenv "DBD_INCLUDE_PATH" (match-string 2 compiler-verbose))
      ))
  (dbd-cc-mode-config)
  )

(defun dbd:get-clang-include-dirs ()
  (setenv "DBD_INCLUDE_PATH" "")
  (let ((compiler-verbose (shell-command-to-string
                           "echo \"int main() {return 0;}\" | g++ -v -xc++ -std=c++11 -E -"))
        (pattern "\\(\<...\> search starts here:\n\\(\\(.*\n\\)*\\)End of search list\\)")
        )
    (if (string-match pattern compiler-verbose)
        (setenv "DBD_INCLUDE_PATH" (match-string 2 compiler-verbose))
      ))
  )

(progn
  (global-set-key (kbd "M-RET") 'auto-complete)
  (global-set-key (kbd "C-x C-o") 'ff-find-other-file)
  (require 'autopair)
  (autopair-global-mode +1)
  (setq autopair-autowrap t)
  (require 'auto-complete)
  (require 'yasnippet)
  (require 'auto-complete-config)
  (require 'auto-complete-clang)
  (require 'auto-complete-c-headers)
  (require 'c-eldoc)
  (require 'function-args)
  (global-ede-mode +1)
  (setq ac-auto-start nil)
  (setq ac-quick-help-delay 0.5)
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  ;; (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  ;; ;; (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  ;; (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  ;; (add-hook 'css-mode-hook 'ac-css-mode-setup)
  ;; (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (fa-config-default)
  ;; (define-key c-mode-map  [(contrl tab)] 'moo-complete)
  ;; (define-key c++-mode-map  [(control tab)] 'moo-complete)
  (define-key c-mode-map (kbd "M-o")  'fa-show)
  (define-key c++-mode-map (kbd "M-o")  'fa-show)
  (dbd:get-clang-include-dirs)
  (add-to-list 'ac-modes 'dbd-cc-mode)
  )

;;;###autoload
(define-derived-mode dbd-cc-mode c++-mode "dbd-cc-mode"
  (dbd-cc-mode-config)
  )
