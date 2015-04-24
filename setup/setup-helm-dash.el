;;; setup-helm-dash.el ---
;; Author: dbd
;; Version: 0.0.1

;;; Commentary:
;;
;;

;;; Code:
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dbd/dash-path (docset)
  (if (string= docset "OpenGL_2")
      (concat (concat helm-dash-docsets-path "/") "OpenGL2.docset")
    (if (string= docset "OpenGL_3")
        (concat (concat helm-dash-docsets-path "/") "OpenGL3.docset")
      (if (string= docset "OpenGL_4")
          (concat (concat helm-dash-docsets-path "/") "OpenGL4.docset")
        (if (string= docset "Emacs_Lisp")
            (concat (concat helm-dash-docsets-path "/") "Emacs Lisp.docset")
          (concat
           (concat
            (concat
             (concat helm-dash-docsets-path "/")
             (nth 0 (split-string docset "_")))) ".docset"))))))

(defun dbd/dash-install (docset)
  (unless (file-exists-p (dbd/dash-path docset))
    (helm-dash-install-docset docset)))

(defun dbd/dash-hook ()
  (local-set-key "\C-h\C-df" 'helm-dash)
  (local-set-key "\C-h\C-dg" 'helm-dash-at-point)
  (local-set-key "\C-h\C-dh" 'helm-dash-reset-connections))

(defun dbd/dash-hook-java ()
  (interactive)
  (setq-local helm-dash-docsets '("Java_SE8")))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq helm-dash-docsets-path (format "%s/.emacs.d/docsets" (getenv "HOME")))

(dbd/dash-install "Android")
(dbd/dash-install "Bash")
(dbd/dash-install "C")
(dbd/dash-install "C++")
(dbd/dash-install "Emacs_Lisp")
(dbd/dash-install "Java_SE8")
(dbd/dash-install "JavaScript")
(dbd/dash-install "Markdown")
(dbd/dash-install "OpenGL_2")
(dbd/dash-install "OpenGL_3")
(dbd/dash-install "OpenGL_4")
(dbd/dash-install "Qt_5")
(dbd/dash-install "Ruby_2")

(setq helm-dash-common-docsets '("C" "C++" "Qt"))
(setq helm-dash-min-length 2)

(add-hook 'prog-mode-hook 'dbd/dash-hook)
(add-hook 'java-mode-hook 'dbd/dash-hook-java)
