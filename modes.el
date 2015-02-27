;;-*- coding: utf-8 -*-
;;; modes.el -- map major mode for file extentsion

;; for html, web, javascript, server
;; setup files ending in “.js” to open in js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . dbd-js-mode)) ;http://ergoemacs.org/emacs/emacs_auto-activate_a_major-mode.html

;; for editor: markdown, orgmode, notes, docs, present, spreadsheet, plantuml
(add-to-list 'auto-mode-alist '("\\.md\\'" . dbd-markdown-mode)) ;http://ergoemacs.org/emacs/emacs_auto-activate_a_major-mode.html
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . dbd-markdown-mode)) ;http://ergoemacs.org/emacs/emacs_auto-activate_a_major-mode.html
(add-to-list 'auto-mode-alist '("\\.org\\'" . dbd-org-mode)) ;http://ergoemacs.org/emacs/emacs_auto-activate_a_major-mode.html
(add-to-list 'auto-mode-alist '("\\.org.txt\\'" . dbd-org-mode)) ;http://ergoemacs.org/emacs/emacs_auto-activate_a_major-mode.html
(add-to-list 'auto-mode-alist '("\\.notes\\'" . dbd-org-notes-mode)) ;http://ergoemacs.org/emacs/emacs_auto-activate_a_major-mode.html
(add-to-list 'auto-mode-alist '("\\.puml\\'" . dbd-planuml-mode)) ;http://ergoemacs.org/emacs/emacs_auto-activate_a_major-mode.html
(add-to-list 'auto-mode-alist '("\\.graphviz\\'" . dbd-graphviz-mode)) ;http://ergoemacs.org/emacs/emacs_auto-activate_a_major-mode.html
(add-to-list 'auto-mode-alist '("\\.xml\\'" . dbd-xml-mode)) ;http://ergoemacs.org/emacs/emacs_auto-activate_a_major-mode.html
(add-to-list 'auto-mode-alist '("\\.\\'" . dbd-markdown-mode)) ;http://ergoemacs.org/emacs/emacs_auto-activate_a_major-mode.html

;; for c\cc programming
(add-to-list 'auto-mode-alist '("\\.cc\\'" . dbd-cc-mode)) ;http://ergoemacs.org/emacs/emacs_auto-activate_a_major-mode.html
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . dbd-cc-mode)) ;http://ergoemacs.org/emacs/emacs_auto-activate_a_major-mode.html
(add-to-list 'auto-mode-alist '("\\.c\\'" . dbd-cc-mode)) ;http://ergoemacs.org/emacs/emacs_auto-activate_a_major-mode.html
(add-to-list 'auto-mode-alist '("\\.h\\'" . dbd-cc-mode)) ;http://ergoemacs.org/emacs/emacs_auto-activate_a_major-mode.html
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . dbd-cc-mode)) ;http://ergoemacs.org/emacs/emacs_auto-activate_a_major-mode.html

;; for make file
(add-to-list 'auto-mode-alist '("\\.mk\\'" . dbd-make-mode)) ;http://ergoemacs.org/emacs/emacs_auto-activate_a_major-mode.html
(add-to-list 'auto-mode-alist '("\\.mak\\'" . dbd-make-mode)) ;http://ergoemacs.org/emacs/emacs_auto-activate_a_major-mode.html
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . dbd-cmake-mode)) ;http://ergoemacs.org/emacs/emacs_auto-activate_a_major-mode.html
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . dbd-cmake-mode))

;; for python programming
(add-to-list 'auto-mode-alist '("\\.py\\'" . dbd-python-mode)) ;http://ergoemacs.org/emacs/emacs_auto-activate_a_major-mode.html
(add-to-list 'auto-mode-alist '("\\.gyp\\'" . dbd-python-mode)) ;http://ergoemacs.org/emacs/emacs_auto-activate_a_major-mode.html

;; for ruby programming
(add-to-list 'auto-mode-alist '("\\.rb\\'" . dbd-ruby-mode)) ;http://ergoemacs.org/emacs/emacs_auto-activate_a_major-mode.html

;; for batch script programming
(add-to-list 'auto-mode-alist '("\\.cmd\\'" . dbd-batch-mode)) ;http://ergoemacs.org/emacs/emacs_auto-activate_a_major-mode.html
(add-to-list 'auto-mode-alist '("\\.bat\\'" . dbd-batch-mode)) ;http://ergoemacs.org/emacs/emacs_auto-activate_a_major-mode.html

;; for shell script programming
(add-to-list 'auto-mode-alist '("\\.sh\\'" . dbd-shell-mode)) ;http://ergoemacs.org/emacs/emacs_auto-activate_a_major-mode.html
(add-to-list 'auto-mode-alist '("\\.bash\\'" . dbd-shell-mode)) ;http://ergoemacs.org/emacs/emacs_auto-activate_a_major-mode.html
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . dbd-shell-mode)) ;http://ergoemacs.org/emacs/emacs_auto-activate_a_major-mode.html

;; for webrtc-log
(add-to-list 'auto-mode-alist '("\\.log\\'" . dbd-log-mode))

;;============================================================
;; autoload example
;; http://stackoverflow.com/questions/4189159/emacs23-elisp-how-to-properly-autoload-this-library/4189794#4189794
;;============================================================
;;;###autoload
(progn
  (message "add to load path")
  (add-to-list 'load-path (concat (file-name-directory (or load-file-name buffer-file-name)) "modes/"))
  (setq dbd-local-mode-dir (concat (file-name-directory (or load-file-name buffer-file-name)) "modes/"))
  (setq generated-autoload-file (concat dbd-local-mode-dir "loaddefs.el"))
  (delete-file generated-autoload-file)
  (update-directory-autoloads dbd-local-mode-dir)
  (load generated-autoload-file)
  )

;;
;;
;;
;;
;; end modes.el
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Modes.html#Modes
