;;; env.el -- set some environment variable for system
;; depend on operation system (OS)

;; Miscellaneous settings
;; ----------------------
;;
(setq-default user-full-name "Duong Bao Duy"
              user-mail-address "baoduy.duong0206@gmail.com"
              user-mobile "01656405505"
              user-home-dir (getenv "HOME")
              user-config ""                                  ; User configuration
              inhibit-startup-screen t                        ; Skip the startup screens
              initial-scratch-message nil
              confirm-kill-emacs 'y-or-n-p                    ; Disallow accidental exits
              blink-cursor-alist '((t . hollow))              ; Cursor blinks solid and hollow
              frame-title-format '(buffer-file-name "%f" "%b") ; I know it's Emacs; show something useful
              mode-line-format '(" %+ "                       ; Simplify the mode line
                                 (:propertize "%b" face mode-line-buffer-id)
                                 ":%l:%c %[" mode-name "%]"
                                 (-2 "%n")
                                 (visual-line-mode " W")
                                 (auto-fill-function " F")
                                 (overwrite-mode " O"))
              max-mini-window-height 1                        ; Bouncy minibuffer/echo gets obnoxious
              truncate-lines t                                ; Truncate lines, don't wrap
              default-truncate-lines t
              vc-make-backup-files t                          ; Keep numbered backups in ~/.saves
              version-control t
              delete-old-versions t
              kept-new-versions 4
              kept-old-versions 0
              backup-directory-alist '((".*" . "~/.saves/"))
              auto-save-list-file-prefix nil
              auto-save-file-name-transforms '((".*" "~/.saves/" t))
              font-lock-use-fonts '(or (mono) (grayscale))    ; Maximal syntax hilighting
              font-lock-use-colors '(color)
              font-lock-maximum-decoration t
              font-lock-maximum-size nil
              font-lock-auto-fontify t
              show-paren-style 'expression                    ; Highlight parenthesis
              comment-empty-lines t                           ; Prefix empty lines too
              show-trailing-whitespace t                      ; Show trailing whitespace
              use-dialog-box nil                              ; Always use the minibuffer for prompts
              query-user-mail-address nil
              display-warning-minimum-level 'error            ; Turn off annoying warning messages
              disabled-command-function nil                   ; Don't second-guess advanced commands
              delete-key-deletes-forward t                    ; Make delete key work normally
              kill-read-only-ok t                             ; Silently copy in read-only buffers
              column-number-mode t                            ; Display line and column numbers
              line-number-mode t
              tab-width 4                                     ; Set tab stops
              tab-stop-list (number-sequence 4 120 4)
              indent-tabs-mode nil                            ; Use spaces only, no tabs
              tabify-regexp "^\t* [ \t]+"                     ; Tabify only initial whitespace
              page-delimiter "^\\s *\n"                       ; Page delim is one or more blank lines
              minibuffer-max-depth nil                        ; Mini-buffer settings
              toolbar-print-function 'ps-print-buffer-with-faces ; Set the print button to print nice PS
              ps-line-number t
              ps-n-up-printing 2
              ps-print-color-p nil
              fill-column 75                                  ; Wrap lines at 75th column
              initial-major-mode 'text-mode                   ; Text mode, not Elisp
              case-fold-search t                              ; Fold case on searches
              buffers-menu-sort-function 'sort-buffers-menu-by-mode-then-alphabetically ; Buffers menu settings
              buffers-menu-grouping-function 'group-buffers-menu-by-mode-then-alphabetically
              buffers-menu-submenus-for-groups-p t
              ibuffer-default-sorting-mode 'filename/process  ; Group buffers primarily by directory
              uniquify-buffer-name-style 'forward             ; Prepend unique dirs when names clash
              uniquify-after-kill-buffer-p t
              uniquify-ignore-buffers-re "^\\*"
              major-mode 'major-mode-from-name                ; Set mode for empty buffers by name
              ispell-program-name "aspell"                    ; Use aspell to spell check
              gdb-many-windows t                              ; Show GUD in all its glory
              ediff-split-window-function 'split-window-horizontally ; Show diffs side-by-side
              diff-switches "-u"                              ; Prefer unified over context diffs
              org-support-shift-select t                      ; Don't nuke shift selection in org-mode
              calc-display-sci-low -5                         ; More zeros before scientific notation
              max-lisp-eval-depth 10000
              max-specpdl-size 5  ; default is 1000, reduce the backtrace level
              )
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq file-name-coding-system 'utf-8)

(defun startup-echo-area-message ()                           ; Use a more interesting startup message
  "By your command...")
(set-frame-parameter nil 'alpha 97)                           ; Make the window 90% opaque

(defun major-mode-from-name ()
  "Choose proper mode for buffers created by switch-to-buffer."
  (let ((buffer-file-name (or buffer-file-name (buffer-name))))
    (set-auto-mode)))
(show-paren-mode t)                                           ; Highlight whole parenthetic expressions
(delete-selection-mode t)                                     ; Typed text replaces selection
(global-subword-mode t)                                       ; Treat CamelCase as multiple words
(auto-fill-mode t)                                            ; Automatically wrap lines
(fset 'yes-or-no-p 'y-or-n-p)                                 ; Yes or no prompts accept short y or n
(defalias 'yes-or-no-p 'y-or-n-p)
(require 'uniquify)                                           ; Actually do buffer renaming on clashes
(setq uniquify-buffer-name-style 'forward)
;; {{{ disable/enable
;; disable
(menu-bar-mode -1)
(when window-system
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (scroll-bar-mode -1)
  (set-scroll-bar-mode 'right)                                  ; Scrollbars should always be on the right
  (set-fringe-mode '(1 . 0))                                    ; Turn off the left fringe
  )
(setq make-backup-files nil) ; stop creating those backup~ files
;; don't show startup messages
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; enable
(global-auto-revert-mode 1)
(setq view-read-only 1)
(show-paren-mode 1)
(column-number-mode t)  ; show column numbers
(global-linum-mode 1)   ; show line numbers (on the left)
;; tabbar
(require 'tabbar)
;; enable recent files mode.
(require 'recentf)
(require 'anything)
(require 'smex)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ;; ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ;; ("SC" . "http://joseito.republika.pl/sunrise-commander/")
                         ))
(when window-system
  (powerline-default-theme)
  (load-theme 'solarized-dark t))
;; }}}


;; for platform

;; if linux

;; else for unix

;; else for window
