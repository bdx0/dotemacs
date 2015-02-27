;;; bootstrap.el -- this is a small elisp lib for the elisp ide to load.
;; usage: emacs -ql basic.el %*
;; use for dev el
;; (setq debug-on-error t)
;; http://iridia.ulb.ac.be/~manuel/dotemacs.html
;;; ========================================================
(defvar *emacs-load-start* (current-time))
(defconst dbd-file-init-el (or load-file-name buffer-file-name))
(defun dbd-reload()
  (interactive)
  (load-file dbd-file-init-el))
;; ===============================================================
(eval-when-compile
  (require 'cl)
  (require 'gnus-sum))
;;(add-to-list 'load-path (concat (getenv emacs_dir) "dotfiles/dbd/elisp/")))

(setq frame-title-format "self-tools-ide")
;; autoload, eval-after-load, add-hook, try-require ???
;; load init.el flow
;; 0. require load elisp lib basic-func.el & autoload.el & env.el
(load-file (concat (file-name-directory (or load-file-name buffer-file-name)) "basic-func.el"))
(load-file (concat (file-name-directory (or load-file-name buffer-file-name)) "elisp.el"))
(load-file (concat (file-name-directory (or load-file-name buffer-file-name)) "dbd-autoload.el"))
(load-file (concat (file-name-directory (or load-file-name buffer-file-name)) "modes.el"))
(load-file (concat (file-name-directory (or load-file-name buffer-file-name)) "env.el"))

;; {{{ Utility
;; config shortcut
(global-set-key (kbd "M-q") 'kill-this-buffer)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-\\") 'find-file-at-point) ;http://stackoverflow.com/questions/3124844/what-are-your-favorite-global-key-bindings-in-emacs
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-S-x") 'anything)
(global-set-key (kbd "C-x C-f") 'ido-find-file)
(global-set-key (kbd "<M-f1>") (lambda () (interactive) (progn
                                                          (sr-speedbar-toggle)
                                                          (sr-speedbar-select-window))))
;; }}}

;; config for elisp
;; (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
;;     (let ((default-directory  "~/.emacs.d/elpa/"))
;;       (normal-top-level-add-subdirs-to-load-path)))

(global-set-key (kbd "<C-return>") 'dbd:auto-complete)

(eshell)
;; ========= special ===============
;; (add-hook 'after-init-hook (lambda () (load "<real init file>")))
;; ================================

;; config styles

;; just simple
;; http://sachachua.com/blog/2014/06/read-lisp-tweak-emacs-beginner-34-can-make-things-convenient/
;; http://www.emacswiki.org/emacs/IncrementalSearch


;; for fuzzy
;; https://github.com/d11wtq/grizzl
;; https://github.com/d11wtq/fiplr
;; http://www.emacswiki.org/emacs/Icicles_-_Fuzzy_Completion#FuzzyMatchCompletion
;;

;; http://www.johndcook.com/emacs_windows.html
;; begin emacs with windows

;; https://github.com/larstvei/dot-emacs/blob/master/init.org
;; http://stackoverflow.com/questions/1144729/how-do-i-prevent-emacs-from-horizontally-splitting-the-screen-when-opening-multip
;; http://stackoverflow.com/questions/3811126/do-you-use-emacs-tabbar
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Standard-Hooks.html'
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Running-Hooks.html
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Hooks.html
;; http://www.mygooglest.com/fni/dot-emacs.html
;; http://www.eyrie.org/~eagle/notes/cvs/emacs.html
;; https://github.com/dholm/dotemacs/blob/master/.emacs.d/basic.el
;; https://github.com/jwiegley/dot-emacs/blob/master/org-settings.el
;; https://github.com/Fuco1/smartparens



;; https://github.com/laynor/emacs-conf/blob/master/init.el
;; https://github.com/tomiacannondale/dot.emacs.d/blob/master/init.el

;; https://github.com/larstvei/dot-emacs/blob/master/init.org
;; http://stackoverflow.com/questions/1144729/how-do-i-prevent-emacs-from-horizontally-splitting-the-screen-when-opening-multip
;; http://stackoverflow.com/questions/3811126/do-you-use-emacs-tabbar
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Standard-Hooks.html'
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Running-Hooks.html
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Hooks.html
;; http://www.mygooglest.com/fni/dot-emacs.html
;; http://www.eyrie.org/~eagle/notes/cvs/emacs.html
;; https://github.com/dholm/dotemacs/blob/master/.emacs.d/basic.el
;; https://github.com/jwiegley/dot-emacs/blob/master/org-settings.el
;; https://github.com/Fuco1/smartparens
;; http://stackoverflow.com/questions/778716/how-can-i-make-emacs-start-up-faster
;; http://www.emacswiki.org/emacs/OptimizingEmacsStartup
;; https://github.com/noahfrederick/dots/blob/master/emacs.d/emacs.org

;; http://issrl.cs.memphis.edu/~jdgarrtt/software/dotemacs
;; http://www.cs.utah.edu/~aek/code/init.el.html
;; http://cjohansen.no/an-introduction-to-elisp#goto-algorithm
;; https://github.com/magnars/.emacs.d/blob/master/init.el

;; (message "My .emacs loaded in %ds" (destructuring-bind (hi lo ms) (current-time) (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))


;; http://emacs-fu.blogspot.com/2011/09/finding-just-about-anything.html
;; http://milkbox.net/note/single-file-master-emacs-configuration/
;; http://stackoverflow.com/questions/18996977/super-key-binding-in-emacs
;; http://emacsredux.com/blog/2013/07/17/make-use-of-the-super-key/
;; https://github.com/joedicastro/dotfiles
