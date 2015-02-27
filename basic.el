;; markdown.el
;; basic.el
;; usage: emacs -ql basic.el %*
;; use for dev el

(setq frame-title-format "init-basic.el")

;; {{{ Utility
;; config shortcut
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-q") 'kill-this-buffer)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-S-x") 'anything)
(global-set-key (kbd "C-x C-f") 'ido-find-file)
(global-set-key (kbd "<M-f1>") (lambda () (interactive) (progn
                                                          (sr-speedbar-toggle)
                                                          (sr-speedbar-select-window))))
(global-set-key (kbd "M-\\") 'find-file-at-point) ;http://stackoverflow.com/questions/3124844/what-are-your-favorite-global-key-bindings-in-emacs

;; }}}
;; just simple
;; http://sachachua.com/blog/2014/06/read-lisp-tweak-emacs-beginner-34-can-make-things-convenient/
;; http://www.emacswiki.org/emacs/IncrementalSearch


;; for fuzzy
;; https://github.com/d11wtq/grizzl
;; https://github.com/d11wtq/fiplr
;; http://www.emacswiki.org/emacs/Icicles_-_Fuzzy_Completion#FuzzyMatchCompletion
;;
