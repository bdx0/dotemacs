;;; hook-config.el --- some default hook need to modify
;; Author: dbd
;; Version: 0.0.1

;;; Commentary:
;;

;;; Code:

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; {{{ c-mode
;; add in your commonly used packages/include directories here, for
;; example, SDL or OpenGL. this shouldn't slow down cpp, even if
;; you've got a lot of them
(setq c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ ")
(load "c-eldoc")
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
;; }}}

;; {{{ el mode
(add-hook 'emacs-lisp-mode-hook 'turn-on-orgstruct)
(add-hook 'emacs-lisp-mode-hook 'fontify-todo)
(add-hook 'emacs-lisp-mode-hook 'fontify-headline)
(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'emacs-lisp-mode-hook 'electric-indent-mode 'append)

(add-hook 'clojure-mode-hook 'turn-on-orgstruct)
(add-hook 'clojure-mode-hook 'fontify-todo)
(add-hook 'clojure-mode-hook 'fontify-headline)
(add-hook 'emacs-lisp-mode-hook 'company-mode 'append)
(add-hook 'emacs-lisp-mode-hook 'electric-indent-mode 'append)

;; (require 'paredit)
;; (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
;; (add-hook 'clojure-mode-hook 'paredit-mode)
;; (add-hook 'emacs-lisp-mode-hook 'electric-layout-mode)
;; }}}
