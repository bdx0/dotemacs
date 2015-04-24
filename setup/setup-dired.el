;;; setup-dired.el --- 
;; Author: dbd
;; Version: 0.0.1

;;; Commentary:
;;
;;

;;; Code:

(require 'dired)
(require 'dired-x)
(require 'dired-details)
(require 'wdired)

(global-set-key (kbd "C-x d") 'dired)
(define-key dired-mode-map "\C-cb" 'org-ibuffer)
(define-key dired-mode-map ")" 'dired-details-toggle)
(define-key dired-mode-map "\C-cg" 'grep-find)
(define-key dired-mode-map "\C-cd" 'dired-clean-tex)

(setq directory-free-space-args "-Pkh")
(setq list-directory-verbose-switches "-al")
(setq dired-listing-switches "-l")
(setq dired-dwim-target t)
(setq dired-omit-mode nil)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq delete-old-versions t)

(setq dired-guess-shell-alist-user
      (list
       ;;       (list "\\.pdf$" "acroread")
       (list "\\.pdf$" "mupdf")
       (list "\\.docx?$" "libreoffice")
       (list "\\.aup?$" "audacity")
       (list "\\.pptx?$" "libreoffice")
       (list "\\.odf$" "libreoffice")
       (list "\\.odt$" "libreoffice")
       (list "\\.odt$" "libreoffice")
       (list "\\.kdenlive$" "kdenlive")
       (list "\\.svg$" "gimp")
       (list "\\.csv$" "libreoffice")
       (list "\\.sla$" "scribus")
       (list "\\.ods$" "libreoffice")
       (list "\\.odp$" "libreoffice")
       (list "\\.xls$" "libreoffice")
       (list "\\.xlsx$" "libreoffice")
       (list "\\.txt$" "gedit")
       (list "\\.sql$" "gedit")
       (list "\\.css$" "gedit")
       (list "\\.html$" "w3m")
       (list "\\.jpe?g$" "gqview")
       (list "\\.png$" "gqview")
       (list "\\.gif$" "gqview")
       (list "\\.psd$" "gimp")
       (list "\\.xcf" "gimp")
       (list "\\.odt$" "libreoffice")
       (list "\\.xo$" "unzip")
       (list "\\.3gp$" "vlc")
       (list "\\.mp3$" "vlc")
       (list "\\.flac$" "vlc")
       (list "\\.avi$" "mplayer -fs")
       ;; (list "\\.og[av]$" "vlc")
       (list "\\.wm[va]$" "vlc")
       (list "\\.flv$" "mplayer -fs")
       (list "\\.mov$" "mplayer -fs")
       (list "\\.divx$" "mplayer -fs")
       (list "\\.mp4$" "mplayer -fs")
       (list "\\.mkv$" "mplayer -fs")
       (list "\\.mpe?g$" "mplayer -fs")
       (list "\\.m4[av]$" "mplayer -fs")
       (list "\\.mp2$" "vlc")
       (list "\\.pp[st]$" "libreoffice")
       (list "\\.ogg$" "vlc")
       (list "\\.ogv$" "mplayer -fs")
       (list "\\.rtf$" "libreoffice")
       (list "\\.ps$" "gv")
       (list "\\.mp3$" "play")
       (list "\\.wav$" "vlc")
       (list "\\.rar$" "unrar x")
       ))

(setq dired-tex-unclean-extensions
      '(".toc" ".log" ".aux" ".dvi" ".out" ".nav" ".snm"))

(setq inferior-lisp-program "sbcl")
