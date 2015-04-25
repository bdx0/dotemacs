;;; some-new.el --- some new function
;; Author: dbd
;; Version: 0.0.1

;;; Commentary:
;;


;;; Code:

(defun ibuffer-previous-line ()
    (interactive) (previous-line)
    (if (<= (line-number-at-pos) 2)
        (goto-line (- (count-lines (point-min) (point-max)) 2))))

(defun ibuffer-next-line ()
    (interactive) (next-line)
    (if (>= (line-number-at-pos) (- (count-lines (point-min) (point-max)) 1))
        (goto-line 3)))

(define-key ibuffer-mode-map (kbd "<up>") 'ibuffer-previous-line)
(define-key ibuffer-mode-map (kbd "<down>") 'ibuffer-next-line)
;;==========================================
;; http://ergoemacs.org/emacs/emacs_hotkey_open_file_fast.html
(require 'ido) ; part of emacs

(defvar xah-filelist nil "alist for files i need to open frequently. Key is a short abbrev string, Value is file path string.")

(setq xah-filelist
      '(
        ("3emacs" . "~/web/ergoemacs_org/emacs/blog.html" )
        ("4code" . "~/web/xahlee_info/comp/blog.html" )
        ("keys" . "~/git/xah_emacs_init/xah_emacs_keybinding.el" )
        ("download" . "~/Downloads/" )
        ("pictures" . "~/Pictures/" )
        ;; more here
        ) )

(defun xah-open-file-fast ()
  "Prompt to open a file from `xah-filelist'.
URL `http://ergoemacs.org/emacs/emacs_hotkey_open_file_fast.html'
Version 2015-04-23"
  (interactive)
  (let ((ξabbrevCode
         (ido-completing-read "Open:" (mapcar (lambda (ξx) (car ξx)) xah-filelist))))
    (find-file (cdr (assoc ξabbrevCode xah-filelist)))))
;; =========================================

;; =========================================
;; http://ergoemacs.org/emacs/emacs_open_file_path_fast.html
(defun xah-open-file-at-cursor ()
  "Open the file path under cursor.
If there is text selection, uses the text selection for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” that indicates line number. If so, jump to that line number.
If path does not have a file extension, automatically try with “.el” for elisp files.
This command is similar to `find-file-at-point' but without prompting for confirmation.

URL `http://ergoemacs.org/emacs/emacs_open_file_path_fast.html'"
  (interactive)
  (let ((ξpath (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (let (p0 p1 p2)
                   (setq p0 (point))
                   ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
                   (skip-chars-backward "^  \"\t\n`'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\`")
                   (setq p1 (point))
                   (goto-char p0)
                   (skip-chars-forward "^  \"\t\n`'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\'")
                   (setq p2 (point))
                   (goto-char p0)
                   (buffer-substring-no-properties p1 p2)))))
    (if (string-match-p "\\`https?://" ξpath)
        (browse-url ξpath)
      (progn ; not starting “http://”
        (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\'" ξpath)
            (progn
              (let (
                    (ξfpath (match-string 1 ξpath))
                    (ξline-num (string-to-number (match-string 2 ξpath))))
                (if (file-exists-p ξfpath)
                    (progn
                      (find-file ξfpath)
                      (goto-char 1)
                      (forward-line (1- ξline-num)))
                  (progn
                    (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" ξfpath))
                      (find-file ξfpath))))))
          (progn
            (if (file-exists-p ξpath)
                (find-file ξpath)
              (if (file-exists-p (concat ξpath ".el"))
                  (find-file (concat ξpath ".el"))
                (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" ξpath))
                  (find-file ξpath ))))))))))
;; =========================================

;; =========================================
;; http://ergoemacs.org/emacs/elisp_thing-at-point.html
(defun xx ()
  "print current word."
  (interactive)
  (message "%s" (thing-at-point 'word)))

(defun xx2 ()
  "print current word."
  (interactive)
  (let (p1 p2)
    (save-excursion
      (skip-chars-backward "-a-z0-9")
      (setq p1 (point))
      (skip-chars-forward "-a-z0-9")
      (setq p2 (point))
      (message "%s" (buffer-substring-no-properties p1 p2)))))
;; =========================================
