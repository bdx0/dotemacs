;; libs.el -- contain functions for emacs

(defun req-w-elget (feature elget-name)
  (condition-case nil
      (require feature)
    (error (progn
             (condition-case nil
                 (el-get 'sync elget-name)
               (error (progn
                        (message "could not require %s" feature)
                        nil)))
             nil))))

                                        ; derived from ELPA installation
                                        ; http://tromey.com/elpa/install.html
(defun dbd:eval-url (url)
  (let ((buffer (url-retrieve-synchronously url)))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move)
      (eval-region (point) (point-max))
      (kill-buffer (current-buffer)))))

(defun dbd:load-file (url)
  )

;;========================================================
;; unixの部屋でunixコマンドをemacsから検索する
;; http://d.hatena.ne.jp/a666666/20110721/1311262558
;; のコメント欄から応用
;;========================================================
(defun man-search ()
  "unixコマンドを検索する"
  (interactive)
  (let ((word (read-from-minibuffer "search word: ")))
    (setq word (url-encode-url word))
    (browse-url (format "http://x68000.q-e-d.net/~68user/unix/pickup?keyword=%s&target=command" word))))

;;========================================================
;; open current buffer in brower
;; http://stackoverflow.com/questions/2035678/how-to-open-files-in-web-browsers-e-g-firefox-within-editors-like-vim-or-emacs/2035987#2035987
;;========================================================
(defun open-in-browser()
  (interactive)
  (let ((filename (buffer-file-name)))
    (browse-url (concat "file://" filename))))


;;========================================================
;; switch to previous buffer
;;http://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
;;========================================================
(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))


;;========================================================
;; hook-into-modes
;; https://github.com/capitaomorte/yasnippet/issues/457#issuecomment-56529735
;;========================================================
(defun exampl1()
  (defmacro hook-into-modes (func modes)
    `(dolist (mode-hook ,modes)
       (add-hook mode-hook ,func)))

  (require 'yasnippet)
  (hook-into-modes #'(lambda () (yas-minor-mode 1))
                   '(prog-mode-hook
                     org-mode-hook
                     ruby-mode-hook
                     message-mode-hook
                     gud-mode-hook
                     erc-mode-hook))
  (define-key yas-minor-mode-map (kbd "C-<return>") 'yas-expand)
  (define-key yas-minor-mode-map (kbd "M-<return>") 'yas-expand)
  )


;;========================================================
;; toggle bar mode
;; http://www.emacswiki.org/emacs/FullScreen
;;========================================================
(defun toggle-bars ()
  "Toggles bars visibility."
  (interactive)
  (menu-bar-mode)
  (tool-bar-mode)
  (scroll-bar-mode))

(defun jbr-init ()
  "Called from term-setup-hook after the default
terminal setup is
done or directly from startup if term-setup-hook not
used.  The value
0xF030 is the command for maximizing a window."
  (interactive)
  (w32-send-sys-command #xf030)
  (ecb-redraw-layout)
  (calendar)
  )

;;========================================================
;; ffap-w-line
;; http://stackoverflow.com/questions/3139970/open-a-file-at-line-with-filenameline-syntax
;;========================================================
(defun find-file-at-point-with-line()
  "if file has an attached line num goto that line, ie boom.rb:12"
  (interactive)
  (setq line-num 0)
  (save-excursion
    (search-forward-regexp "[^ ]:" (point-max) t)
    (if (looking-at "[0-9]+")
         (setq line-num (string-to-number (buffer-substring (match-beginning 0) (match-end 0))))))
  (find-file-at-point)
  (if (not (equal line-num 0))
      (goto-line line-num)))
