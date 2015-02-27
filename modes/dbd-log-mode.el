;;; dbd-log-mode.el -- describe log mode

(provide 'dbd-log-mode)

;;;###autoload
(define-derived-mode dbd-log-mode text-mode "Log mode"
  )

(defun xah-xml-mode-keys ()
  "my keys for `xml-mode'."
  (interactive)
  (local-set-key (kbd "<f8>") 'browse-url-of-buffer)
  (local-set-key (kbd "<tab>") 'html-next-content)
  (local-set-key (kbd "S-<tab>") 'html-previous-content)
  )
