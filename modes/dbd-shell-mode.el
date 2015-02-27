;;; dbd-shell-mode.el -- describe log mode

(provide 'dbd-shell-mode)

(require 'sh-script)

;;;###autoload
(define-derived-mode dbd-shell-mode sh-mode "dbd sh  mode"
  (message "Load dbd-shell-mode" )
  )

(defun xah-xml-mode-keys ()
  "my keys for `xml-mode'."
  (interactive)
  (local-set-key (kbd "<f8>") 'browse-url-of-buffer)
  (local-set-key (kbd "<tab>") 'html-next-content)
  (local-set-key (kbd "S-<tab>") 'html-previous-content)
  )
