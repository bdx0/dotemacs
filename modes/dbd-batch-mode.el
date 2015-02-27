;;; dbd-batch-mode.el -- describe log mode

(provide 'dbd-batch-mode)

(require 'batch-mode nil 'noerror)


(defun xah-xml-mode-keys ()
  "my keys for `xml-mode'."
  (interactive)
  (local-set-key (kbd "<f8>") 'browse-url-of-buffer)
  (local-set-key (kbd "<tab>") 'html-next-content)
  (local-set-key (kbd "S-<tab>") 'html-previous-content)
  )

;;;###autoload
(defun dbd-batch-mode ()
  "dbd batch mode"
  (batch-mode)
  (message "Load dbd-batch-mode"))

;;
;;
;;
;;; end dbd-batch-mode.el
