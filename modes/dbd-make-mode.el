;;; dbd-make-mode.el -- describe gnu make mode in emacs

(provide 'dbd-make-mode)

(require 'make-mode)

;;;###autoload
(define-derived-mode dbd-make-mode makefile-mode "dbd-make-mode"
  )
