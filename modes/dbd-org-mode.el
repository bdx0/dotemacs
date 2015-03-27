;;; dbd-org-mode.el -- dbd org mode define
;; follow http://pages.sachachua.com/.emacs.d/Sacha.html#unnumbered-6
;; http://members.optusnet.com.au/~charles57/GTD/gtd_workflow.html


(provide 'dbd-org-mode)
(require 'cl-lib)
(require 'org)

(defvar dbd-org-mode-hook nil)

;;;###autoload
(define-derived-mode dbd-org-mode org-mode "dbd-org-mode"

  )

;; (add-hook 'org-mode-hook 'dbd-org-mode)

;;; end dbd-org-mode.el
