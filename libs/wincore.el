;;; wincore.el -- contain some function for window

(provide 'wincore)

;;============================================================
;; dbd:install
;; install package on window
;; https://chocolatey.org/
;;============================================================
(defun dbd:prebuilt-install (package)
  (interactive)
  (start-process "install chocolatey" "*dbd prebuilt install*"
                 "start"
                 "@powershell -NoProfile -ExecutionPolicy unrestricted -Command \"iex ((new-object net.webclient).DownloadString(\'https:\/\/chocolatey.org\/install.ps1\'))\" && SET PATH=%PATH%;%ALLUSERSPROFILE%\chocolatey\bin")

  )

;;============================================================
;; dbd:buffer-rename
;; rename current buffer name with file path like npp
;;
;;============================================================
(defun dbd:buffer-rename ()
  )


;;; end win-core.el
