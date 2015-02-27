;; ;; init.el
;; ;; set local recipes, el-get-sources should only accept PLIST element
;; (setq
;;  el-get-sources
;;  '((:name buffer-move           ; have to add your own keys
;;           :after (progn
;;                    (global-set-key (kbd "<C-S-up>")     'buf-move-up)
;;                    (global-set-key (kbd "<C-S-down>")   'buf-move-down)
;;                    (global-set-key (kbd "<C-S-left>")   'buf-move-left)
;;                    (global-set-key (kbd "<C-S-right>")  'buf-move-right)))
;; 
;;    (:name smex              ; a better (ido like) M-x
;;           :after (progn
;;                    (setq smex-save-file "~/.emacs.d/.smex-items")
;;                    (global-set-key (kbd "M-x") 'smex)
;;                    (global-set-key (kbd "M-X") 'smex-major-mode-commands)))
;; 
;;    (:name magit             ; git meet emacs, and a binding
;;           :after (progn
;;                    (global-set-key (kbd "C-x C-z") 'magit-status)))
;; 
;;    (:name goto-last-change      ; move pointer back to last change
;;           :after (progn
;;                    ;; when using AZERTY keyboard, consider C-x C-_
;;                    (global-set-key (kbd "C-x C-/") 'goto-last-change)))))
;; 
;; ;; now set our own packages
;; (setq
;;  my:el-get-packages
;;  '(el-get               ; el-get is self-hosting
;;    escreen                      ; screen for emacs, C-\ C-h
;;    php-mode-improved            ; if you're into php...
;;    switch-window            ; takes over C-x o
;;    auto-complete            ; complete as you type with overlays
;;    zencoding-mode           ; http://www.emacswiki.org/emacs/ZenCoding
;;    color-theme                      ; nice looking emacs
;;    color-theme-tango))                  ; check out color-theme-solarized
;; 
;;                                         ;
;; ;; Some recipes require extra tools to be installed
;; ;;
;; ;; Note: el-get-install requires git, so we know we have at least that.
;; ;;
;; ;; (when (el-get-executable-fi;; nd "cvs")
;; ;; (add-to-list 'my:el-get-packages 'emacs-goodies-el)) ; the debian addons for emacs
;; 
;; (when (el-get-executable-find "svn")
;;   (loop for p in '(psvn         ; M-x svn-status
;;                    yasnippet        ; powerful snippet mode
;;                    )
;;         do (add-to-list 'my:el-get-packages p)))
;; 
;; (setq my:el-get-packages
;;       (append my:el-get-packages
;;               (mapcar #'el-get-source-name el-get-sources)))
;; 
;; ;; install new packages and init already installed packages
;; (el-get 'sync my:el-get-packages)
;; 
;; ;; }}}
;; 
;; ;; require as default {{{
;; ;; {{{ tabbar is first
;; (require 'tabbar)
;; ;; Tabbar settings
;; (set-face-attribute
;;  'tabbar-default nil
;;  :background "gray20"
;;  :foreground "gray20"
;;  :box '(:line-width 1 :color "gray20" :style nil))
;; (set-face-attribute
;;  'tabbar-unselected nil
;;  :background "gray30"
;;  :foreground "white"
;;  :box '(:line-width 5 :color "gray30" :style nil))
;; (set-face-attribute
;;  'tabbar-selected nil
;;  :background "gray75"
;;  :foreground "black"
;;  :box '(:line-width 5 :color "gray75" :style nil))
;; (set-face-attribute
;;  'tabbar-highlight nil
;;  :background "white"
;;  :foreground "black"
;;  :underline nil
;;  :box '(:line-width 5 :color "white" :style nil))
;; (set-face-attribute
;;  'tabbar-button nil
;;  :box '(:line-width 1 :color "gray20" :style nil))
;; (set-face-attribute
;;  'tabbar-separator nil
;;  :background "gray20"
;;  :height 0.6)
;; 
;; ;; Change padding of the tabs
;; ;; we also need to set separator to avoid overlapping tabs by highlighted tabs
;; (custom-set-variables
;;  '(tabbar-separator (quote (0.5))))
;; ;; adding spaces
;; (defun tabbar-buffer-tab-label (tab)
;;   "Return a label for TAB.
;; That is, a string used to represent it on the tab bar."
;;   (let ((label  (if tabbar--buffer-show-groups
;;                     (format "[%s]  " (tabbar-tab-tabset tab))
;;                   (format "%s  " (tabbar-tab-value tab)))))
;;     ;; Unless the tab bar auto scrolls to keep the selected tab
;;     ;; visible, shorten the tab label to keep as many tabs as possible
;;     ;; in the visible area of the tab bar.
;;     (if tabbar-auto-scroll-flag
;;         label
;;       (tabbar-shorten
;;        label (max 1 (/ (window-width)
;;                        (length (tabbar-view
;;                                 (tabbar-current-tabset)))))))))
;; 
;; ;; }}}
;; ;;  }}}
