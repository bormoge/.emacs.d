(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(modus-vivendi)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-file "~/.emacs.d/my-emacs-config/functions/load-user-file.el")

(load-user-file "my-emacs-config/functions/the-entire-line.el")
(load-user-file "my-emacs-config/functions/my-desktop-sessions.el")

;; Apply configuration after loading .el files
(load-user-file "my-emacs-config/my-emacs-config.el")

;; Comment this if you don't have the file
;; (load-user-file "my-emacs-config/functions/tetris.el")
