(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(misterioso)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-file "~/.emacs.d/my-emacs-config/functions/load-user-file.el")

(load-user-file "my-emacs-config/my-emacs-config.el")
(load-user-file "my-emacs-config/functions/copy-the-entire-line.el")
(load-user-file "my-emacs-config/functions/my-desktop-sessions.el")
