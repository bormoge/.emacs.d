;; Check init speed
;;(profiler-start 'cpu)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

;; This needs to be set before use-package is loaded
(setq use-package-enable-imenu-support t)

;; Apply vanilla configuration
(load-file (expand-file-name "my-emacs-config/my-emacs-config.el" user-emacs-directory))

;; Load theme(s)
(load-file (expand-file-name "my-emacs-config/my-themes-config.el" user-emacs-directory))

;; Apply packages configuration
(load-file (expand-file-name "my-emacs-config/my-packages-config.el" user-emacs-directory))

;; Define configuration environment for packages authored by me.
(load-file (expand-file-name "my-emacs-config/my-own-packages-config.el" user-emacs-directory))

;; Finish checking init speed
;;(profiler-report)
;;(profiler-stop)
