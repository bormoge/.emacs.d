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

(setq package-user-dir "~/.emacs.d/elpa")

(load-file "~/.emacs.d/my-emacs-config/functions/flnkf.el")

;; Load theme(s)
(load-file "~/.emacs.d/my-emacs-config/my-themes-config.el")

;; Apply packages configuration
(load-file "~/.emacs.d/my-emacs-config/my-packages-config.el")

;; Apply configuration after loading .el files
(load-file "~/.emacs.d/my-emacs-config/my-emacs-config.el")

;; Comment this if you don't have the file
;; (load-file "~/.emacs.d/my-emacs-config/functions/tetris.el")

;; A different way of loading files
;; (let ((configuration-directory (concat user-emacs-directory "my-emacs-config/")))
;;   (load (concat configuration-directory "the-entire-line.el")))

;; Finish checking init speed
;;(profiler-report)
;;(profiler-stop)
