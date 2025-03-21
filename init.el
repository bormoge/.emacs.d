;; Check init speed
;;(profiler-start 'cpu)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes 'nil)
 '(package-selected-packages
   '(pgmacs pg all-the-icons all-the-icons-dired treemacs-tab-bar treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil forge projectile evil yasnippet lsp-treemacs treemacs minimap dap-mode lsp-ui lsp-mode doom-themes magit diff-hl))
 '(package-vc-selected-packages
   '((pgmacs :vc-backend Git :url "https://github.com/emarsden/pgmacs")
     (pg :vc-backend Git :url "https://github.com/emarsden/pg-el"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-file "~/.emacs.d/my-emacs-config/functions/el-user-files.el")

(load-user-file "my-emacs-config/functions/the-entire-line.el")
(load-user-file "my-emacs-config/functions/tnwmt.el")
(load-user-file "my-emacs-config/my-packages-config.el")

;; Apply configuration after loading .el files
(load-user-file "my-emacs-config/my-emacs-config.el")

;; Comment this if you don't have the file
;; (load-user-file "my-emacs-config/functions/tetris.el")

;; A different way of loading files
;; (let ((configuration-directory (concat user-emacs-directory "my-emacs-config/")))
;;   (load (concat configuration-directory "the-entire-line.el")))

;; Finish checking init speed
;;(profiler-report)
;;(profiler-stop)
