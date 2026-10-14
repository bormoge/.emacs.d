;;; my-packages-config.el --- Configuration for Emacs packages I've authored -*- lexical-binding: t; -*-

(use-package guava-themes
  :ensure nil
  :demand t
  :load-path "~/.emacs.d/elpa/guava-themes/"
  :custom
  (guava-themes-visible-bell-duration 0.15)
  (guava-themes-visible-bell-idle-delay 0.0)
  (ring-bell-function #'guava-themes-change-visible-bell)
  (visible-bell t)
  :config
  (if (my/podman-container-p)
      (load-theme 'guava-themes-rubus t) ;; guava-themes-rubus
    (load-theme 'guava-themes-vaccinium t)) ;; guava-themes-vaccinium
  (set-face-attribute 'mode-line nil :font "IoskeleyMonoTerm Nerd Font Light 14")
  (set-face-attribute 'tab-line nil :font "IoskeleyMonoTerm Nerd Font Light 14" :weight 'bold)
  (set-face-attribute 'tab-line-tab-modified nil :font "IoskeleyMonoTerm Nerd Font Light 14" :weight 'bold)
  (set-face-attribute 'tab-line-tab-special nil :font "IoskeleyMonoTerm Nerd Font Light 14" :weight 'bold)
  (set-face-attribute 'tab-bar nil :font "IoskeleyMonoTerm Nerd Font Light 16" :weight 'bold)
  )

;; (use-package guava-themes
;;   :vc (:url "https://github.com/bormoge/guava-themes"
;;             :rev :newest
;;             :branch "main"
;;             :vc-backend Git)
;;   :ensure t
;;   :config
;;   (setq ring-bell-function #'guava-themes-change-visible-bell)
;;   (setq visible-bell t)
;;   (load-theme 'guava-themes-acer t)
;;   )

;; (unless (package-installed-p 'guava-themes)
;;   (package-vc-install "https://github.com/bormoge/guava-themes" nil nil 'guava-themes))

;; (require 'guava-themes)

;; (setq ring-bell-function #'guava-themes-change-visible-bell)
;; (setq visible-bell t)
;; (load-theme 'guava-themes-psidium t)

;; End:

;;; my-own-packages-config.el ends here
