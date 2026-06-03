(use-package package-build
  :vc (:url "https://github.com/melpa/package-build"
            :rev :newest
            :branch "master"
            :vc-backend Git)
  ) ;; Functions to remember: package-build-archive, package-build-current-recipe, package-build-create-recipe

(use-package package-lint
  :ensure t
  ) ;; Functions to remember: package-lint-flymake-setup, flymake-show-buffer-diagnostics, package-lint-current-buffer

(use-package package-lint-flymake
  :ensure t
  )

(use-package guava-themes
  :ensure nil
  :load-path "~/.emacs.d/elpa/guava-themes/"
  :custom
  (guava-themes-visible-bell-duration 0.15)
  (guava-themes-visible-bell-idle-delay 0.0)
  (ring-bell-function #'guava-themes-change-visible-bell)
  (visible-bell t)
  :config
  (load-theme 'guava-themes-vaccinium t) ;; guava-themes-vaccinium
  (set-face-attribute 'mode-line nil :font "JuliaMono Light 12")
  (set-face-attribute 'tab-line nil :font "JuliaMono Light 12" :weight 'bold)
  (set-face-attribute 'tab-bar nil :font "JuliaMono Light 12" :weight 'bold)
  (set-face-attribute 'ansi-color-blue nil :foreground "deep sky blue" :background "deep sky blue")
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
