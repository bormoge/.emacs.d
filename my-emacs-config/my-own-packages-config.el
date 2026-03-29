(use-package package-build
  :ensure nil
  :vc (:url "https://github.com/melpa/package-build"
            :rev :newest
            :branch "master"
            :vc-backend Git)
  ) ;; Functions to remember: package-build-archive, package-build-current-recipe, package-build-create-recipe

(use-package package-lint
  :ensure t
  ;; :load-path "~/.emacs.d/elpa/package-lint/"
  ;; :vc (:url "https://github.com/purcell/package-lint"
  ;;           :rev :newest
  ;;           :branch "master"
  ;;           :vc-backend Git)
  ) ;; Functions to remember: package-lint-flymake-setup, flymake-show-buffer-diagnostics, package-lint-current-buffer

(use-package package-lint-flymake
  :ensure t)

(use-package guava-themes
  :ensure nil
  :load-path "~/.emacs.d/elpa/guava-themes/"
  ;; :custom
  ;; (guava-themes-visible-bell-duration 0.3)
  ;; (guava-themes-visible-bell-idle-delay 0.3)
  :config
  (setq ring-bell-function #'guava-themes-change-visible-bell
        visible-bell t)
  (load-theme 'guava-themes-rubus t)
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
