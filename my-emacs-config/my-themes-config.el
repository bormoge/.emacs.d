(defun configure-theme-for-tab-line-tab-bar (THEME &optional NO-CONFIRM NO-ENABLE)
  "Configure `tab-bar-mode' and `tab-line-mode' after loading a theme."
  ;; Tab Bars
  (require 'tab-bar)

  (set-face-attribute 'tab-bar nil
                      ;;:background "#252526" ;;bg-alt
                      :foreground (face-attribute 'mode-line-emphasis :foreground))

  (set-face-attribute 'tab-bar-tab nil
		      :background (face-attribute 'mode-line :background)
		      :foreground (face-attribute 'mode-line-emphasis :foreground)
		      :weight 'bold
		      :height 1.0)

  (set-face-attribute 'tab-bar-tab-inactive nil
		      :background (face-background 'tab-bar)
		      :height 1.0)

  ;; Tab Lines
  (require 'tab-line)

  (set-face-attribute 'tab-line nil
                      ;;:background "#252526" ;;bg-alt
                      :foreground (face-attribute 'mode-line-emphasis :foreground))

  (set-face-attribute 'tab-line-tab nil
		      :background (face-attribute 'tab-line :background)
		      :height 0.9)

  (set-face-attribute 'tab-line-tab-current nil
		      :background (face-attribute 'mode-line :background)
		      :foreground (face-attribute 'mode-line-emphasis :foreground)
		      :weight 'bold
		      :height 0.9)

  (set-face-attribute 'tab-line-tab-inactive nil
		      :background (face-attribute 'tab-line :background)
		      :height 0.9)

  (set-face-attribute 'tab-line-tab-modified nil
        	      ;;:background "#252526"  ;; remove this line if you want dark-violet background on modified tab lines
        	      :foreground (face-attribute 'font-lock-string-face :foreground)  ;; alt: font-lock-string-face, error, which-func
        	      :height 0.9)

  (set-face-attribute 'tab-line-tab-special nil
		      :weight 'unspecified
		      :slant 'italic
		      :height 0.9)

  (when doom-dark+-blue-modeline
    (require 'which-func)
    (set-face-attribute 'which-func nil
		      :foreground (face-attribute 'mode-line :foreground))
    )
  )

(advice-add 'load-theme :after #'configure-theme-for-tab-line-tab-bar)

;; doom-themes
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
        doom-themes-enable-italic nil  ; if nil, italics is universally disabled
        doom-dark+-blue-modeline nil)
  (load-theme 'doom-dark+ t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; default "doom-atom"; use "doom-colors" for less minimal icon theme
  ;; (setq doom-themes-treemacs-theme "doom-colors")
  ;; (doom-themes-treemacs-config)
  (doom-themes-org-config)
  )

;; spacemacs-theme
;; (use-package spacemacs-theme
;;   :vc (:url "https://github.com/nashamri/spacemacs-theme"
;;        :rev :newest
;;        :branch "master")
;;   :ensure t
;;   :defer t)

;; catppuccin-theme
;; (use-package catppuccin-theme
;;   :ensure t
;;   :defer t
;;   :config
;;   (setq catppuccin-flavor 'mocha) ;; Values: 'frappe, 'latte, 'macchiato, 'mocha
;;   (catppuccin-reload)
;;   )

;; kanagawa-themes
;; (use-package kanagawa-themes
;;   :ensure t
;;   :defer t)

;; everforest-theme
;; (use-package everforest
;;   :vc (:url "https://github.com/Theory-of-Everything/everforest-emacs"
;;        :rev :newest
;;        :branch "master2")
;;   :ensure t
;;   :defer t)

;; ;; zenburn-theme.el
;; (use-package zenburn-theme
;;   :vc (:url "https://github.com/bbatsov/zenburn-emacs"
;;        :rev :newest
;;        :branch "master")
;;   :ensure t)

;; ;; morning-star-theme.el
;; (use-package morning-star-theme
;;   :vc (:url "https://github.com/Alexander-Miller/morning-star-theme"
;;        :rev :newest
;;        :branch "master")
;;   :ensure t)

;; ;; doric-themes
;; (use-package doric-themes
;;   :vc (:url "https://github.com/protesilaos/doric-themes"
;;        :rev :newest
;;        :branch "main")
;;   :ensure t)

;; ;; ef-themes
;; (use-package ef-themes
;;   :vc (:url "https://github.com/protesilaos/ef-themes"
;;        :rev :newest
;;        :branch "main")
;;   :ensure t)

;; nordic-night-theme
;; (use-package nordic-night-theme
;;   :ensure t)

;; temple-os-emacs-theme
;; (use-package temple-os-emacs-theme
;;   :vc (:url "https://github.com/Senka07/temple-os-emacs-theme"
;;        :rev :newest
;;        :branch "main"
;;        :vc-backend Git)
;;   :ensure t
;;   :defer t
;;   )
