;; doom-themes
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dark+ t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; default "doom-atom"; use "doom-colors" for less minimal icon theme
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  ;; Tab Bars
  (load "tab-bar")

  (set-face-attribute 'tab-bar-tab nil
		      :background "#68217A"
		      :distant-foreground "#D4D4D4"
		      :foreground "#D4D4D4"
		      :box '(:line-width (3 . 3) :color "black" :style flat-button)
		      :weight 'heavy
		      :height 130)

  (set-face-attribute 'tab-bar-tab-inactive nil
		      :background "#252526"
		      :foreground "#AEAFAD"
		      :height 130
		      :box '(:line-width (3 . 3) :color "black" :style flat-button))

  ;; Tab Lines
  (load "tab-line")

  (set-face-attribute 'tab-line-tab nil
		      :background "#252526"
		      :foreground "#AEAFAD"
		      :box '(:line-width (3 . 3) :color "black" :style flat-button)
		      :height 110)

  (set-face-attribute 'tab-line-tab-current nil
		      :background "#68217A"
		      :distant-foreground "#D4D4D4"
		      :foreground "#D4D4D4"
		      :weight 'heavy
		      :box '(:line-width (3 . 3) :color "black" :style flat-button)
		      :height 110)

  (set-face-attribute 'tab-line-tab-inactive nil
		      :background "#252526"
		      :foreground "#AEAFAD"
		      :box '(:line-width (3 . 3) :color "black" :style flat-button)
		      :height 110)

  (set-face-attribute 'tab-line-tab-modified nil
		      :foreground "#E54568"
		      :background "#130034")
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
