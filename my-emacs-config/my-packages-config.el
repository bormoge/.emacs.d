;; Recompile packages if necessary
;; (byte-recompile-directory package-user-dir nil 'force)

;; Priority for installation
(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
	("nongnu"       . "https://elpa.nongnu.org/nongnu/")
        ("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities
      '(("gnu"          . 4)
        ("nongnu"       . 3)
        ("melpa"        . 2)
        ("melpa-stable" . 1)))

;; Doom themes
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dark+ t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
;;  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
;;  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; Used to highlight lines changed
(use-package diff-hl
  :ensure t)

;; Package used to manage git
(use-package magit
  :ensure t)

(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
;;  :hook ((emacs-lisp-mode . lsp)) ;; replace XXX-mode with concrete major-mode(e. g. python-mode)
  :commands lsp)

;; Packages configuration

;; Package configs; maybe I'm putting it in another file.
(global-diff-hl-mode)

;; To integrate diff-hl with magit
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
