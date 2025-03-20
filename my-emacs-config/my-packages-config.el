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

;; Allows linting, formatting, auto-completion, semantic editing, among other features.
(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
;;  :hook ((emacs-lisp-mode . lsp)) ;; replace XXX-mode with concrete major-mode(e. g. python-mode)
  :commands lsp)

;; lsp-ui to show higher abtraction interfaces for lsp-mode
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; Dap mode for debugging
(use-package dap-mode
  :ensure t)

;; Minimap
(use-package minimap
  :ensure t)

;; YASnippet for shortcuts
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets"))
  )

;; Corfu + Cape + Vertico + Consult + Marginalia + Orderless + Embark (minad-oantolin stack AKA the corfuverse)
;; There is also TempEl, an alternative to YASnippet developed by minad.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Packages configuration

;; Package configs; maybe I'm putting it in another file.
(global-diff-hl-mode)

;; To integrate diff-hl with magit
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; Add tracked files to magit-status
(magit-add-section-hook
   'magit-status-sections-hook
   'magit-insert-tracked-files
   nil
   'append)

;; Treesitter grammar repositories
(setq treesit-language-source-alist
      '((java "https://github.com/tree-sitter/tree-sitter-java")))

;; Replace normal mode with its equivalent treesitter mode (ts-mode)
(setq major-mode-remap-alist
      '((java-mode . java-ts-mode)))

;; lsp-idle-delay determines how often lsp-mode will refresh.
(setq lsp-idle-delay 0.500)

;; Minimap config. By default the package is globally activated, but it only works on modes derived from 'prog-mode'.
(minimap-mode 1)
(setq minimap-window-location 'right)
(setq minimap-width-fraction '0.10)
