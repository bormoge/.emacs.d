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
  ;; default "doom-atom"; use "doom-colors" for less minimal icon theme
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; Used to highlight lines changed
(use-package diff-hl
  :ensure t)

;; Package used to manage git
(use-package magit
  :ensure t)

;; Package used to manage git forges (GitHub, GitLab, Codeberg, etc...)
(use-package forge
  :ensure t
  :after magit)

;; Allows linting, formatting, auto-completion, semantic editing, etc.
(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
;;  :hook ((XXX-mode . lsp)) ;; replace XXX-mode with concrete major-mode(e. g. python-mode)
  :commands lsp)

;; lsp-ui to show higher abtraction interfaces for lsp-mode
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; Dap mode for debugging
(use-package dap-mode
  :ensure t)

;; Project manager
(use-package projectile
  :ensure t
  :init
  (setq projectile-project-search-path '("~/projects/" "~/work/" "~/playground"))
  :config
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
  (global-set-key (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  )

;; File and project explorer
;; Most of this config is taken from Alexander-Miller's repository
(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; Note from treemacs author Alexander-Miller:
    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

;;    (treemacs-hide-gitignored-files-mode nil)
    )
  :bind
  (:map global-map
        ("H-0"       . treemacs-select-window)
        ("H-x t 1"   . treemacs-delete-other-windows)
        ("H-x t t"   . treemacs)
        ("H-x t d"   . treemacs-select-directory)
        ("H-x t B"   . treemacs-bookmark)
        ("H-x t C-t" . treemacs-find-file)
        ("H-x t M-t" . treemacs-find-tag))
  
  ) ;; Yes, this is the end of the use-package treemacs.

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package all-the-icons ;; IMPORTANT: to see the icons you need to install them using all-the-icons-install-fonts
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package treemacs-icons-dired
  :after (all-the-icons-dired)
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

;; (treemacs-start-on-boot)

;; Treemacs integration with LSP
(use-package lsp-treemacs
  :ensure t
  :after lsp)

;; Vim mode for Emacs
(use-package evil
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

;; Packages to manage PostgreSQL
;; To upgrade use package-vc-upgrade
(unless (package-installed-p 'pg)
  (package-vc-install "https://github.com/emarsden/pg-el" nil nil 'pg))
(unless (package-installed-p 'pgmacs)
  (package-vc-install "https://github.com/emarsden/pgmacs" nil nil 'pgmacs))

(require 'pg)
(require 'pgmacs)

;; Package to fold code
;; To upgrade use package-vc-upgrade
(unless (package-installed-p 'treesit-fold)
  (package-vc-install "https://github.com/emacs-tree-sitter/treesit-fold" nil nil 'treesit-fold))

(require 'treesit-fold)

;; Syntax checking using Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; Focus on selected text
(use-package focus
  :ensure t)

(use-package lsp-focus
  :ensure t)

;; ledger-mode
;; Note to self: it has a flycheck package

;; TBW

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Corfu + Cape + Vertico + Consult + Marginalia + Orderless + Embark (minad-oantolin stack AKA the corfuverse)
;; There is also TempEl, an alternative to YASnippet developed by minad.

;; TBW


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; Packages configuration

;; diff-hl
;; Activate in all buffers diff-hl.
(global-diff-hl-mode)

;; To integrate diff-hl with magit.
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; Magit
;; Add tracked files to magit-status.
(magit-add-section-hook
   'magit-status-sections-hook
   'magit-insert-tracked-files
   nil
   'append)

;; Minimap
;; Minimap config. By default the package is globally activated, but it only works on modes derived from 'prog-mode'.
(minimap-mode 1)
(setq minimap-window-location 'right)
(setq minimap-width-fraction '0.10)

;; evil-mode
;; Set C-z for evil-mode
(global-unset-key (kbd "C-z")) ;; Originally suspend-frame, it also uses C-x C-z
(global-set-key (kbd "C-z") 'evil-mode)

;; YASnippets
;; Remap the snippet expansion from TAB to M-+
(define-key yas-minor-mode-map [(tab)] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "M-+") 'yas-expand)

;; Treemacs
;; Increase zoom for all modes except treemacs
(add-hook 'after-change-major-mode-hook
	  (lambda ()
	    (unless (derived-mode-p 'treemacs-mode)
	      (text-scale-set 3))))

;; Focus (elements it can focus: org-element, paragraph, sentence, sexp, symbol, word)
(add-to-list 'focus-mode-to-thing '(java-ts-mode . paragraph))
(add-hook 'focus-mode-hook #'lsp-focus-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lsp clients: To Be Installed
;; lsp-java
;; lsp-python
;; lsp-javascript / lsp-typescript


;; (add-hook 'java-mode-hook 'lsp)
;; (add-hook 'java-ts-mode-hook 'lsp)

;; (add-to-list 'lsp-client-packages 'lsp-XXX)

;; lsp-idle-delay determines how often lsp-mode will refresh.
(setq lsp-idle-delay 0.500)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Treesitter
;; Treesitter grammar repositories.
(setq treesit-language-source-alist
      '((java "https://github.com/tree-sitter/tree-sitter-java")))

;; Replace normal mode with its equivalent treesitter mode (ts-mode).
(setq major-mode-remap-alist
      '((java-mode . java-ts-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Dap-mode
;;(require 'dap-java)
;; (dap-register-debug-template "Java Runner"
;;                              (list :type "java"
;;                                    :request "launch"
;;                                    :args ""
;;                                    :vmArgs "-ea -Dmyapp.instance.name=myapp_1"
;;                                    :projectName "myapp"
;;                                    :mainClass "com.domain.AppRunner"
;;                                    :env '(("DEV" . "1"))))
