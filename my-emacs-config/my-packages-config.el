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
  :defer t
  :after magit)

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

;; Vim mode for Emacs
(use-package evil
  :ensure t
  :defer t)

;; YASnippet for shortcuts
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets"))
  )

;; Focus on selected text
(use-package focus
  :ensure t)

;; Multiline cursors.
(use-package multiple-cursors
  :ensure t
  :defer t)

;; Loading, configuring, and ensuring that org-mode is installed.
(use-package org
  :ensure t
  :defer t
  ;;:config
  ;; (setq org-startup-indented t
  ;;       org-hide-emphasis-markers t
  ;;       org-agenda-files '("~/org/"))
  ;; (org-babel-do-load-languages
  ;;  'org-babel-load-languages
  ;;  '((js . t)))
  )

;; ledger-mode
(use-package ledger-mode
  :ensure t
  :defer t)

;; Syntax checking using Flycheck
(use-package flycheck
  :ensure t
  :hook (ledger-mode . flycheck-mode))

;; flycheck intregration for ledger-mode
(use-package flycheck-ledger
  :ensure t
  :after (flycheck ledger))

;; Automatically show available commands
(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 10.0)
  (which-key-setup-side-window-right-bottom)
  (which-key-mode))

;; Directory-specific environments
(use-package direnv
  :ensure t
  :defer t
  ;; :config
  ;; (direnv-mode)
  )

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

;; evil-mode
;; Set C-z for evil-mode
(global-unset-key (kbd "C-z")) ;; Originally suspend-frame, it also uses C-x C-z
(global-set-key (kbd "C-z") 'evil-mode)

;; YASnippets
;; Remap the snippet expansion from TAB to H-TAB
(define-key yas-minor-mode-map [(tab)] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "H-<tab>") 'yas-expand)

;; Treemacs
;; Increase zoom for all modes except treemacs
(add-hook 'after-change-major-mode-hook
	  (lambda ()
	    (unless (derived-mode-p 'treemacs-mode)
	      (text-scale-set 3))))

;; Focus (elements it can focus: org-element, paragraph, sentence, sexp, symbol, word)
;; (add-to-list 'focus-mode-to-thing '(java-ts-mode . paragraph))

;; multiple-cursors.el
;; Add a cursor to each line of an active region.
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; Add multiple cursors not based on continuous lines, but based on keywords.
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C-<mouse-1>") 'mc/add-cursor-on-click)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
