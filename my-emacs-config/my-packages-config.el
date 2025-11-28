;; Recompile packages if necessary
;; (byte-recompile-directory package-user-dir nil 'force)

;; Priority for installation
(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
        ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
        ("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu-devel"    . "https://elpa.gnu.org/devel/"))
      package-archive-priorities
      '(("gnu"          . 5)
        ("nongnu"       . 4)
        ("melpa"        . 3)
        ("melpa-stable" . 2)
        ("gnu-devel"    . 1)))

(setq package-selected-packages
      '(smartparens nerd-icons-xref nerd-icons-grep doom-modeline ef-themes doric-themes morning-star-theme zenburn-emacs spacemacs-theme nerd-icons-ibuffer nerd-icons-completion nerd-icons-dired nerd-icons avy gnu-elpa-keyring-update direnv ledger-mode focus treemacs-tab-bar treemacs-magit forge yasnippet treemacs doom-themes magit diff-hl))

;; Load theme(s)
(load-file "~/.emacs.d/my-emacs-config/my-themes-config.el")

;; Use doom-modeline, a modified version of the modeline
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  ;; :hook (after-init . doom-modeline-mode)
  :custom ;; Does this package have more options than treemacs?
  (doom-modeline-support-imenu t)
  (doom-modeline-height 27)
  (doom-modeline-bar-width 1)
  (doom-modeline-hud nil)
  (doom-modeline-window-width-limit 85)
  (doom-modeline-spc-face-overrides nil)
  (doom-modeline-project-detection 'project)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-lsp-icon t)
  (doom-modeline-time-icon t)
  (doom-modeline-time-live-icon t)
  (doom-modeline-time-analogue-clock t)
  (doom-modeline-time-clock-size 0.7)
  (doom-modeline-unicode-fallback nil)
  (doom-modeline-buffer-name t)
  (doom-modeline-highlight-modified-buffer-name t)
  (doom-modeline-column-zero-based t)
  (doom-modeline-percent-position '(-3 "%p"))
  (doom-modeline-position-line-format '("L%l"))
  (doom-modeline-position-column-format '("C%c"))
  (doom-modeline-position-column-line-format '("%l:%c"))
  (doom-modeline-minor-modes t)
  (doom-modeline-selection-info nil)
  ;; (doom-modeline-enable-word-count t)
  ;; (doom-modeline-continuous-word-count-modes '(text-mode markdown-mode gfm-mode org-mode))
  (doom-modeline-continuous-word-count-modes nil)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-indent-info nil)
  ;; (doom-modeline-total-line-number t)
  (doom-modeline-vcs-icon t)
  (doom-modeline-vcs-max-length 15)
  (doom-modeline-vcs-display-function #'doom-modeline-vcs-name)
  (doom-modeline-vcs-state-faces-alist
      '((needs-update . (doom-modeline-warning bold))
        (removed . (doom-modeline-urgent bold))
        (conflict . (doom-modeline-urgent bold))
        (unregistered . (doom-modeline-urgent bold))))
  (doom-modeline-check-icon t)
  (doom-modeline-check-simple-format nil)
  (doom-modeline-number-limit 99)
  (doom-modeline-project-name t)
  (doom-modeline-workspace-name t)
  (doom-modeline-persp-name t)
  (doom-modeline-display-default-persp-name nil)
  (doom-modeline-persp-icon t)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-github-interval (* 30 60))
  (doom-modeline-modal t)
  (doom-modeline-modal-icon t)
  (doom-modeline-modal-modern-icon t)
  (doom-modeline-always-show-macro-register nil)
  (doom-modeline-mu4e nil)
  ;; (mu4e-alert-enable-mode-line-display)
  (doom-modeline-gnus t)
  (doom-modeline-gnus-timer 2)
  ;; (doom-modeline-gnus-excluded-groups '("dummy.group"))
  (doom-modeline-irc t)
  ;; (doom-modeline-irc-stylize #'doom-modeline-shorten-irc) ;;'identity
  (doom-modeline-battery t)
  (doom-modeline-time t)
  (doom-modeline-display-misc-in-all-mode-lines t)
  (doom-modeline-buffer-file-name-function #'identity)
  (doom-modeline-buffer-file-truename-function #'identity)
  (doom-modeline-env-version t)
  (doom-modeline-env-enable-python t)
  (doom-modeline-env-enable-ruby t)
  (doom-modeline-env-enable-perl t)
  (doom-modeline-env-enable-go t)
  (doom-modeline-env-enable-elixir t)
  (doom-modeline-env-enable-rust t)
  ;; (doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
  ;; (doom-modeline-env-ruby-executable "ruby")
  ;; (doom-modeline-env-perl-executable "perl")
  ;; (doom-modeline-env-go-executable "go")
  ;; (doom-modeline-env-elixir-executable "iex")
  ;; (doom-modeline-env-rust-executable "rustc")
  (doom-modeline-env-load-string "...")
  ;; (doom-modeline-always-visible-segments '(mu4e irc))
  ;; (doom-modeline-before-update-env-hook nil)
  ;; (doom-modeline-after-update-env-hook nil)
  )

;; Used to highlight lines changed
(use-package diff-hl
  :ensure t
  :config
  ;; Ensure VC is enabled
  ;;(setq vc-handled-backends '(RCS CVS SVN SCCS SRC Bzr Git Hg))
  ;; Activate diff-hl in all buffers.
  (global-diff-hl-mode)
  )

;; Package used to manage git
(use-package magit
  :ensure t
  :defer t
  :config
  ;; Integrate diff-hl with magit.
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  ;; Add tracked files to magit-status.
  (magit-add-section-hook
   'magit-status-sections-hook
   'magit-insert-tracked-files
   nil
   'append)
  :commands (magit-status magit-dispatch)
  )

;; Package used to manage git forges (GitHub, GitLab, Codeberg, etc...)
(use-package forge
  :ensure t
  :defer t
  :after magit)

;; Project manager
;; (use-package projectile
;;   :ensure t
;;   :init
;;   (setq projectile-project-search-path '("~/projects/" "~/work/" "~/playground"))
;;   :config
;;   (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
;;   (global-set-key (kbd "C-c p") 'projectile-command-map)
;;   (projectile-mode +1)
;;   )

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
          treemacs-space-between-root-nodes        nil ;;default: t
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
    ;;(treemacs-project-follow-mode t)
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
        ("H-x t o"   . treemacs-select-window)
        ("H-x t 1"   . treemacs-delete-other-windows)
        ("H-x t t"   . treemacs)
        ("H-x t d"   . treemacs-select-directory)
        ("H-x t B"   . treemacs-bookmark)
        ("H-x t C-t" . treemacs-find-file)
        ("H-x t M-t" . treemacs-find-tag))
  
  ) ;; Yes, this is the end of the use-package treemacs.

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

;; (treemacs-start-on-boot)

;; YASnippet for shortcuts
(use-package yasnippet
  :ensure t
  :init
  ;; YASnippet global mode
  (yas-global-mode 1)
  ;; Remap the snippet expansion from TAB to H-TAB
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "H-<tab>") 'yas-expand)
  :config
  ;; YASnippet directories
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets"))
  )

;; Focus on selected text
(use-package focus
  :ensure t
  :defer t
  :config
  ;; Focus (elements it can focus: org-element, paragraph, sentence, sexp, symbol, word)
  ;; (add-to-list 'focus-mode-to-thing '(java-ts-mode . paragraph))
  :commands (focus-mode focus-read-only-mode)
  )

;; avy
(use-package avy
  :ensure t
  :defer t
  :config
  ;; Mapping keys for avy
  (global-set-key (kbd "C-: c 1") 'avy-goto-char)
  (global-set-key (kbd "C-: c 2") 'avy-goto-char-2)
  (global-set-key (kbd "C-: w 1") 'avy-goto-word-0)
  (global-set-key (kbd "C-: w 2") 'avy-goto-word-1)
  :commands (avy-goto-char avy-goto-char-0 avy-goto-word-1 avy-goto-word-2)
  )

;; ledger-mode
(use-package ledger-mode
  :ensure t
  :defer t
  :config
  (add-hook 'ledger-mode-hook #'ledger-flymake-enable)
  )

;; Automatically show available commands
;; Already preinstalled in Emacs 30
(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 5.0)
  (which-key-setup-side-window-right-bottom)
  ;;(which-key-mode)
  )

;; Directory-specific environments
(use-package direnv
  :ensure t
  :defer t
  ;; :config
  ;; (direnv-mode)
  )

;; gnu-elpa-keyring-update
(use-package gnu-elpa-keyring-update
  :ensure t)

;; Highlight cursor line
(use-package hl-line
  :ensure nil
  :config
  ;; Gray, with disabled underline and overline
  ;;(set-face-background 'hl-line "#303030")
  ;;(custom-set-faces '(hl-line ((t (:background "#303030" :underline nil :overline nil)))))
  (set-face-attribute 'hl-line nil
                      :background "#404040"
		      :underline nil
                      :overline nil)
  (global-hl-line-mode t)
  :hook ((eshell-mode
          ;;eat-mode
          shell-mode
          term-mode
	  ;;vterm-mode
          comint-mode
          cfrs-input-mode
          image-mode
	  magit-diff-mode)
         ;; Disable hl-line for some modes
         . (lambda () (setq-local global-hl-line-mode nil))))

;; Project manager
(use-package project
  ;; Use demand to load the package automatically
  :demand t
  :config
  ;; Show project name on mode-line
  (setq project-mode-line t)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Nerd Icons

;; nerd-icons
(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  ;; (nerd-icons-install-fonts) ;; To install the Symbols Nerd Font
  )

(use-package nerd-icons-dired
  :ensure t
  :after (dired nerd-icons)
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :ensure t
  ;;:after marginalia
  :config
  (nerd-icons-completion-mode)
  ;;(add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
  )

(use-package nerd-icons-ibuffer
  :ensure t
  :after (ibuffer)
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-grep
  :ensure t
  :init
  (nerd-icons-grep-mode)
  :custom
  ;; This setting is a pre-requirement, so an icon can be displayed near each
  ;; heading
  (grep-use-headings t)
  )

(use-package nerd-icons-xref
  :ensure t
  :init
  (nerd-icons-xref-mode)
  )

(use-package smartparens
  :ensure t
  :hook (prog-mode text-mode markdown-mode org-mode) ;; Add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; Org configuration

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
