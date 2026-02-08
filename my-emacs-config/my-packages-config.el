;; Recompile packages if necessary
;; (byte-recompile-directory package-user-dir nil 'force)

;; Check if Emacs is inside a (podman) container
(defun container-p ()
  "Return non-nil if Emacs is inside a (podman) container."
  (let ((exit-code (call-process-shell-command
                    "env | grep container=podman"
                    nil
                    nil
                    nil)))

    (eq exit-code 0)))

;; Check if the operating system is NixOS.
(defun nixos-p ()
  "Return non-nil if the current system is NixOS."
  (when (file-readable-p "/etc/os-release")
    (with-temp-buffer
      (insert-file-contents "/etc/os-release")
      (or (re-search-forward "^ID=nixos$" nil t)
          (re-search-forward "^NAME=NixOS$" nil t)))))

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
        ("gnu-devel"    . 1)
        )
      )

(setq package-selected-packages
      '(apheleia json-mode tomlparse toml-mode toml yaml yaml-mode consult-eglot-embark consult-eglot consult-yasnippet combobulate markdown-mode dape rust-mode dashboard mason nix-ts-mode nix-mode uv-mode smartparens nerd-icons-xref nerd-icons-grep doom-modeline ef-themes doric-themes morning-star-theme zenburn-emacs spacemacs-theme nerd-icons-ibuffer nerd-icons-corfu nerd-icons-completion nerd-icons-dired cider clojure-ts-mode clojure-mode nerd-icons vertico-prescient prescient embark-consult corfu-prescient avy-embark-collect embark marginalia vertico avy vundo auctex pdf-tools consult cape gnu-elpa-keyring-update envrc flymake-hledger hledger-mode ledger-mode orderless corfu focus treesit-fold pgmacs pg forge yasnippet doom-themes magit diff-hl))

;; Dashboard to display projects and bookmarks
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-center-content t)
  (set-face-attribute 'dashboard-items-face nil :height 163 :inherit 'widget-button)
  (setq dashboard-display-icons-p t)     ; display icons on both GUI and terminal
  (setq dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-week-agenda t)
  (setq dashboard-items '(
                          (bookmarks . 30)
                          (projects  . 15)
                          (agenda    . 5)
                          ))
  (dashboard-setup-startup-hook))

;; doom-modeline, a modified version of the modeline
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
  (doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
  ;; (doom-modeline-env-ruby-executable "ruby")
  ;; (doom-modeline-env-perl-executable "perl")
  ;; (doom-modeline-env-go-executable "go")
  ;; (doom-modeline-env-elixir-executable "iex")
  (doom-modeline-env-rust-executable "rustc")
  (doom-modeline-env-load-string "...")
  ;; (doom-modeline-always-visible-segments '(mu4e irc))
  ;; (doom-modeline-before-update-env-hook nil)
  ;; (doom-modeline-after-update-env-hook nil)
  
  :config
  (add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
  (add-to-list 'tab-bar-format 'doom-modeline-tab-bar-format-global 'append)
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
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  ;; Add tracked files to magit-status.
  (magit-add-section-hook
   'magit-status-sections-hook
   'magit-insert-tracked-files
   nil
   'append)
  (setq transient-default-level 4) ;; default: 4
  :commands (magit-status magit-dispatch)
  )

;; Package used to manage git forges (GitHub, GitLab, Codeberg, etc...)
(use-package forge
  :ensure t
  :defer t
  :after magit)

;; YASnippet for shortcuts
(use-package yasnippet
  :ensure t
  :init
  ;; YASnippet global mode
  (yas-global-mode 1)
  ;; Remap the snippet expansion from TAB to H-TAB
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "s-<tab>") 'yas-expand)
  (define-key yas-minor-mode-map (kbd "s-<0x10081247> <tab>") 'yas-expand)
  (define-key yas-minor-mode-map (kbd "s-<0x10081247> S-s-<iso-lefttab>") 'yas-expand)
  :config
  ;; YASnippet directories
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets"))
  )

;; Package to fold code using tree-sitter technology
(use-package treesit-fold
  :ensure t
  :defer t)

(use-package flymake
  :hook
  (((
     java-mode
     java-ts-mode
     emacs-lisp-mode
     nix-mode
     nix-ts-mode
     rust-mode
     rust-ts-mode
     markdown-mode
     js-mode
     js-ts-mode
     html-mode
     html-ts-mode
     css-mode
     css-ts-mode
     json-mode
     json-ts-mode
     python-mode
     python-ts-mode
     elixir-mode
     elixir-ts-mode
     erlang-mode
     erlang-ts-mode
     ) . flymake-mode))
  )

;; Focus on selected text
(use-package focus
  :ensure t
  :defer t
  :config
  ;; Focus (elements it can focus: org-element, paragraph, sentence, sexp, symbol, word)
  (add-to-list 'focus-mode-to-thing '(java-ts-mode . paragraph))
  :commands (focus-mode focus-read-only-mode)
  )

;; avy
(use-package avy
  :ensure t
  :defer t
  :init
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

;; hledger-mode
(use-package hledger-mode
  :ensure t
  :defer t
  )

;; flymake-hledger
(use-package flymake-hledger
  :ensure t
  :defer t
  )

;; Automatically show available commands
;; Already preinstalled in Emacs 30
(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 5.0)
  (which-key-setup-side-window-right-bottom)
  ;; `prefix-help-command' becomes  `which-key-C-h-dispatch'
  (which-key-mode)
  )

;; Directory-specific environments (direnv)
(use-package envrc
  :ensure t
  ;; :hook (after-init . envrc-global-mode)
  ;; :config
  ;; (with-eval-after-load 'envrc
  ;;   (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map))
  )

;; gnu-elpa-keyring-update
(use-package gnu-elpa-keyring-update
  :ensure t)

;; vundo
(use-package vundo
  :ensure t
  :defer t
  :init
  ;; Map the `undo' function onto C-x M-u
  (define-key (current-global-map) (kbd "C-x M-u") 'vundo)
  :commands (vundo)
  )

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
  ;; (set-face-attribute 'show-paren-match nil
  ;;                     :background (face-attribute 'hl-line :background))
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
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
  )

(use-package nerd-icons-corfu
  :ensure t
  :after (corfu)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
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
  ;; (smartparens-global-strict-mode)
  (define-key (current-global-map) (kbd "s-p <backspace>") 'sp-unwrap-sexp)
  )

(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "pandoc")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Corfu + Cape + Vertico + Consult + Marginalia + Orderless + Embark (minad-oantolin stack AKA the corfuverse)
;; There is also TempEl, an alternative to YASnippet developed by minad.
;; prescient.el will be used as the sorting algorithm of this stack.

;; Corfu
(use-package corfu
  :ensure t
  :defer t
  :hook
  (((
     java-mode
     java-ts-mode
     emacs-lisp-mode
     nix-mode
     nix-ts-mode
     rust-mode
     rust-ts-mode
     markdown-mode
     js-mode
     js-ts-mode
     html-mode
     html-ts-mode
     css-mode
     css-ts-mode
     json-mode
     json-ts-mode
     python-mode
     python-ts-mode
     elixir-mode
     elixir-ts-mode
     erlang-mode
     erlang-ts-mode
     ) . corfu-mode))
  :config
  ;; See: minad/corfu#transfer-completion-to-the-minibuffer
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (consult-completion-in-region beg end table pred)))))
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer)
  
  (define-key (current-global-map) (kbd "s-c") 'corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.2)
  (corfu-preselect 'first)
  (corfu-preview-current 'insert)
  ;; If already indented, then try to complete at point
  ;;(setopt tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil) ;; For Emacs 30 and newer
  :bind
  (:map corfu-map
        ("RET" . newline)
        ([return] . newline)
        ("TAB" . corfu-insert)
        ([tab] . corfu-insert)
	("H-<tab>" . yas-expand)
	("H-d" . corfu-popupinfo-mode)
	("H-t" . corfu-popupinfo-toggle)
	("S-<down>" . corfu-popupinfo-scroll-up)
	("S-<up>" . corfu-popupinfo-scroll-down)
        ("M-m" . corfu-move-to-minibuffer)
	)
  )

;; Cape
(use-package cape
  :ensure t
  :bind ("C-c p" . cape-prefix-map) ;; Press C-c p C-h to see a list of keys binded to C-c p
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        etc...)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dict)
  (add-hook 'completion-at-point-functions #'cape-elisp-symbol)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; etc...
  )


;; Vertico
(use-package vertico
  :ensure t
  :custom
  (vertico-scroll-margin 2) ;; Different scroll margin
  (vertico-count 10) ;; Show more candidates
  (vertico-resize 'grow-only) ;; Grow and shrink the Vertico minibuffer. Other values: t, grow-only
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :bind (:map vertico-map
              ("<tab>" . minibuffer-complete)
              ("M-g M-c" . switch-to-completions)
              ("M-<tab>" . vertico-insert)
              ("<backtab>" . vertico-insert)
	      ("C-M-n" . vertico-next-group)
	      ("C-M-p" . vertico-previous-group)
              ("<wheel-down>" . next-line)
              ("<wheel-up>" . previous-line)
              ("<mouse-1>" . vertico-exit)
	      )
  :init
  (vertico-mode)
  )

;; List of vertico extensions found in github@minad/vertico.

;; vertico-buffer
;; vertico-directory
;; vertico-flat
;; vertico-grid
;; vertico-indexed
;; vertico-mouse
;; vertico-multiform
;; vertico-quick
;; vertico-repeat
;; vertico-reverse
;; vertico-sort
;; vertico-suspend
;; vertico-unobtrusive


;; Consult
(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ;;("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e c" . consult-compile-error)
         ("M-g e g" . consult-eglot-symbols)
         ("M-g r" . consult-grep-match)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g M-o" . consult-org-heading)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-g y y" . consult-yasnippet)
         ("M-g y v" . consult-yasnippet-visit-snippet-file)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s M-d" . consult-fd)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history)                 ;; orig. previous-matching-history-element
         )

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "s-.")
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

(use-package consult-eglot
  :ensure t
  :defer t
  :after (consult eglot)
  )

(use-package consult-eglot-embark
  :ensure t
  :defer t
  :after (consult eglot embark)
  )

(use-package consult-yasnippet
  :ensure t
  :after (consult yasnippet)
  )

;; Marginalia
(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :custom
  (marginalia-max-relative-age 1209600) ;; Can be set to 1209600 or 0
  (marginalia-align 'center) ;; Can be set right, left, or center
  :init
  (marginalia-mode)
  )

;; Embark
(use-package embark
  :ensure t
  :defer t
  :bind
  (("s-m ." . embark-act)
   ("s-m ," . embark-dwim)
   ("s-m b" . embark-bindings)
   ("s-m e" . embark-export)
   )
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; If which-key-mode is enabled before embark, by default `which-key--prefix-help-cmd-backup'
  ;; will have the value `describe-prefix-bindings'.
  ;; This code replaces whatever value `which-key--prefix-help-cmd-backup' had with `embark-prefix-help-command'.
  ;; It also replaces `prefix-help-command' with `which-key-C-h-dispatch'.
  (when which-key-mode
    (setq which-key--prefix-help-cmd-backup #'embark-prefix-help-command)
    (setq prefix-help-command #'which-key-C-h-dispatch)
    )

  ;; Show the Embark target at point via Eldoc
  ;;(add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;;(setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  ;; Change the value of prefix-help-command. Default: describe-prefix-bindings
  (defun change-between-describe-prefix-and-embark-prefix ()
    "Change the value of the variable `prefix-help-command' into either `describe-prefix-bindings' or `embark-prefix-help-command'."
    (interactive)
    (if which-key-mode
        (if (equal which-key--prefix-help-cmd-backup 'describe-prefix-bindings)
            (progn
              (setq which-key--prefix-help-cmd-backup #'embark-prefix-help-command)
	      (message "Changed `which-key--prefix-help-cmd-backup' into `embark-prefix-help-command'"))
          (progn
            (setq which-key--prefix-help-cmd-backup #'describe-prefix-bindings)
	    (message "Changed `which-key--prefix-help-cmd-backup' into `describe-prefix-bindings'")))
      (if (equal prefix-help-command 'describe-prefix-bindings)
          (progn
            (setq prefix-help-command #'embark-prefix-help-command)
            (message "Changed `prefix-help-command' into `embark-prefix-help-command'"))
        (progn
          (setq prefix-help-command #'describe-prefix-bindings)
          (message "Changed `prefix-help-command' into `describe-prefix-bindings'")))))

  (global-set-key (kbd "s-m c") 'change-between-describe-prefix-and-embark-prefix)
  )

(use-package embark-consult
  :ensure t
  :defer t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  )

(use-package avy-embark-collect
  :ensure t
  :defer t
  :after (embark avy)
  )

;; Orderless
(use-package orderless
  :ensure t
  ;;:after vertico
  :custom
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic)) ;;partial-completion
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)) ;;basic
                                   (eglot (styles orderless))
                                   (eglot-capf (styles orderless))))
  ;;(completion-pcm-leading-wildcard t) ;; Emacs 31: partial-completion behaves like substring
  )

;; Prescient
(use-package prescient
  :ensure t
  :custom
  (prescient-aggressive-file-save nil) ;; default: nil
  (prescient-sort-length-enable nil) ;; default: t
  (prescient-sort-full-matches-first t) ;; default: nil
  (prescient-history-length 300) ;; default: 100
  (prescient-frequency-decay 0.997) ;; default: 0.997
  (prescient-frequency-threshold 0.05) ;; default: 0.05
  (prescient-save-file (file-truename "~/.emacs.d/prescient/prescient-save.el"))
  :config
  (prescient-persist-mode 1)
  )

(use-package corfu-prescient
  :ensure t
  :demand t
  :after (corfu prescient)
  :custom
  ;; Sorting.
  (corfu-prescient-enable-sorting t) ;; default: t
  ;; Don't override `display-sort-function'
  (corfu-prescient-override-sorting nil) ;; default: nil

  ;; Filtering
  ;; Telling corfu-prescient not to use prescient to do the filtering
  (corfu-prescient-enable-filtering nil) ;; default: t
  ;; See also `corfu-prescient-completion-styles',
  ;; `corfu-prescient-completion-category-overrides' and
  ;; `prescient--completion-recommended-overrides'.  Those options apply only
  ;; when `corfu-prescient-enable-filtering' is non-nil.
  :config
  (corfu-prescient-mode 1)
  )

(use-package vertico-prescient
  :ensure t
  :demand t
  :after vertico prescient
  :custom
  ;; Sorting.
  (vertico-prescient-enable-sorting t) ;; default: t
  ;; Don't override `display-sort-function'
  (vertico-prescient-override-sorting nil) ;; default: nil

  ;; Filtering
  ;; Telling vertico-prescient not to use prescient to do the filtering
  (vertico-prescient-enable-filtering nil) ;; default: t
  ;; See also `vertico-prescient-completion-styles',
  ;; `vertico-prescient-completion-category-overrides', and
  ;; `prescient--completion-recommended-overrides'.  Those options apply only
  ;; when when `vertico-prescient-enable-filtering' is non-nil.
  :config
  (vertico-prescient-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Treesitter

(use-package treesit
  :ensure nil
  :config
  ;; Treesitter grammar repositories.
  (setq treesit-language-source-alist
        '(
	  (java . ("https://github.com/tree-sitter/tree-sitter-java"))
	  (clojure . ("https://github.com/sogaiu/tree-sitter-clojure"))
	  (css . ("https://github.com/tree-sitter/tree-sitter-css"))
	  (html . ("https://github.com/tree-sitter/tree-sitter-html"))
	  (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
	  (json . ("https://github.com/tree-sitter/tree-sitter-json"))
	  (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
	  (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
	  (nix . ("https://github.com/nix-community/tree-sitter-nix" "master"))
	  (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "master"))
	  (python . ("https://github.com/tree-sitter/tree-sitter-python" "master"))
          (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "master"))
          (markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
          (markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src"))
          (regex . ("https://github.com/tree-sitter/tree-sitter-regex" "master"))
          (c . ("https://github.com/tree-sitter/tree-sitter-c" "master"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp" "master"))
          (erlang . ("https://github.com/WhatsApp/tree-sitter-erlang" "main"))
          (elixir . ("https://github.com/elixir-lang/tree-sitter-elixir" "main"))
          (toml . ("https://github.com/tree-sitter-grammars/tree-sitter-toml"))
          (heex . ("https://github.com/phoenixframework/tree-sitter-heex"))
	  )
        )
  ;; To install all the grammars at once use this: (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
  (dolist (lang treesit-language-source-alist)
    (unless (treesit-language-available-p (car lang))
      (treesit-install-language-grammar (car lang))))

  ;; Replace normal mode with its equivalent treesitter mode (ts-mode).
  (setq major-mode-remap-alist
        '(
	  (java-mode . java-ts-mode)
	  (clojure-mode . clojure-ts-mode)
	  (css-mode . css-ts-mode)
	  (html-mode . html-ts-mode)
	  (js-mode . js-ts-mode)
          (javascript-mode . js-ts-mode)
          (js2-mode . js-ts-mode)
	  (js-json-mode . json-ts-mode)
	  (json-mode . json-ts-mode)
	  (typescript-tsx-mode . tsx-ts-mode)
	  (typescript-mode . typescript-ts-mode)
	  (nix-mode . nix-ts-mode)
          (rust-mode . rust-ts-mode)
          (python-mode . python-ts-mode)
          (toml-mode . toml-ts-mode)
          (elixir-mode . elixir-ts-mode)
          (erlang-mode . erlang-ts-mode)
	  )
        )

  ;; Initialize major modes on these file extensions
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . html-ts-mode))

  ;; 16.13.2 Parser-based Font Lock
  ;; Adds everything else that can be fontified: operators, delimiters, brackets, other punctuation, function names in function calls, property look ups, variables, etc.
  (setopt treesit-font-lock-level 4)
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

;; Dabbrev config
(use-package dabbrev
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)
         ("C-M-<Ungrab>" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; When using NixOS, set up the appropriate environment.
(when (nixos-p)
  (use-package nix-mode
    :ensure t)
  
  (use-package nix-ts-mode
    :ensure t
    :hook
    (nix-ts-mode . eglot-ensure))
  
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'eglot-format nil t)))

  (with-eval-after-load 'eglot
    (dolist (mode '((nix-mode . ("nixd"))))
      (add-to-list 'eglot-server-programs mode))))
