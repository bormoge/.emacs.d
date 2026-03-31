;; Recompile packages if necessary
;; (byte-recompile-directory package-user-dir nil 'force)

;; Check if Emacs is inside a (podman) container
(defun podman-container-p ()
  "Return non-nil if Emacs is inside a (podman) container."
  (let ((exit-code
         (call-process-shell-command
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

;; Dashboard to display projects and bookmarks
(use-package dashboard
  :ensure t
  :custom
  (dashboard-center-content t)
  (dashboard-display-icons-p t) ;; display icons on both GUI and terminal
  (dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package
  (dashboard-items '(
                     (agenda    . 30)
                     (bookmarks . 30)
                     (projects  . 20)
                     (recents   . 20)
                     (registers . 10)
                     ))
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-week-agenda t)
  :config
  ;; (set-face-attribute 'dashboard-items-face nil :height 163 :inherit 'widget-button)
  (dashboard-setup-startup-hook)
  )

;; doom-modeline, a modified version of the modeline
(use-package doom-modeline
  :ensure t
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
  (set-fontset-font t 'unicode "Symbols Nerd Font Mono" nil 'append)
  (add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
  (add-to-list 'tab-bar-format 'doom-modeline-tab-bar-format-global 'append)
  ;; (add-to-list 'tab-bar-format 'tab-bar-format-global 'append)

  ;; (advice-add 'doom-modeline-update-battery-status
  ;;             :after #'(lambda (&rest _)
  ;;                        (setq battery-mode-line-string (substring
  ;;                                                        (concat
  ;;                                                         (substring-no-properties (cdr doom-modeline--battery-status)))
  ;;                                                        0 -1))))

  (doom-modeline-mode +1)
  )

;; Used to highlight lines changed
(use-package diff-hl
  :ensure t
  ;; :init
  ;; (global-diff-hl-mode +1)
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :custom
  (diff-hl-disable-on-remote nil)
  (diff-hl-update-async nil)
  (diff-hl-show-staged-changes t)
  (diff-hl-draw-borders t)
  ;; Ensure VC is enabled
  ;;(vc-handled-backends '(RCS CVS SVN SCCS SRC Bzr Git Hg))
  )

;; Package used to manage git
(use-package magit
  :ensure t
  :defer t
  :custom
  (magit-diff-refine-hunk nil)
  (git-commit-major-mode 'text-mode) ;; 'git-commit-elisp-text-mode
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
  :custom
  (yas-verbosity 4)
  :config
  ;; YASnippet directories
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets"))
  (yas-reload-all)
  )

;; Package to fold code using tree-sitter technology
(use-package treesit-fold
  :ensure t
  :defer t)

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
  :init
  ;; Mapping keys for avy
  (global-set-key (kbd "C-: c 1") 'avy-goto-char)
  (global-set-key (kbd "C-: c 2") 'avy-goto-char-2)
  (global-set-key (kbd "C-: w 1") 'avy-goto-word-0)
  (global-set-key (kbd "C-: w 2") 'avy-goto-word-1)
  (global-set-key (kbd "C-: w 2") 'avy-goto-word-1)
  (global-set-key (kbd "C-: e 1") 'avy-embark-collect-choose)
  (global-set-key (kbd "C-: e 2") 'avy-embark-collect-act)
  :custom
  (avy-case-fold-search nil)
  (avy-all-windows nil)
  (avy-all-windows-alt t)
  :commands (avy-goto-char avy-goto-char-0 avy-goto-word-1 avy-goto-word-2)
  )

;; ledger-mode
(use-package ledger-mode
  :ensure t
  :defer t
  :config
  (add-hook 'ledger-mode-hook #'ledger-flymake-enable)
  :if (executable-find "ledger")
  )

;; hledger-mode
(use-package hledger-mode
  :ensure t
  :defer t
  :if (executable-find "hledger")
  )

;; flymake-hledger
(use-package flymake-hledger
  :ensure t
  :defer t
  :after (flymake hledger)
  )

;; Directory-specific environments (direnv)
(use-package envrc
  :ensure t
  :bind (:map global-map
              ("C-c e" . envrc-command-map)
              )
  :hook (after-init . envrc-global-mode)
  ;; :config
  ;; (with-eval-after-load 'envrc
  ;;   (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map))
  )

;; gnu-elpa-keyring-update
(use-package gnu-elpa-keyring-update
  :ensure t
  ;; ref: https://elpa.gnu.org/packages/gnu-elpa-keyring-update.html
  ;; ref: https://emacs.stackexchange.com/a/53142
  ;;
  ;; (setq package-check-signature--old package-check-signature
  ;;       package-check-signature nil)
  ;;
  ;; (setq package-check-signature package-check-signature--old)
  )

;; Client for `undo'
(use-package vundo
  :ensure t
  :defer t
  :init
  ;; Map the `undo' function onto C-x M-u
  (define-key (current-global-map) (kbd "C-x M-u") 'vundo)
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  :commands (vundo)
  )

;; Mode for markdown
(use-package markdown-mode
  :ensure t
  :defer t
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . gfm-mode)
   ("\\.markdown\\'" . gfm-mode))
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do))
  :init (setq markdown-command "pandoc")
  ;; :custom
  ;; (markdown-enable-wiki-links t)
  ;; (markdown-italic-underscore t)
  ;; (markdown-asymmetric-header t)
  ;; (markdown-make-gfm-checkboxes-buttons t)
  ;; (markdown-gfm-uppercase-checkbox t)
  ;; (markdown-fontify-code-blocks-natively t)
  ;; (markdown-gfm-additional-languages "Mermaid")
  ;; :config
  ;; (add-to-list 'markdown-code-lang-modes '("mermaid" . mermaid-mode))
  :if (executable-find "pandoc")
  )

;; https://devdocs.io/
(use-package devdocs
  :defer t
  :ensure t
  :bind (:map global-map
              ("s-d d" . devdocs-lookup)
              ("s-d i" . devdocs-install)
              ("s-d l" . devdocs-delete)
              ("s-d a" . devdocs-update-all)
              ("s-d s" . devdocs-search)
              ("s-d p" . devdocs-peruse)
              )
  :custom
  (devdocs-data-dir (expand-file-name ".cache/devdocs" user-emacs-directory))
  )

;; When using NixOS, install nix-mode and nix-ts-mode
(when (nixos-p)
  (use-package nix-mode
    :ensure t)

  (use-package nix-ts-mode
    :ensure t))

;; Emacs headerline for projects
(use-package breadcrumb
  :ensure t
  :init
  (breadcrumb-mode)
  )

(use-package helpful
  :ensure t
  :defer
  :bind (:map global-map
              ("C-h s-f" . helpful-callable)
              ("C-h s-v" . helpful-variable)
              ("C-h s-k" . helpful-key)
              ("C-h s-x" . helpful-command)
              ("C-h s-d" . helpful-at-point)
              ("C-h s-F" . helpful-function)
              )
  :config
  (require 'shortdoc)
  :commands (helpful-callable helpful-variable helpful-key helpful-command helpful-at-point helpful-function)
  )

(use-package puni
  :ensure t
  :defer t
  :bind (:map puni-mode-map
              ("s-<backspace>" . puni-splice)
              )
  :hook ((prog-mode
          sgml-mode
          nxml-mode
          tex-mode
          eval-expression-minibuffer-setup
          text-mode
          org-mode) . puni-mode)
  )

(use-package lin
  :ensure t
  :config
  (lin-global-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; elfeed

(with-temp-buffer
  (insert-file-contents (expand-file-name "rss.txt" user-emacs-directory))
  (eval-buffer))

(use-package elfeed
  :ensure t
  :custom
  (elfeed-feeds rss-links)
  (elfeed-db-directory "~/.elfeed")
  ;; (elfeed-search-filter )
  (elfeed-search-filter "+unread") ;; default: "@6-months-ago +unread"
  (elfeed-show-entry-switch #'switch-to-buffer);; #'pop-to-buffer
  :config
  (setf url-queue-timeout 30)
  (define-key (current-global-map) (kbd "s-<0x10081247> s-E e") 'elfeed)
  (define-key (current-global-map) (kbd "s-<0x10081247> s-E u") 'elfeed-update)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Nerd Icons

;; nerd-icons
(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  ;; (nerd-icons-install-fonts) ;; To install the Symbols Nerd Font
  :config
  ;; (advice-add 'set-frame-font :after
  ;;             (lambda (&rest _) (set-fontset-font t 'unicode "Symbols Nerd Font Mono" nil 'append)))
  (add-hook 'after-setting-font-hook (lambda ()
                                       (set-fontset-font t 'unicode "Symbols Nerd Font Mono" nil 'append)))
  )

(use-package nerd-icons-dired
  :ensure t
  :after (dired nerd-icons)
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode)
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
  )

(use-package nerd-icons-xref
  :ensure t
  :init
  (nerd-icons-xref-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Corfu + Cape + Vertico + Consult + Marginalia + Orderless + Embark (minad-oantolin stack AKA the corfuverse)
;; There is also TempEl, an alternative to YASnippet developed by minad.

;; Corfu
(use-package corfu
  :ensure t
  :defer t
  :hook
  (((
     prog-mode
     eglot--managed-mode
     ;; java-mode
     ;; java-ts-mode
     ;; emacs-lisp-mode
     ;; nix-mode
     ;; nix-ts-mode
     ;; rust-mode
     ;; rust-ts-mode
     ;; markdown-mode
     ;; js-mode
     ;; js-ts-mode
     ;; html-mode
     ;; html-ts-mode
     ;; css-mode
     ;; css-ts-mode
     ;; json-mode
     ;; json-ts-mode
     ;; python-mode
     ;; python-ts-mode
     ;; elixir-mode
     ;; elixir-ts-mode
     ;; erlang-mode
     ;; erlang-ts-mode
     ;; c-mode
     ;; c++-mode
     ;; clojure-mode
     ;; clojure-ts-mode
     ;; yaml-mode
     ;; yaml-ts-mode
     ) . corfu-mode))
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
        ("C-g" . corfu-quit)
	)
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.5)
  (corfu-preselect 'first)
  (corfu-preview-current 'insert)
  (corfu-popupinfo-delay '(2.0 . 0.5))
  (corfu-quit-no-match 'separator)
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

  ;; (set-face-background 'corfu-current "#2A3456")
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
  (add-hook 'completion-at-point-functions #'cape-abbrev)
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
  :bind (:map vertico-map
              ("<tab>" . minibuffer-complete)
              ("M-g M-c" . switch-to-completions)
              ("M-<tab>" . vertico-insert)
              ("<backtab>" . vertico-insert)
	      ("C-M-n" . vertico-next-group)
	      ("C-M-p" . vertico-previous-group)
	      ("s-b" . vertico-buffer-mode)
	      )
  :init
  (vertico-mode)
  :custom
  (vertico-scroll-margin 2) ;; Different scroll margin
  (vertico-count 12) ;; Show more candidates
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer. Other values: t, grow-only
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :config
  (vertico-mouse-mode +1)
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
         ("s-<0x10081247> s-K" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ;;("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-?" . consult-register-store)
         ("C-M-¡" . consult-register)
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
         ("s-<0x10081247> s-O" . consult-org-heading)
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

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :custom
  (consult-buffer-sources
   '(consult-source-buffer
     consult-source-hidden-buffer
     consult-source-modified-buffer
     consult-source-other-buffer
     consult-source-recent-file
     consult-source-buffer-register
     consult-source-file-register
     consult-source-bookmark
     consult-source-project-buffer
     consult-source-project-recent-file
     consult-source-project-root
     consult-source-project-buffer-hidden
     consult-source-project-recent-file-hidden
     consult-source-project-root-hidden
     ))
  :config

  ;; Number of characters needed when using an async consult command (e.g. consult-grep, consult-find)
  (setq consult-async-min-input 3)

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

(use-package consult-dir
    :ensure t
    ;; :after (consult)
    :custom
    (consult-dir-sources
     '(consult-dir--source-default
       consult-dir--source-project
       consult-dir--source-recentf
       consult-dir--source-tramp-local
       consult-dir--source-bookmark
       ))
    (consult-dir-sort-candidates nil)
    :bind ((:map vertico-map ;; minibuffer-local-completion-map
                 ("C-x M-d" . consult-dir)
                 ("C-x M-j" . consult-dir-jump-file)
                 :map global-map
                 ("C-x M-d" . consult-dir)
                 ))
    :commands (consult-dir consult-dir-jump-file)
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

(use-package consult-symbol
  :ensure t
  :bind ("C-h o" . consult-symbol)
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
  :bind
  (("s-m ." . embark-act)
   ("s-m ," . embark-dwim)
   ("s-m b" . embark-bindings)
   ("s-m e" . embark-export)
   ("s-m l" . embark-collect)
   ("s-m s" . embark-select)
   ("s-m a" . embark-act-all)
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
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  )

(use-package avy-embark-collect
  :ensure t
  :after (embark avy)
  )

;; Orderless
(use-package orderless
  :ensure t
  ;;:after vertico
  :custom
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  ;; (completion-styles '(orderless basic)) ;;partial-completion
  (completion-styles '(orderless partial-completion basic)) ;;partial-completion
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)) ;;basic
                                   (eglot (styles orderless))
                                   (eglot-capf (styles orderless))))
  (completion-pcm-leading-wildcard t) ;; Emacs 31: partial-completion behaves like substring
  :config
  (setq completion-preview-completion-styles completion-styles)
  )

;; Prescient (used as the sorting algorithm of this stack)
(use-package prescient
  :ensure t
  :demand t
  :custom
  ;; Works well with `initialism'.
  (prescient-sort-full-matches-first t) ;; default: nil
  (prescient-filter-method '(literal initialism prefix regexp))
  (prescient-use-char-folding t)
  (prescient-use-case-folding 'smart)
  (prescient-sort-length-enable t) ;; default: t
  (prescient-aggressive-file-save nil) ;; default: nil
  (prescient-history-length 500) ;; default: 100
  (prescient-frequency-decay 0.807) ;; default: 0.997
  (prescient-frequency-threshold 0.05) ;; default: 0.05
  (prescient-save-file (file-truename (concat user-emacs-directory "prescient/prescient-save.el")))
  :config
  (setq corfu-sort-function #'prescient-completion-sort)
  (setq vertico-sort-function #'prescient-completion-sort)
  (setq completion-preview-sort-function #'prescient-completion-sort)
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

;; ;; checking for missing cli
;; (defun missing-cli (cli-list)
;;   "Check each element of CLI-LIST to verify if they exist as cli."
;;   (seq-filter
;;    (lambda (cli)
;;      (not (executable-find cli)))
;;    cli-list))
;;
;; (defun cli-sanity-check (cli-list)
;;   "Check if each CLI-LIST element exists.
;; For each non-existent cli throw a warning."
;;   (let ((mcli (missing-cli cli-list)))
;;     (when mcli
;;       (display-warning
;;        'environment
;;        (format "Missing CLI commands: %s"
;;                (string-join mcli ", "))
;;        :warning))))

;; (cli-sanity-check '("rustup" "uv" "java" "node")) ;"asdf"

;; LSP, DAP, linter and formatter installer
(use-package mason
  :ensure t
  :config
  (mason-ensure
   (lambda ()
     (dolist (pkg
              '(
                "clangd"
                "cljfmt"
                "clojure-lsp"
                "codelldb"
                "css-lsp"
                ;; "debugpy" ;; couldn't make mason-installed debugpy work, so I ended up installing it through uv.
                "html-lsp"
                "java-debug-adapter"
                "jdtls"
                "js-debug-adapter"
                "json-lsp"
                "prettier"
                "rassumfrassum"
                "ruff"
                "rust-analyzer"
                "tombi"
                "ty"
                "typescript-language-server"
                "yaml-language-server"
                ))
       (unless (mason-installed-p pkg)
	 (ignore-errors (mason-install pkg))))))
  (mason-setup))

;; Packages to manage PostgreSQL
;; To upgrade use package-vc-upgrade

;; pgmacs
(use-package pgmacs
  :vc (:url "https://github.com/emarsden/pgmacs"
            :rev :newest
            :branch "main"
            :vc-backend Git)
  :ensure t
  :defer t
  )

;; pgmacs
(use-package pg
  :vc (:url "https://github.com/emarsden/pg-el"
            :rev :newest
            :branch "main"
            :vc-backend Git)
  :ensure t
  :defer t
  )

;; Integration for uv, the Python package manager
;; (use-package uv-mode
;;   :ensure t
;;   :defer t
;;   :hook (python-mode . uv-mode-auto-activate-hook)
;;   :hook (python-ts-mode . uv-mode-auto-activate-hook)
;;   :if (executable-find "uv")
;;   )

;; Package to integrate various python tools. I'll leave this here just in case I end up using it.
;; (use-package pet
;;   :ensure t
;;   :config
;;   (add-hook 'python-base-mode-hook 'pet-mode -10)
;;   ;; (add-hook 'python-mode-hook
;;   ;;         (lambda ()
;;   ;;           (setq-local python-shell-interpreter (pet-executable-find "python")
;;   ;;                       python-shell-virtualenv-root (pet-virtualenv-root))))
;;   )

;; Note to myself: look how to configure it.
(use-package apheleia
  :ensure t
  :bind (
         ("s-<0x10081247> s-A" . 'apheleia-format-buffer)
         )
  :config
  (add-to-list 'apheleia-mode-alist '(toml-ts-mode . tombi))
  (add-to-list 'apheleia-formatters '(tombi "tombi" "format"))
  ;; (apheleia-global-mode)
  )

(use-package disaster
  :ensure t
  :defer t
  :config
  ;; (define-key fortran-mode-map (kbd "s-d s") 'disaster)
  )

(use-package cc-mode
  :defer t
  :config
  (define-key c-mode-map (kbd "s-d s") 'disaster)
  (define-key c++-mode-map (kbd "s-d s") 'disaster)
  (setq-default c-basic-offset 4)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; json config
(use-package json-mode
  :ensure t
  :defer t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; toml config
(use-package toml-mode
  :ensure t
  :defer t
  :config
  (require 'tomlparse)
  )

(use-package toml-ts-mode
  :ensure t
  :defer t
  :config
  (require 'tomlparse)
  )

(use-package toml
  :ensure t
  :defer t
  )

(use-package tomlparse
  :ensure t
  :defer t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; yaml config
(use-package yaml
  :ensure t
  :defer t
  )

(use-package yaml-mode
  :ensure t
  :defer t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Clojure config

;; clojure-mode / clojure-ts-mode
(use-package clojure-mode
  :ensure t
  :defer t
  :custom
  (clojure-indent-style 'always-align)
  :commands (clojure-mode)
  )

(use-package clojure-ts-mode
  :ensure t
  :defer t
  :commands (clojure-ts-mode)
  )

(use-package cider
  :ensure t
  :after (clojure-mode)
  :custom
  (cider-preferred-build-tool nil) ;clj
  :config
  (setq cider-repl-pop-to-buffer-on-connect nil)
  (setq cider-show-error-buffer t)
  (setq cider-auto-select-error-buffer t)
  (setq cider-repl-history-file nil)
  (setq cider-repl-wrap-history nil)
  (setq cider-prompt-for-symbol nil)
  (setq cider-repl-use-pretty-printing t)
  (setq nrepl-log-messages nil)
  (setq cider-font-lock-dynamically (macro core deprecated))
  (setq cider-eldoc-display-for-symbol-at-point t)
  (setq cider-prompt-for-symbol nil)
  (setq cider-use-xref t)
  :commands (cider cider-jack-in)
  )

;; Other packages to consider: clj-refactor.el, inf-clojure

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Elixir / Erlang config

;; Elixir
;; (use-package elixir-mode
;;   :ensure t
;;   :defer t
;;   )

;; (use-package mix
;;   :ensure t
;;   :defer t
;;   :config
;;   (add-hook 'elixir-mode-hook 'mix-minor-mode)
;;   (add-hook 'elixir-ts-mode-hook 'mix-minor-mode)
;;   )

;; (use-package exunit
;;   :ensure t
;;   :defer t
;;   :config
;;   (add-hook 'elixir-mode-hook 'exunit-mode)
;;   (add-hook 'elixir-ts-mode-hook 'exunit-mode)
;;   )

;; (use-package inf-elixir
;;   :ensure t
;;   :defer t
;;   :bind (("C-c i i" . 'inf-elixir)
;;          ("C-c i p" . 'inf-elixir-project)
;;          ("C-c i l" . 'inf-elixir-send-line)
;;          ("C-c i r" . 'inf-elixir-send-region)
;;          ("C-c i b" . 'inf-elixir-send-buffer)
;;          ("C-c i R" . 'inf-elixir-reload-module))
;;   )

;; (use-package ob-elixir
;;   :ensure t
;;   :defer t
;;   :config
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '((emacs-lisp . t)
;;      (elixir . t)))
;;   )

;; (use-package heex-ts-mode
;;   :ensure t
;;   :defer t
;;   )

;; Erlang
;; (use-package erlang
;;   :ensure t
;;   :defer t
;;   :config
;;   ;; (setq load-path (cons "/usr/local/otp/lib/tools/emacs" ;;"/usr/local/otp/lib/tools-<ToolsVer>/emacs"
;;   ;;                       load-path))
;;   ;; (setq erlang-root-dir "/usr/local/otp")
;;   ;; (setq exec-path (cons "/usr/local/otp/bin" exec-path))
;;   ;; (require 'erlang-start)
;;   ;; (require 'erlang-flymake)
;;   )

;; (use-package erlang-ts
;;   :ensure t
;;   ;; :mode ("\\.erl\\'" . erlang-ts-mode)
;;   :defer t
;;   )

;; (use-package edts
;;   :ensure t
;;   :defer t
;;   )

;; (use-package ob-erlang
;;   :vc (:url "https://github.com/B7rian/ob-erlang"
;;        :rev :newest
;;        :branch "master"
;;        :vc-backend Git)
;;   :ensure t
;;   :defer t
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;;; typescript-mode config
;; (use-package typescript-mode
;;   :ensure t
;;   :defer t
;;   :config
;;   (require 'ansi-color)
;;   (defun colorize-compilation-buffer ()
;;     (ansi-color-apply-on-region compilation-filter-start (point-max)))
;;   (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
;;   )

;; ;; tide package config
;; (use-package tide
;;   :ensure t
;;   :defer t
;;   :config
;;   (defun setup-tide-mode ()
;;     (interactive)
;;     (tide-setup)
;;     (eldoc-mode +1)
;;     (tide-hl-identifier-mode +1))

;;   ;; ;; formats the buffer before saving
;;   ;; (add-hook 'before-save-hook 'tide-format-before-save)

;;   ;; if you use typescript-mode
;;   (add-hook 'typescript-mode-hook #'setup-tide-mode)
;;   ;; if you use treesitter based typescript-ts-mode (emacs 29+)
;;   (add-hook 'typescript-ts-mode-hook #'setup-tide-mode)
;;   )

;; ;;;; tsx-mode config
;; (use-package tsx-mode
;;   :vc (:url "https://github.com/orzechowskid/tsx-mode.el"
;;        :rev :newest
;;        :branch "emacs30"
;;        :vc-backend Git)
;;   :ensure t
;;   :defer t
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Rust config
(use-package rust-mode
  :ensure t
  :defer t
  )
;; :init
;; (setq rust-mode-treesitter-derive t)
;; ;; (setq rust-format-on-save t) ;; You can also use Apheleia as an alternative for this variable.
;; :hook
;; (rust-mode . flymake-mode))
;; ;; (rust-ts-mode . flymake-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; LaTeX and pdf configuration

;; AUCTeX
(use-package auctex
  :ensure t
  :defer t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  ;; (setq TeX-view-program-selection '(((output-dvi has-no-display-manager) "dvi2tty")
  ;; 				     ((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi")
  ;; 				     (output-pdf "PDF Tools") (output-html "xdg-open"))
  ;; 	) ;; changed (output-pdf "Evince") to (output-pdf "PDF Tools")
  ;; (setq TeX-source-correlate-start-server t)
  ;; (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  ;; (setq TeX-PDF-mode t) ;; use PDFTeX by default
  ;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  ;; (setq reftex-plug-into-AUCTeX t)
  )

;; RefTeX
(use-package reftex ;; Not necessary, just a formality
  :defer t
  )

;; pdf-tools
(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install) ;; To uninstall use the function `pdf-tools-uninstall'
  ;; For more info about dependencies check ~/.emacs.d/elpa/pdf-tools-1.3.0/server/autobuild
  ;; Dependencies (Fedora Linux): autoconf automake gcc libpng-devel make poppler-devel poppler-glib-devel zlib-devel
  ;; Dependencies (NixOS Linux): automake autoconf pkg-config libpng zlib poppler
  (setq pdf-info-epdfinfo-program "~/.emacs.d/.cache/epdfinfo")
  ;;(setq-default pdf-view-display-size 'fit-page)
  ;;(setq pdf-annot-activate-created-annotations t)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; dape configuration
(use-package dape
  :ensure t
  :defer t
  :bind (:map global-map
              ("C-x C-a d" . dape)
              )
  :config
  (setq dape-buffer-window-arrangement 'right)

  ;; Put symlinks on debug-adapters directory so dape sees where the debuggers are located.
  (unless (file-exists-p (expand-file-name "~/.emacs.d/debug-adapters/codelldb/"))
    (make-symbolic-link (expand-file-name "~/.emacs.d/mason/packages/codelldb/") (expand-file-name "~/.emacs.d/debug-adapters/codelldb/")))

  (unless (file-exists-p (expand-file-name "~/.emacs.d/debug-adapters/com.microsoft.java.debug.plugin-0.53.2.jar"))
    (make-symbolic-link (expand-file-name "~/.emacs.d/mason/packages/java-debug-adapter/extension/server/com.microsoft.java.debug.plugin-0.53.2.jar") (expand-file-name "~/.emacs.d/debug-adapters/com.microsoft.java.debug.plugin-0.53.2.jar")))

  (unless (file-exists-p (expand-file-name "~/.emacs.d/debug-adapters/js-debug-adapter"))
    (make-symbolic-link (expand-file-name "~/.emacs.d/mason/bin/js-debug-adapter") (expand-file-name "~/.emacs.d/debug-adapters/js-debug-adapter")))

  ;; (setenv "PATH" (concat "~/.emacs.d/mason/bin:" (getenv "PATH")))
  ;; (setq exec-path (cons "~/.emacs.d/mason/bin" exec-path))

  (add-to-list 'dape-configs
               `(js-debug-adapter
                 modes (js-mode js-ts-mode typescript-mode typescript-ts-mode)
                 command "node"
                 command-args (,(expand-file-name "~/.emacs.d/mason/packages/js-debug-adapter/js-debug/src/dapDebugServer.js") "8123")
                 :type "pwa-node"
                 :request "launch"
                 :cwd "${workspaceFolder}"
                 :program (if (buffer-file-name) ;; if buffer is visiting file, return file name, else nil
                              (setq js-debug-buffer-filename (buffer-file-name)) ;; if buffer-file-name = non-nil set the file name
                            js-debug-buffer-filename) ;; else either use nil or assume last file visited is target
                 ;; for some reason ${file} didn't work on :program
                 ensure dape-ensure-command command-cwd
                 dape-command-cwd command "js-debug-adapter"
                 host "localhost"
                 port 8123))

  ;; For now this won't be necessary
  ;; (add-to-list 'dape-configs
  ;;              `(codelldb-rust-2 modes (rust-mode rust-ts-mode) command-args
  ;;                                ("--port" :autoport "--settings"
  ;;                                 "{\"sourceLanguages\":[\"rust\"]}")
  ;;                                ensure dape-ensure-command command-cwd
  ;;                                dape-command-cwd command "codelldb" port :autoport :type "lldb"
  ;;                                :request "launch" :cwd "." :program
  ;;                                (file-name-concat "target" "debug"
  ;;                                                  (car
  ;;                                                   (last
  ;;                                                    (file-name-split
  ;;                                                     (directory-file-name (dape-cwd))))))))

  )
