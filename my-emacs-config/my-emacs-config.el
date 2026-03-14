(require 'cl-lib)

(use-package emacs
  :custom
  ;; Remove startup screen
  (inhibit-startup-screen t)
  (inhibit-startup-echo-area-message nil) ;;user-login-name
  (inhibit-default-init nil)
  ;; Remove scratch text
  (initial-scratch-message nil)
  ;; Turn off clickable text highlight
  (mouse-highlight nil)
  ;; Disable use of system trash
  (delete-by-moving-to-trash nil)
  ;; Highlight active region in nonselected windows.
  (highlight-nonselected-windows t)
  ;; Dialog boxes
  (use-file-dialog t)
  (use-dialog-box t)
  ;; Create lock files
  (create-lockfiles t)
  (enable-recursive-minibuffers t)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)) ;; Original value: (read-only t face minibuffer-prompt)
  ;; If already indented, try to complete the text.
  (tab-always-indent 'complete)
  ;; Blink the screen
  (visible-bell t)
  ;; Scroll settings
  (scroll-conservatively 100000)
  (scroll-margin 6)
  (scroll-step 1)
  (scroll-preserve-screen-position t)
  ;; Short yes or no answer
  (use-short-answers t)
  ;; Use mark even when region is inactive
  (mark-even-if-inactive t)
  ;; Limit on depth in ‘eval’, ‘apply’ and ‘funcall’ before error.
  (max-lisp-eval-depth 1600)
  ;; Whether to ignore case when searching
  (case-fold-search 1) ;; For some reason setting t to this variable doesn't seem to work. At least setting 1 does work.
  ;; Undo limits
  (undo-limit 160000) ;(* 13 160000)
  (undo-strong-limit 240000) ;(* 13 240000)
  (undo-outer-limit 24000000) ;(* 13 24000000)
  ;; Disable fontification during user input to reduce lag in large buffers.
  (redisplay-skip-fontification-on-input t)
  (fill-column 80)
  (display-line-numbers-width nil)
  (fast-but-imprecise-scrolling t)

  :config
  ;; Disable compact font caches
  (setq inhibit-compacting-font-caches t)
  ;; Change default cursor type
  (setq-default cursor-type t)
  ;; (setq-default cursor-type '(bar . 7)) ;; default: t
  ;; Default image scaling
  (setq-default image-scaling-factor 'auto)

  ;; Bidirectional editing config
  (setq-default bidi-display-reordering 'left-to-right)
  (setq-default bidi-paragraph-direction 'left-to-right)
  (if (version<= "27.1" emacs-version)
      (setq bidi-inhibit-bpa t))

  ;; Modify the appearance of the region
  ;; (custom-set-faces '(region ((t :extend t)))) ;; Use ':extend t' or ':extend nil' to modify if region covers entire line.
  )

(use-package frame
  :custom
  ;; Change cursor's appearance
  (blink-cursor-mode t)
  (window-divider-default-places t)
  :config
  ;; Change font
  ;; (set-frame-font "Adwaita Mono 12" nil t)
  ;; (set-frame-font "JetBrainsMono Nerd Font Mono 12" nil t)
  ;; (set-frame-font "Hack Nerd Font Mono 12" nil t)
  ;; (set-frame-font "Iosevka Nerd Font Mono 14" nil t)
  ;; (set-frame-font "FiraCode Nerd Font Mono Light 12" nil t)
  ;; (set-frame-font "UbuntuSansMono Nerd Font Mono 13" nil t)
  ;; (set-frame-font "Ioskeley Mono Light 12" nil t)
  ;; (set-frame-font "Inconsolata Nerd Font Mono 14" nil t)
  (set-frame-font "JuliaMono Light 12" nil t)
  ;; Maximize Emacs on start
  ;;(add-to-list 'default-frame-alist '(fullscreen . maximized))
  ;;(toggle-frame-fullscreen)
  (window-divider-mode +1)
  (toggle-frame-maximized)
  )

(use-package window
  :bind
  (:map global-map
        ;; Set key for burying buffers without deleting them
        ("C-x M-k" . bury-buffer)
        ;; Set key for swapping windows
        ("C-x M-o" . window-swap-states)
   )
  )

;; Enable syntax highlighting
(use-package font-core
  :config
  (global-font-lock-mode t)
  )

;; Package for miscellaneous features
(use-package simple
  :bind (:map global-map
              ("H-x s-q" . indent-tabs-mode)
              ("H-x s-e" . delete-trailing-whitespace)
              ("H-w" . count-words)
              ("C-S-/". undo-only)
              ("s-<0x10081247> s-P p" . auto-fill-mode)
              ("s-<0x10081247> s-P d" . display-fill-column-indicator-mode)
              ("s-<0x10081247> s-P f" . fill-paragraph)
              )
  ;; Enable auto-save-mode only when the buffer is associated with a file.
  :hook (after-change-major-mode . (lambda ()
                                     (if (buffer-file-name)
                                         (auto-save-mode +1)
                                       (auto-save-mode -1))))
  :custom
  (line-number-mode t)
  (column-number-mode t)
  (size-indication-mode t)
  (read-extended-command-predicate #'command-completion-default-include-p) ;; Other value(s): nil, transient-command-completion-not-suffix-only-p
  ;; Show character name in ‘what-cursor-position’
  (what-cursor-show-names t)
  (copy-region-blink-predicate #'region-indistinguishable-p); default: #'region-indistinguishable-p
  ;;(save-interprogram-paste-before-kill t)
  (kill-whole-line nil)
  (line-move-visual t)
  (set-mark-command-repeat-pop t)
  (delete-active-region t)
  ;; Allow / disallow "region-aware" commands to act on empty regions
  (use-empty-active-region nil)
  ;; Disable truncation of printed s-expressions in the message buffer
  (eval-expression-print-length nil)
  (eval-expression-print-level nil)
  (kill-do-not-save-duplicates t)
  (blink-matching-paren t)
  (next-line-add-newlines nil)
  :config
  ;; Truncate long lines
  ;;(setq-default truncate-lines t)
  ;;(setq-default word-wrap nil) ;; Visual Line mode sets this to t
  (global-visual-line-mode t)
  ;; Use spaces for indentation
  ;; Alternatively, you can modify the variable `tab-width'
  ;; (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)
  ;; Show trailing whitespaces
  (setq-default show-trailing-whitespace t)
  ;; (add-hook 'before-save-hook #'delete-trailing-whitespace)
  ;; Auto-Save-Mode
  ;; (auto-save-mode +1)
  ;; (global-display-fill-column-indicator-mode +1)
  ;; Enable Transient Mark Mode
  (transient-mark-mode +1)
  )

(use-package visual-wrap
  :custom
  (visual-wrap-extra-indent 0)
  :config
  (global-visual-wrap-prefix-mode +1)
  )

;; Show number of the lines
(use-package display-line-numbers
  :config
  (global-display-line-numbers-mode +1)
  )

;; Enable use of system clipboard
(use-package select
  :custom
  (select-enable-clipboard t)
  )

;; Tab Bars
(use-package tab-bar
  :custom
  (tab-bar-history-mode nil)
  (tab-bar-auto-width-max '((300) 30))
  ;; tab-bar-tab-name-current, tab-bar-tab-name-current-with-count, tab-bar-tab-name-truncated, tab-bar-tab-name-all
  (tab-bar-tab-name-function #'tab-bar-tab-name-current)
  (tab-bar-tab-name-truncated-max 20)
  (tab-bar-auto-width t)
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)
  :config
  (setq tab-bar-tab-name-ellipsis t)
  (tab-bar-mode +1)
  )

;; Tab Lines
(use-package tab-line
  :custom
  ;; tab-line-tab-name-buffer, tab-line-tab-name-truncated-buffer
  (tab-line-tab-name-function #'tab-line-tab-name-buffer)
  (tab-line-tab-name-truncated-max 20)
  (tab-line-new-button-show nil)
  (tab-line-close-button-show nil)
  :config
  (setq tab-line-tab-name-ellipsis t)
  (add-to-list 'tab-line-tab-face-functions #'tab-line-tab-face-inactive-alternating)
  (global-tab-line-mode +1)
  )

;; Enable menu-bar
(use-package menu-bar
  :config
  (menu-bar-mode +1)
  )

;; Disable tool-bar
(use-package tool-bar
  :defer t
  :config
  (tool-bar-mode -1)
  ;;(modifier-bar-mode -1)
  )

;; Enable tooltips
(use-package tooltip
  :config
  (tooltip-mode +1)
  )

;; Enable Vertical Scroll Bar
(use-package scroll-bar
  :config
  ;; (scroll-bar-mode 'right)
  (scroll-bar-mode -1)
  )

;; Replace a selected area with typed text
(use-package delsel
  :custom
  (delete-selection-temporary-region nil)
  :config
  (delete-selection-mode +1)
  )

;; Add directory to desktop-path
(use-package desktop
  :defer t
  :config
  (add-to-list 'desktop-path (expand-file-name "desktop-sessions/" user-emacs-directory))
  (setq desktop-dirname (expand-file-name "desktop-sessions/" user-emacs-directory))
  :commands (desktop-save desktop-read desktop-remove)
  )

;; Display warnings depending of level
(use-package warnings
  :custom
  (warning-minimum-level :warning)
  )

;; Disables docstring warnings
(use-package bytecomp
  :custom
  (byte-compile-warnings
      '(not docstrings))
  )

;; Display time and date on the minibuffer (neat stuff)
(use-package time
  :custom
  (display-time-day-and-date t)
  (display-time-default-load-average nil)
  (display-time-format "%I:%M %a %d-%m-%Y")
  :config
  (display-time-mode t)
  )

;; Save minibuffer history. By default it will be on ~/.emacs.d/history
(use-package savehist
  :defer t
  :custom
  (savehist-save-minibuffer-history t)
  (savehist-file (concat user-emacs-directory "history"))
  ;; (savehist-additional-variables '((kill-ring . 20)
  ;;                                  search-ring
  ;;                                  regexp-search-ring
  ;;                                  register-alist
  ;;                                  mark-ring
  ;;                                  global-mark-ring
  ;;                                  (recentf-list . 50)
  ;;                                  ))
  :config
  (setq history-length 150)
  (setq history-delete-duplicates t)
  (savehist-mode +1)
  )

;; Auto-refresh buffers. If a file was changed on disk, revert changes on buffer.
(use-package autorevert
  :custom
  (global-auto-revert-ignore-modes '(doc-view-mode pdf-view-mode))
  (auto-revert-remote-files nil)
  (auto-revert-verbose t)
  (auto-revert-interval 5)
  (auto-revert-avoid-polling nil)
  (auto-revert-stop-on-user-input nil)
  (global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode +1)
  )

(use-package minibuffer
  :custom
  ;; More detailed completions
  (completions-detailed t)
  :config
  ;; Config for vertico package
  (setq completion-in-region-function #'consult-completion-in-region) ;;default: #'completion--in-region
  )

;; Enable right click menu
(use-package mouse
  :config
  (context-menu-mode +1)
  )

;; Display differences between files / buffers
(use-package diff
  :defer t
  ;; Define keys for diff
  :bind (:map global-map
              ("H-d 1" . diff)
              ("H-d 2" . diff-buffers))
  :custom
  ;; While (diff-font-lock-syntax 'hunk-also) looks great, it hinders me when I try to check if I added or removed something.
  ;; (diff-font-lock-syntax 'hunk-also)
  (diff-font-lock-syntax nil) ;; default: t
  (diff-font-lock-prettify t)
  (diff-switches "-u")
  ;; (diff-switches '("--color=never"))
  )

;; Interactively display differences between files / buffers
(use-package ediff
  :defer t
  ;; Define keys for ediff
  :bind (:map global-map
              ("H-d 3" . ediff)
              ("H-d 4" . ediff-buffers))
  :hook (ediff-prepare-buffer . outline-show-all)
  :custom
  (ediff-keep-variants t)
  (ediff-make-buffers-readonly-at-startup nil)
  (ediff-show-clashes-only nil)
  (ediff-split-window-function 'split-window-horizontally) ;; default: 'split-window-vertically
  (ediff-window-setup-function 'ediff-setup-windows-plain) ;; default: 'ediff-setup-windows-default
  )

;; EditorConfig (https://editorconfig.org/)
;; You can also use .dir-locals.el and .dir-locals-2.el, both alongside and as alternatives to .editorconfig files
(use-package editorconfig
  :config
  (editorconfig-mode t)
  )

;; eldoc config
(use-package eldoc
  :bind
  (:map global-map
        ;; Define easy key for eldoc documentation
        ("s-. ." . eldoc)
   )
  :custom
  (eldoc-idle-delay 0.7) ;; default: 0.5
  (eldoc-echo-area-display-truncation-message nil)
  :config
  (global-eldoc-mode +1)
  )

;; Display battery status on mode-line
(use-package battery
  :config
  (display-battery-mode +1)
  )

;; Display name of a "function" (depends of the context)
(use-package which-func
  :custom
  (which-func-update-delay 0.5)
  (which-func-display 'header)
  :config
  (which-function-mode +1)
  )

(use-package compile
  ;; Add key for compile
  :bind (:map global-map
              ("s-<0x10081247> s-C" . compile))
  :custom
  ;; Follow the compilation buffer
  (compilation-scroll-output t) ;; 'first-error
  (compilation-ask-about-save t)
  (compilation-always-kill t)
  (compilation-max-output-line-length nil)
  :commands (compile)
  )

;; Native compilation for Emacs
(use-package comp
  :custom
  (native-comp-speed 2)
  :config
  (setq native-comp-jit-compilation t)
  )

;; Dired config
(use-package dired
  :defer t
  :ensure nil
  :custom
  (dired-listing-switches "-ahl --group-directories-first")
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-free-space 'first)
  (dired-dwim-target t)
  (dired-vc-rename-file t)
  (dired-create-destination-dirs 'ask)
  (dired-clean-confirm-killing-deleted-buffers t)
  (dired-auto-revert-buffer nil)
  ;; :config
  ;; (setq dired-deletion-confirmer 'y-or-n-p)
  )

;; grep config
(use-package grep
  ;; Set key for grep
  :bind (:map global-map
              ("H-g" . grep))
  :custom
  ;; This setting is a pre-requirement for nerd-icons-grep, so an
  ;; icon can be displayed near each heading
  (grep-use-headings t)
  (grep-command "grep --color=auto -nH --null -r -i ") ;; Original: "grep --color=auto -nH --null -e "
  )

;; Rectangle mode config
(use-package rect
  :defer t
  :bind
  (:map rectangle-mark-mode-map
        ("s-a" . string-rectangle))
  :defer t)

(use-package descr-text
  :custom
  ;; Control how you want to show info using `what-cursor-position'
  ;; (describe-char-unidata-list '(name old-name general-category decomposition canonical-combining-class bidi-class decimal-digit-value digit-value numeric-value mirrored iso-10646-comment uppercase lowercase titlecase))
  (describe-char-unidata-list t)
  )

;; Calc config
(use-package calc
  :defer t
  :bind (:map global-map
              ("<Calculator>" . full-calc)
              )
  :init
  ;; Open full-calc
  ;; (define-key (current-global-map) (kbd "<Calculator>") 'full-calc)
  :config
  (setq calc-group-digits t)
  :commands (full-calc)
  )

;; Highlight pairs of parentheses
(use-package paren
  :custom
  (show-paren-style 'mixed) ;; default: 'parenthesis
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  (show-paren-highlight-openparen t)
  (show-paren-delay 0.15) ;; default: 0.125
  :config
  (show-paren-mode +1)
  )

(use-package vc
  :defer t
  :custom
  (vc-follow-symlinks 'ask)
  (vc-git-diff-switches '("--histogram")) ;; default: t
  (vc-git-print-log-follow nil)
  (vc-make-backup-files nil)
  (vc-command-messages nil)
  :bind (:map vc-dir-mode-map
              ("r" . vc-dir-refresh))
  :config
  (require 'vc-dir)
  (setq vc-log-short-style '(directory file))
  )

(use-package completion-preview
  :custom
  ;; The time before `completion-preview' appears.
  (completion-preview-idle-delay 0.5)
  ;; The minimum amount of letters needed for `completion-preview' to appear.
  (completion-preview-minimum-symbol-length 2)
  :config
  (global-completion-preview-mode)
  )

(use-package isearch
  :custom
  (isearch-lax-whitespace t)
  ;; Count the number of instances and show that number on the minibuffer.
  (isearch-lazy-count t)
  (isearch-lazy-highlight t)
  (isearch-wrap-pause t)
  )

;; recentf configuration
(use-package recentf
  :custom
  (recentf-save-file (expand-file-name "recentf" user-emacs-directory))
  (recentf-max-saved-items 50)
  (recentf-max-menu-items 20)
  (recentf-auto-cleanup 'mode) ;; default: 'mode
  (recentf-filename-handlers '(abbreviate-file-name)) ;; default: '(abbreviate-file-name)
  (recentf-exclude nil)
  :bind
  (:map recentf-mode-map
        ("s-<0x10081247> s-R" . recentf-open))
  :config
  (recentf-mode +1)
  )

(use-package saveplace
  :custom
  (save-place-file (expand-file-name "saveplace" user-emacs-directory))
  (save-place-limit 600)
  :config
  (save-place-mode +1)
  )

;; Backup config. Instead of automatically generating backup files, choose when and where to generate them.
(use-package files
  :custom
  ;; If nil, disable backups
  (make-backup-files nil)
  ;; Whether to create auto-save files or not
  (auto-save-default t)
  (auto-save-no-message nil)
  ;; Backup directory
  (backup-directory-alist `(("." . "~/.emacs_backup_files")))
  ;; Backup by copying instead of renaming original file
  (backup-by-copying t)
  ;; More than one backup
  (version-control t)
  (dired-kept-versions 2) ;; This should not have any effect on this specific config.
  (kept-new-versions 2)
  (kept-old-versions 0)
  (delete-old-versions t)
  ;; Always generate an empty line at the end of the file
  (require-final-newline t)
  (find-file-visit-truename t)
  (confirm-nonexistent-file-or-buffer 'after-completion)
  (safe-local-variable-values
   '((eval when (featurep 'package-lint-flymake) (package-lint-flymake-setup))))
  :config
  ;; (setq backup-inhibited t)

  (defun force-backup-of-file ()
    (interactive)
    (setq buffer-backed-up nil))

  (defun enable-or-disable-backups ()
    (interactive)
    (if make-backup-files
        (progn
          (setq make-backup-files nil)
          (message "Disabling backups."))
      (progn
        (setq make-backup-files t)
        (message "Enabling backups."))))

  (define-key (current-global-map) (kbd "s-<0x10081247> s-B f") 'force-backup-of-file)
  (define-key (current-global-map) (kbd "s-<0x10081247> s-B b") 'enable-or-disable-backups)

  ;; (add-hook 'before-save-hook 'force-backup-of-buffer)
  )

(use-package windmove
  ;; Set keys for changing window focus
  :bind (:map global-map
              ("s-<left>" . windmove-left)
              ("s-<right>" . windmove-right)
              ("s-<up>" . windmove-up)
              ("s-<down>" . windmove-down)
              ))

(use-package mwheel
  :custom
  (mouse-wheel-progressive-speed t))

(use-package prog-mode
  :hook (prog-mode . prettify-symbols-mode)
  ;; Note: `prettify-symbols-alist' is the list of symbols that are prettified by `prettify-symbols-mode'
  )

;; Some security config from minimal-emacs.d
(use-package gnutls
  :defer t
  :custom
  (gnutls-verify-error t) ;; Prompts user if there are certificate issues
  (gnutls-min-prime-bits 3072) ;; Stronger GnuTLS encryption
  )

(use-package tls
  :defer t
  :custom
  (tls-checktrust t)  ; Ensure SSL/TLS connections undergo trust verification
  )

(use-package cus-edit
  :custom
  ;; Exiting a custom buffer kills it
  (custom-buffer-done-kill t)
  ;; Use real function / variables names when customizing
  (custom-unlispify-tag-names nil)
  )

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'post-forward-angle-brackets)
  )

(use-package whitespace
  :bind (:map global-map
              ("H-x s-w" . whitespace-mode)
              )
  :custom
  (whitespace-line-column nil) ;; Use `fill-column' as the value of `whitespace-line-column'.
  )

(use-package epg-config
  :custom
  ;; Directs gpg-agent to use the minibuffer for passphrase entry.
  (epg-pinentry-mode 'loopback)
  )

(use-package auth-source
  :custom
  ;; Use GPG to encrypt the authinfo file. For more information check `(auth)Help for users' on the Emacs auth-source manual.
  (auth-sources (list "~/.authinfo.gpg"))
  )

(use-package imenu
  :custom
  (imenu-auto-rescan t)
  (imenu-max-item-length 160)
  )

(use-package comint
  :custom
  (comint-prompt-read-only t)
  (comint-buffer-maximum-size 4096)
  )

(use-package ansi-color
  :custom
  (ansi-color-for-comint-mode t)
  )

(use-package newcomment
  :custom
  (comment-multi-line t)
  (comment-empty-lines t)
  )

(use-package paragraphs
  :defer t
  :custom
  (sentence-end nil)
  (sentence-end-double-space t)
  (sentence-end-without-period nil)
  (sentence-end-base "[.?!…‽][]\"'”’)}»›]*")
  )

(use-package fill
  :defer t
  :custom
  (colon-double-space nil)
  )

(use-package apropos
  :custom
  (apropos-do-all t)
  )

(use-package help
  :custom
  (help-enable-completion-autoload nil)
  (help-enable-autoload nil)
  (help-enable-symbol-autoload nil)
  ;; When created, change focus to Help buffer
  (help-window-select nil)
  ;; Reuse Help window in contexts other than another Help buffer
  (help-window-keep-selected t)
  )

(use-package ispell
  :defer t
  :custom
  (ispell-silently-savep t)
  )

(use-package flyspell
  :defer t
  ;; :custom
  ;; (flyspell-issue-message-flag nil)
  ;; (flyspell-issue-welcome-flag nil)
  )

(use-package text-mode
  :defer t
  :custom
  (text-mode-ispell-word-completion nil)
  )

;; This configuration sets up a few `package' repositories and their priorities, from largest to smallest integer.
(use-package package
  :bind (:map global-map
              ("H-q l" . list-packages)
              ("s-<0x10081247> s-L" . list-packages)
              )
  :custom
  ;; Priority for installation
  (package-archives
   '(("melpa"        . "https://melpa.org/packages/")
     ("gnu"          . "https://elpa.gnu.org/packages/")
     ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")
     ("gnu-devel"    . "https://elpa.gnu.org/devel/")
     ))
  (package-archive-priorities
   '(("melpa"        . 5)
     ("gnu"          . 4)
     ("nongnu"       . 3)
     ("melpa-stable" . 2)
     ("gnu-devel"    . 1)
     ))
  (package-selected-packages
   '(
     package-build
     package-lint
     package-lint-flymake
     guava-themes
     breadcrumb
     devdocs
     consult-dir
     elfeed
     disaster
     apheleia
     json-mode
     tomlparse
     toml-mode
     toml
     yaml
     yaml-mode
     consult-eglot-embark
     consult-eglot
     consult-yasnippet
     markdown-mode
     dape
     rust-mode
     dashboard
     mason
     nix-ts-mode
     nix-mode
     uv-mode
     nerd-icons-xref
     nerd-icons-grep
     doom-modeline
     ef-themes
     doric-themes
     morning-star-theme
     zenburn-emacs
     spacemacs-theme
     nerd-icons-ibuffer
     nerd-icons-corfu
     nerd-icons-completion
     nerd-icons-dired
     cider
     clojure-ts-mode
     clojure-mode
     nerd-icons
     vertico-prescient
     corfu-prescient
     prescient
     embark-consult
     avy-embark-collect
     embark
     marginalia
     vertico
     avy
     vundo
     auctex
     pdf-tools
     consult
     cape
     gnu-elpa-keyring-update
     envrc
     flymake-hledger
     hledger-mode
     ledger-mode
     orderless
     corfu
     focus
     treesit-fold
     pgmacs
     pg
     yasnippet
     doom-themes
     magit
     diff-hl
     )))

(use-package xref
  :custom
  ;; Use Consult to select xref locations with preview
  ;; alt value: #'xref-show-definitions-completing-read
  (xref-show-xrefs-function #'consult-xref) ;; default: xref--show-xref-buffer
  ;; alt value: #'xref-show-definitions-completing-read
  (xref-show-definitions-function #'consult-xref) ;;default: xref-show-definitions-buffer
  )

;; Highlight cursor line
(use-package hl-line
  :ensure nil
  :hook ((eshell-mode
          ;;eat-mode
          shell-mode
          term-mode
	  ;;vterm-mode
          comint-mode
          cfrs-input-mode
          image-mode
	  magit-diff-mode
          diff-mode
          )
         ;; Disable hl-line for some modes
         . (lambda () (setq-local global-hl-line-mode nil)))
  :custom
  (global-hl-line-sticky-flag t)
  :config
  ;; Gray, with disabled underline and overline
  ;; (set-face-background 'hl-line "#303030")
  ;; (custom-set-faces '(hl-line ((t (:background "#303030" :underline nil :overline nil)))))
  ;; (set-face-attribute 'hl-line nil :background "#404040" :underline nil :overline nil)
  ;; (set-face-attribute 'show-paren-match nil
  ;;                     :background (face-attribute 'hl-line :background))
  (global-hl-line-mode t)
  )

;; Project manager
(use-package project
  ;; Use demand to load the package automatically
  :demand t
  :config
  ;; Show project name on mode-line
  (setq project-mode-line t)
  )

;; Electric packages
(use-package elec-pair
  :custom
  (electric-pair-inhibit-predicate #'electric-pair-default-inhibit)
  :config
  (add-to-list 'electric-pair-pairs '(123 . 125) 'append)
  (electric-pair-mode 1))

(use-package electric
  :config
  ;; (add-to-list 'electric-indent-chars 32 'append) ;; '(?\n ?\^?)
  (electric-indent-mode 1)
  )

;; Automatically show available commands
(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 5.0)
  (which-key-setup-side-window-right-bottom)
  ;; `prefix-help-command' becomes  `which-key-C-h-dispatch'
  (which-key-mode)
  )

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
     c-mode
     c++-mode
     clojure-mode
     clojure-ts-mode
     yaml-mode
     yaml-ts-mode
     ) . flymake-mode))
  :bind (:map flymake-mode-map
              ("H-f l" . flymake-switch-to-log-buffer)
              ("H-f f" . flymake-show-buffer-diagnostics)
              ("H-f p" . flymake-show-project-diagnostics)
              ;; ("H-f s" . flymake-show-diagnostic)
              ;; ("H-f g" . flymake-goto-diagnostic)
              )
  :custom
  (flymake-no-changes-timeout 0.5)
  (flymake-fringe-indicator-position 'left-fringe)
  (flymake-margin-indicator-position 'left-margin)
  (flymake-show-diagnostics-at-end-of-line t)
  (flymake-wrap-around t)
  )

(use-package abbrev
  :custom
  (abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))
  (save-abbrevs 'silently)
  )

;; Dabbrev config
(use-package dabbrev
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)
         ("C-M-<Ungrab>" . dabbrev-expand))
  :custom
  (dabbrev-upcase-means-case-search t)
  (dabbrev-case-fold-search 'case-fold-search)
  ;; (dabbrev-ignored-buffer-regexps
  ;;     '(;; - Buffers starting with a space (internal or temporary buffers)
  ;;       "\\` "
  ;;       ;; Tags files such as ETAGS, GTAGS, RTAGS, TAGS, e?tags, and GPATH,
  ;;       ;; including versions with numeric extensions like <123>
  ;;       "\\(?:\\(?:[EG]?\\|GR\\)TAGS\\|e?tags\\|GPATH\\)\\(<[0-9]+>\\)?"))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\(?:\\(?:[EG]?\\|GR\\)TAGS\\|e?tags\\|GPATH\\)\\(<[0-9]+>\\)?")
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode)
  )

;; Org configuration
(use-package org
  :ensure t
  :defer t
  :custom
  (org-startup-indented t)
  (org-startup-folded 'overview)
  (org-persist-directory (expand-file-name ".cache/org-persist/" user-emacs-directory))
  (org-imenu-depth 8)
  ;;(org-return-follows-link nil)
  ;;(org-hide-emphasis-markers nil)
  ;;(org-agenda-files '("~/.emacs.d/org-agenda/"))
  ;;(org-src-window-setup 'reorganize-frame ;'current-window)
  ;;(org-hierarchical-todo-statistics t)
  ;;(org-image-actual-width t)
  ;;(org-pretty-entities nil)
  ;;(org-log-into-drawer nil)
  ;;(org-extend-today-until 0)
  ;;(org-use-effective-time nil)
  ;;(org-element-use-cache t)
  ;;(org-tags-column -77)
  ;;(org-reverse-note-order nil)

  ;;(org-confirm-babel-evaluate t)
  ;;(org-src-fontify-natively t)
  ;;(org-src-tab-acts-natively t)

  ;;(org-clock-clocked-in-display 'mode-line)
  ;;(org-habit-show-habits t)
  ;;(org-edit-src-content-indentation 2)
  ;;(org-cycle-include-plain-lists t)
  ;;(org-fold-core-style 'overlays)
  ;;(org-archive-location "%s_archive::") ;;"archive/%s_archive::"
  ;;(org-directory "~/.emacs.d/org-notes")
  ;;(org-id-link-to-org-use-id nil
  ;;(org-log-states-order-reversed t)
  ;;(org-log-note-clock-out nil)
  ;;(org-log-state-notes-insert-after-drawers nil)
  ;;(org-log-into-drawer nil)
  ;;(setq org-refile-targets nil) ;;'((org-agenda-files . (:maxlevel . 3))))
  ;;(setq org-refile-use-outline-path nil);; 'file)
  ;;(setq org-outline-path-complete-in-steps t)
  ;;(setq org-refile-allow-creating-parent-nodes nil);;'confirm)
  ;;:config
  ;; (with-eval-after-load 'org
  ;;   (org-babel-do-load-languages
  ;;    'org-babel-load-languages
  ;;    '((emacs-lisp . t)
  ;;      (shell . t)
  ;;      (scheme . t)
  ;;      (python . t)
  ;;      (ruby . t)
  ;;      (jupyter . t)
  ;;      (elixir . t)
  ;;      (erlang . t)
  ;;      (haskell . t)
  ;;      (js . t)
  ;;      (css . t)
  ;;      (sql . t)
  ;;      (gnuplot . t)
  ;;      (plantuml . t)
  ;;      (java . t)
  ;;      (c . t)
  ;;      )))
  )

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

;;;; eglot configuration
(use-package eglot
  :ensure nil
  :defer t
  :preface
  ;; https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc
  (defun mp-eglot-eldoc ()
    (setq eldoc-documentation-strategy
          'eldoc-documentation-compose-eagerly))
  :hook
  (eglot-managed-mode . mp-eglot-eldoc)
  ((nix-mode nix-ts-mode) . eglot-ensure)
  ((rust-mode rust-ts-mode) . eglot-ensure)
  ((java-mode java-ts-mode) . eglot-ensure)
  ((js-mode js-ts-mode) . eglot-ensure)
  ((python-mode python-ts-mode) . eglot-ensure)
  ((toml-mode toml-ts-mode) . eglot-ensure)
  ((yaml-mode yaml-ts-mode) . eglot-ensure)
  ((c-mode c++-mode) . eglot-ensure)
  ((html-mode html-ts-mode) . eglot-ensure)
  ((css-mode css-ts-mode) . eglot-ensure)
  ((clojure-mode clojure-ts-mode) . eglot-ensure)
  :bind (:map eglot-mode-map
              ("s-e c a" . eglot-code-actions)
              ("s-e o i" . eglot-code-action-organize-imports)
              ("s-e q f" . eglot-code-action-quickfix)
              ("s-e s d" . eglot-shutdown)
              ("s-e f t" . eglot-find-typeDefinition)
              ("s-e f b" . eglot-format-buffer)
              ("s-e f r" . eglot-format)
              ("s-e r n" . eglot-rename)
              ("s-e r w" . eglot-code-action-rewrite)
              ("s-e r c" . eglot-reconnect)
              )
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-edits '((eglot-rename . nil) (t . maybe-summary) (t . diff)))
  ;; async
  (eglot-sync-connect 0) ;; default: 3
  (eglot-connect-timeou1t 30)
  (eglot-ignored-server-capabilities nil) ;; examples: '(:inlayHintProvider), '(:documentHighlightProvider)', '(:documentFormattingProvider :documentRangeFormattingProvider :documentOnTypeFormattingProvider :colorProvider :foldingRangeProvider)
  (eglot-events-buffer-config '(:size 2000000 :format full))
  (eglot-extend-to-xref nil)
  (eglot-send-changes-idle-time 0.5)
  (eglot-report-progress t)
  :config
  ;; (fset #'jsonrpc--log-event #'ignore)
  ;; (setq jsonrpc-event-hook nil)

  ;; Enable cache busting, depending on if your server returns
  ;; sufficiently many candidates in the first place.
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  ;; (advice-add 'eglot-completion-at-point :around #'cape-wrap-nonexclusive)

  ;; (setopt eglot-send-changes-idle-time 0.5
  ;;         eglot-extend-to-xref nil)

  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
                 ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))

  ;; You need a Java compiler to use the lsp server JDTLS.
  ;; Also, you need to concat the absolute path, not relative.
  ;; (setenv "PATH" (concat (expand-file-name "~/.emacs.d/java-lts/jdk-21/bin:") (getenv "PATH")))

  ;; You can find the settings for JDTLS here: marketplace dot visualstudio dot com / items ?itemName=redhat.java

  (add-to-list 'eglot-server-programs
               `((java-mode java-ts-mode) .
                 ("jdtls"
                  :initializationOptions
                  (:bundles
                   ;; This needs to be the absolute path to java-debug-adapter
                   [,(expand-file-name "~/.emacs.d/mason/packages/java-debug-adapter/extension/server/com.microsoft.java.debug.plugin-0.53.2.jar")]
                   :settings
                   (:java
                    (:format
                     (:settings
                      (:url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")
                      :enabled t)
                     :completion
                     (:maxResults "100")))))))

  ;;   (add-to-list 'eglot-server-programs
  ;;              `((java-mode java-ts-mode) .
  ;;                ("jdtls"
  ;;                 :initializationOptions
  ;;                 (:bundles
  ;;                  ;; This needs to be the absolute path to java-debug-adapter
  ;;                  [,(expand-file-name "~/.emacs.d/mason/packages/java-debug-adapter/extension/server/com.microsoft.java.debug.plugin-0.53.2.jar")]))))

  ;; (setq-default eglot-workspace-configuration
  ;;               `(:java
  ;;                 (:format
  ;;                  (:settings
  ;;                   (:url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")
  ;;                   :enabled t)
  ;;                  :completion
  ;;                  (:maxResults 70))))


  (add-to-list 'eglot-server-programs
               '(((js-mode :language-id "javascript")
                  (js-ts-mode :language-id "javascript")
                  (js-jsx-mode :language-id "javascriptreact")
                  (tsx-ts-mode :language-id "typescriptreact")
                  (typescript-ts-mode :language-id "typescript")
                  (typescript-mode :language-id "typescript"))
                 . ("typescript-language-server" "--stdio")))
                 ;; . ("rass" "--" "typescript-language-server" "--stdio" "--" "quick-lint-js" "--lsp-server")))

  (add-to-list 'eglot-server-programs
               '((python-ts-mode python-mode) .
                 ("rass" "--" "ty" "server" "--" "ruff" "server")))

  (add-to-list 'eglot-server-programs
               '((toml-ts-mode toml-mode) .
                 ("tombi" "lsp"))) ;; "lint" "format" "completion"

  ;; (add-to-list 'eglot-server-programs
  ;;              '((c++-mode c-mode) .
  ;;                ("clangd")))

  ;; (add-hook 'prog-mode-hook
  ;;           (lambda ()
  ;;             (add-hook 'before-save-hook 'eglot-format nil t)))

  (add-to-list 'eglot-server-programs
               '((nix-mode) .
                 ("nixd")))

  (add-to-list 'eglot-server-programs
               '((yaml-mode yaml-ts-mode) .
                 ("yaml-language-server" "--stdio")))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Miscellaneous

;; Increase zoom
;; (add-hook 'after-change-major-mode-hook (lambda () (text-scale-set 3)))
(setq default-text-scale-mode-amount 2)

(setq forbidden-prefixes-text-scale-mode '("dape" "dashboard-mode"))

(add-hook 'after-change-major-mode-hook
	  (lambda ()
	    ;; (if (derived-mode-p 'treemacs-mode)
            (unless (cl-some (lambda (prefix)
                               (string-prefix-p prefix (symbol-name major-mode)))
                             forbidden-prefixes-text-scale-mode)
	        ;(text-scale-set 0)
	      (text-scale-set default-text-scale-mode-amount))
	    ))

;; Reduce text size when there are more than two windows
(advice-add 'split-window-right :after #'(lambda (&rest _)
                                           (setq default-text-scale-mode-amount 0)
                                           (text-scale-set default-text-scale-mode-amount)))

(advice-add 'split-window-below :after #'(lambda (&rest _)
                                           (setq default-text-scale-mode-amount 0)
                                           (text-scale-set default-text-scale-mode-amount)))

(advice-add 'delete-window :after #'(lambda (&rest _)
                                      (when (<= (length (window-list)) 1)
                                        (setq default-text-scale-mode-amount 2)
                                        (text-scale-set default-text-scale-mode-amount)
                                        )))

(advice-add 'delete-other-windows :after #'(lambda (&rest _)
                                             (setq default-text-scale-mode-amount 2)
                                             (text-scale-set default-text-scale-mode-amount)))


;; Enable commands
(put 'narrow-to-region 'disabled nil)
(put 'widen 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; narrow-to-region
;; narrow-to-page
;; upcase-region
;; downcase-region
;; list-threads
;; erase-buffer
;; scroll-left
;; dired-find-alternate-file
;; set-goal-column


;; Tooltips at cursor point on minibuffer
(defun display-help-echo-at-point ()
  "Display the 'help-echo' text property at cursor point in the minibuffer."
  (interactive)
  (let ((help-text (get-text-property (point) 'help-echo)))
    (if help-text
        (message "%s" help-text)
      (message "No help-echo at point"))))

(define-key (current-global-map) (kbd "s-. p") 'display-help-echo-at-point)


;; Add alternative keys for C-x and M-x
(define-key key-translation-map (kbd "s-<0x10081247> s-:") (kbd "C-x"))
(define-key key-translation-map (kbd "s-<0x10081247> s-¡") (kbd "M-x"))


;; Global lexical-binding (Emacs 31)
;; (set-default-toplevel-value 'lexical-binding t)

;; Enable Semantic Font Lock for Emacs (Emacs 31)
;; (setq elisp-fontify-semantically t)

;; Review policy (Emacs 31)
;; (setq package-review-policy t
;;      package-review-diff-command '("git" "diff" "--no-index" "--color=never" "--diff-filter=d"))
