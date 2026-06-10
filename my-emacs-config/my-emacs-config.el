;; Note to self: DO NOT USE M-s-s (The orca screen reader)

;; To invoke Hyper key: CTRL + x @ h

;; If you wonder what s-<0x10081247> is, it's the copilot key that comes with the new thinkpads.

(require 'cl-lib)

(use-package emacs
  :bind (:map global-map
              ("s-<0x10081247> s-D" . delete-pair)
              )
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
  ;; When scrolling preserve cursor position
  (scroll-preserve-screen-position t)
  ;; Short yes or no answer
  (use-short-answers t)
  ;; Use mark even when region is inactive
  (mark-even-if-inactive t)
  ;; Limit on depth in ‘eval’, ‘apply’ and ‘funcall’ before error.
  (max-lisp-eval-depth 1600)
  ;; Whether to ignore case when searching
  (case-fold-search nil) ;; For some reason setting t to this variable doesn't seem to work. At least setting 1 does work.
  ;; Undo limits
  (undo-limit 160000) ;(* 13 160000)
  (undo-strong-limit 240000) ;(* 13 240000)
  (undo-outer-limit 24000000) ;(* 13 24000000)
  ;; Disable fontification during user input to reduce lag in large buffers.
  (redisplay-skip-fontification-on-input t)
  (fill-column 80)
  ;; Optimize scrolling
  (fast-but-imprecise-scrolling t)
  ;; When deleting a pair, push mark at the end of the the delimited region
  (delete-pair-push-mark t)
  ;; When deleting a pair, do it immediatly
  (delete-pair-blink-delay 0)
  ;; Show boundaries around buffer edges
  (indicate-buffer-boundaries t)
  ;; Limit history length
  (history-length 150)
  ;; Delete duplicates in history variables
  (history-delete-duplicates t)
  ;; Only show the cursor in the selected window
  (cursor-in-non-selected-windows nil)
  ;; Only highlight active region in selected windows.
  (highlight-nonselected-windows nil)
  ;; Make the cursor noticeable
  (visible-cursor t)
  ;; Customize cursor
  (cursor-type t)
  ;; Default image scaling
  (image-scaling-factor 'auto)
  ;; Don't use the system font
  (font-use-system-font nil)
  ;; Resizing configuration
  (frame-resize-pixelwise t)
  (window-resize-pixelwise t)
  (mode-line-compact nil)

  :config
  ;; Disable compact font caches
  (setq-default inhibit-compacting-font-caches t)
  ;; Bidirectional editing config
  (setq-default bidi-display-reordering 'left-to-right)
  (setq-default bidi-paragraph-direction 'left-to-right)
  (if (version<= "27.1" emacs-version)
      (setq bidi-inhibit-bpa t))
  )



(use-package frame
  :custom
  ;; Change cursor's appearance
  (blink-cursor-mode t)
  (window-divider-default-places t)
  :config
  ;; Change font
  ;; Note that when Emacs is used inside a terminal (-nw) the font used is the same as the terminal's.
  (when (display-graphic-p)
    ;; (set-frame-font "JuliaMono Light 18" nil t)
    (set-frame-font "IoskeleyMonoTerm Nerd Font Light 18" nil t)
    )
  ;; Maximize Emacs on start
  (toggle-frame-maximized)
  ;; Add borders to windows
  (window-divider-mode +1)
  )

(use-package window
  :bind
  (:map global-map
        ;; Set key for burying buffers without deleting them
        ("C-x M-k" . bury-buffer)
        ;; Set key for swapping windows
        ("C-x M-o" . window-swap-states)
        )
  :custom
  (switch-to-buffer-obey-display-actions nil)
  (split-height-threshold nil)
  (split-width-threshold 80)
  :config
  (add-to-list 'display-buffer-alist
               '((or . ((derived-mode . occur-mode)
			(derived-mode . grep-mode)
			(derived-mode . Buffer-menu-mode)
			(derived-mode . log-view-mode)
			(derived-mode . help-mode)
			))
		 (display-buffer-reuse-mode-window display-buffer-below-selected)
		 (body-function . select-window)))
  )

;; Enable syntax highlighting
(use-package font-core
  :config
  (global-font-lock-mode t)
  )

(use-package font-lock
  :custom
  (font-lock-maximum-decoration t)
  )

;; Package for miscellaneous features
(use-package simple
  :bind (:map global-map
              ("H-x s-q" . indent-tabs-mode)
              ("H-x s-e" . delete-trailing-whitespace)
              ("H-w" . count-words)
              ("C-S-/". undo-only)
              ("s-<0x10081247> s-¡ p" . auto-fill-mode)
              ("s-<0x10081247> s-¡ d" . display-fill-column-indicator-mode)
              ("s-<0x10081247> s-¡ f" . fill-paragraph)
              ("H-s s c" . shell-command)
              )
  ;; Enable auto-save-mode only when the buffer is associated with a file.
  :hook ((after-change-major-mode . (lambda ()
                                      (if (buffer-file-name)
                                          (auto-save-mode +1)
                                        (auto-save-mode -1))))
         (special-mode . (lambda ()
                           (setq-local show-trailing-whitespace nil)
                           (setq-local truncate-lines nil)
                           ))
         ;; Delete all trailing-whitespaces when saving
         (before-save . my/delete-trailing-whitespace)
         )
  :custom
  (line-number-mode t)
  (column-number-mode t)
  (size-indication-mode t)
  (read-extended-command-predicate #'command-completion-default-include-p) ;; Other value(s): nil, transient-command-completion-not-suffix-only-p
  ;; Show character name in ‘what-cursor-position’
  (what-cursor-show-names t)
  (copy-region-blink-predicate #'region-indistinguishable-p); default: #'region-indistinguishable-p
  ;; The clipboard text is saved to the ‘kill-ring’ prior to any kill command
  (save-interprogram-paste-before-kill t)
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
  ;; Enable / disable truncated lines
  (truncate-lines nil)
  (word-wrap t) ;; Visual Line mode sets this variable to t
  (tab-width 8)
  ;; Show current directory when prompting for a shell command
  (shell-command-prompt-show-cwd t)
  ;; Show trailing whitespaces
  (show-trailing-whitespace t)
  ;; Delete trailing whitespaces
  (delete-trailing-lines t)

  :config
  ;; Use spaces for indentation
  (indent-tabs-mode -1)
  ;; Enable Transient Mark Mode
  (transient-mark-mode +1)

  (defun my/delete-trailing-whitespace ()
    "Call `delete-trailing-whitespace' unless the buffer is read-only."
    (unless buffer-read-only (delete-trailing-whitespace)))
  )

(use-package visual-wrap
  :custom
  (visual-wrap-extra-indent 0)
  :config
  (global-visual-wrap-prefix-mode +1)
  )

;; Show number of the lines
(use-package display-line-numbers
  :custom
  (display-line-numbers t)
  (display-line-numbers-width nil)
  (display-line-numbers-minor-tick 25)
  (display-line-numbers-major-tick 100)
  (display-line-numbers-width-start t)
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
  (tab-bar-history-mode -1)
  )

;; Tab Lines
(use-package tab-line
  :custom
  ;; tab-line-tab-name-buffer, tab-line-tab-name-truncated-buffer
  (tab-line-tab-name-function #'tab-line-tab-name-buffer)
  (tab-line-tab-name-truncated-max 20)
  (tab-line-new-button-show nil)
  (tab-line-close-button-show nil)
  ;; (tab-line-define-keys) ;; Emacs-31
  :config
  (setq tab-line-tab-name-ellipsis t)
  (add-to-list 'tab-line-tab-face-functions #'tab-line-tab-face-inactive-alternating)
  (global-tab-line-mode +1)
  )

;; Enable menu-bar
(use-package menu-bar
  :config
  (menu-bar-mode -1)
  )

;; Disable tool-bar
(use-package tool-bar
  :defer t
  :config
  (tool-bar-mode -1)
  (modifier-bar-mode -1)
  )

;; Enable tooltips
(use-package tooltip
  :config
  (tooltip-mode +1)
  )

;; Enable Vertical Scroll Bar
(use-package scroll-bar
  :config
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
  :custom
  (desktop-restore-eager t)
  (desktop-auto-save-timeout 30)
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
  (display-time-format "%H:%M %a %d-%m-%Y")
  :config
  (display-time-mode t)
  )

;; Bookmarks
(use-package bookmark
  :bind (:map global-map
              ("M-S-s-<0x10081247> M-s-K" . bookmark-jump-other-window)
              )
  :custom
  (bookmark-save-flag t)
  (bookmark-sort-flag t)
  )

;; Save minibuffer history. By default it will be on ~/.emacs.d/history
(use-package savehist
  :hook (savehist-save-hook . (lambda ()
                                (setq kill-ring
                                      (mapcar #'substring-no-properties
                                              (cl-remove-if-not #'stringp kill-ring)))))
  :custom
  (savehist-save-minibuffer-history t)
  (savehist-file (concat user-emacs-directory "savehist"))
  (savehist-additional-variables '((kill-ring . 1)
                                   (search-ring . 1)
                                   (regexp-search-ring . 1)
                                   (register-alist . 10)
                                   ;; (mark-ring . 1)
                                   ;; (global-mark-ring . 1)
                                   ))
  (savehist-ignored-variables '(recentf-list))
  :config
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
  (completion-auto-help t)
  (completions-max-height nil)
  (completion-cycle-threshold nil)
  (completions-format 'horizontal)
  (completions-group nil)
  (completion-auto-select nil)
  (minibuffer-visible-completions nil)
  (completion-eager-update t) ;; Emacs-31
  (completion-eager-display t) ;; Emacs-31
  )

;; Enable right click menu
(use-package mouse
  :config
  (context-menu-mode +1)
  )

(use-package vc
  :defer t
  :custom
  (vc-follow-symlinks 'ask)
  (vc-git-diff-switches '("--histogram")) ;; default: t
  (vc-git-print-log-follow nil)
  (vc-make-backup-files nil)
  (vc-command-messages nil)
  (vc-find-revision-no-save nil)
  ;; Deduce vc on all buffers
  (vc-deduce-backend-nonvc-modes t) ;; default: '(dired-mode shell-mode eshell-mode compilation-mode)
  :config
  (setq vc-log-short-style '(directory file))
  )

(use-package vc-dir
  :defer t
  :bind (:map vc-dir-mode-map
              ("r" . vc-dir-refresh)
              )
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
  (diff-font-lock-syntax nil) ;; default: t
  (diff-font-lock-prettify t)
  (diff-switches '("-u")) ;--color=never
  :config
  (setq diff-use-changed-face t)
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
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-documentation-strategy #'eldoc-documentation-default)
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
  (native-comp-async-report-warnings-errors t)
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
  (dired-clean-confirm-killing-deleted-buffers t)
  (dired-auto-revert-buffer nil)
  (dired-clean-up-buffers-too t)
  :config
  (setq dired-deletion-confirmer #'yes-or-no-p) ;#'y-or-n-p
  )

(use-package find-dired
  :bind (:map global-map
              ("H-f d" . find-name-dired))
  )

(use-package wdired
  :defer t
  :custom
  (wdired-create-parent-directories t)
  )

(use-package dired-aux
  :defer t
  :bind (:map dired-mode-map
              ("s-w" . dired-do-async-shell-command))
  :custom
  (dired-create-destination-dirs 'ask)
  (dired-create-destination-dirs-on-trailing-dirsep t)
  )

;; grep config
(use-package grep
  ;; Set key for grep
  :bind (:map global-map
              ("H-x g" . grep))
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
  :defer t
  )

(use-package register
  :config
  (defun my/clear-register ()
    "Remove all elements in `register-alist'"
    (interactive)
    (setq register-alist nil))
  )

(use-package jit-lock
  :custom
  (jit-lock-defer-time nil) ;; default: nil
  )

(use-package descr-text
  :bind (:map global-map
              ("C-h M-c" . describe-char)
              )
  :custom
  ;; Control how you want to show info using `what-cursor-position'
  (describe-char-unidata-list t)
  )

;; Calc config
(use-package calc
  :defer t
  :bind (:map global-map
              ;; Open full-calc
              ("<Calculator>" . full-calc)
              )
  :init
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

(use-package completion-preview
  :bind (:map completion-preview-active-mode-map
              ("s-<up>" . completion-preview-prev-candidate)
              ("s-<down>" . completion-preview-next-candidate)
              )
  :custom
  ;; The time before `completion-preview' appears.
  (completion-preview-idle-delay 0.5)
  ;; The minimum amount of letters needed for `completion-preview' to appear.
  (completion-preview-minimum-symbol-length 2)
  :init
  (global-completion-preview-mode)
  )

(use-package isearch
  ;; HACK: instead of setting case-fold-search to nil, which also affects regex,
  ;; I set isearch-case-fold-search to yes or nil when starting or ending
  ;; isearch-mode.
  :hook ((isearch-mode . (lambda ()
                           (setq-local isearch-case-fold-search 'yes)))
         (isearch-mode-end . (lambda ()
                               (setq-local isearch-case-fold-search nil))))
  :custom
  (isearch-lax-whitespace t)
  (isearch-regexp-lax-whitespace nil)
  ;; Count the number of instances and show that number on the minibuffer.
  (isearch-lazy-count t)
  (lazy-count-prefix-format "(%s/%s) ")
  (lazy-count-suffix-format " (%s/%s)")
  (isearch-lazy-highlight t)
  (isearch-wrap-pause t)
  (lazy-highlight-initial-delay 0.25)
  (lazy-highlight-no-delay-length 3)
  (isearch-allow-scroll nil)
  (isearch-allow-motion nil)
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
  (save-place-forget-unreadable-files t)
  :config
  (save-place-mode +1)
  (advice-add 'save-place-find-file-hook :after
              (lambda (&rest _)
                (when buffer-file-name (ignore-errors (recenter)))))
  )

(use-package replace
  :bind (:map global-map
              ("H-r" . replace-string)
              ;; Replace string
              ("M-s M-r s" . query-replace)
              ;; Replace regexp
              ("M-s M-r r" . query-replace-regexp)
              ;; Delete lines containing a specific string
              ("M-°" . flush-lines)
              ))

;; Backup config. Instead of automatically generating backup files, choose when and where to generate them.
(use-package files
  ;; :hook (after-save-hook . executable-make-buffer-file-executable-if-script-p)
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
  (auto-save-visited-interval 480)
  ;; Confirm killing processes on exit
  (confirm-kill-processes t)
  (view-read-only t)
  (remote-file-name-inhibit-delete-by-moving-to-trash t)
  :config
  ;; var: backup-inhibited

  (defun my/force-backup-of-file ()
    (interactive)
    (setq buffer-backed-up nil))

  (defun my/enable-or-disable-backups ()
    (interactive)
    (if make-backup-files
        (progn
          (setq make-backup-files nil)
          (message "Disabling backups."))
      (progn
        (setq make-backup-files t)
        (message "Enabling backups."))))

  (define-key (current-global-map) (kbd "s-<0x10081247> s-B f") 'my/force-backup-of-file)
  (define-key (current-global-map) (kbd "s-<0x10081247> s-B b") 'my/enable-or-disable-backups)

  (auto-save-visited-mode -1)
  )

(use-package message
  :custom
  (message-kill-buffer-query t)
  )

(use-package page
  :bind (:map global-map
              ;; To use this, use C-q C-l to put form-feeds on the buffers.
              ("s-<prior>" . backward-page)
              ("s-<next>" . forward-page)
              )
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

(use-package nsm
  :defer t
  :custom
  (network-security-level 'high)
  )

(use-package cus-edit
  :custom
  ;; Exiting a custom buffer kills it
  (custom-buffer-done-kill t)
  ;; Use real function / variables names when customizing
  (custom-unlispify-tag-names nil)
  (custom-raised-buttons t)
  )

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'post-forward-angle-brackets)
  (uniquify-after-kill-buffer-p t)
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
  (help-window-select t)
  ;; Reuse Help window in contexts other than another Help buffer
  (help-window-keep-selected t)
  )

(use-package ispell
  :defer t
  ;; :if (executable-find "aspell")
  :custom
  (ispell-silently-savep t)
  (ispell-help-in-bufferp 'electric)
  (ispell-program-name "aspell") ;; alt: hunspell
  (ispell-extra-args nil) ;'("--sug-mode=ultra" "--lang=en_US" "--dont-run-together")

  ;; Doesn't seem to work on NixOS
  ;; (ispell-dictionary "en_US")

  :init
  ;; This sets the starting language of aspell.
  (setenv "ASPELL_CONF" "lang en_US" t)
  ;; If you want to use a different language you need to first change the
  ;; environment variable and then restart aspell.
  ;; (ispell-kill-ispell)
  )

(use-package flyspell
  :defer t
  ;; :if (executable-find "aspell")
  :bind (:map global-map
              ("s-<0x10081247> s-F f" . flyspell-mode)
              ("s-<0x10081247> s-F p" . flyspell-prog-mode)
              )
  :custom
  (flyspell-issue-message-flag t)
  (flyspell-issue-welcome-flag t)
  )

(use-package text-mode
  :defer t
  :custom
  (text-mode-ispell-word-completion nil)
  )

;; Commands to repeat other commands.
(use-package repeat
  :defer t
  :bind (:map global-map
              ("s-<" . repeat)
              ("s->" . repeat-mode)
              ("s-z" . describe-repeat-maps)
              )
  :commands (repeat repeat-mode describe-repeat-maps)
  )

(use-package python
  :defer t
  :bind ((:map python-mode-map
               ("s-<0x10081247> s-¡ f" . python-fill-paragraph)
               )
	 (:map python-ts-mode-map
	       ("s-<0x10081247> s-¡ f" . python-fill-paragraph)
	       )
	 )
  :custom
  (python-fill-docstring-style 'pep-257)
  )

(use-package etags-regen
  :defer t
  :config
  (etags-regen-mode -1)
  )

(use-package cc-mode
  :defer t
  :custom
  ;; https://superuser.com/questions/23552/how-do-i-make-c-basic-offset-stick-in-emacs
  (c-basic-offset 4)
  :config
  (define-key c-mode-map (kbd "s-d s") 'disaster)
  (define-key c++-mode-map (kbd "s-d s") 'disaster)
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
     ligature
     buffer-to-pdf
     git-modes
     python-coverage
     python-pytest
     consult-flyspell
     clj-refactor
     form-feed
     lin
     puni
     consult-symbol
     helpful
     vscode-dark-plus-theme
     package-build
     package-lint
     package-lint-flymake
     guava-themes
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
     ))
  (package-menu-use-current-if-no-marks t)
  )

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
          shell-mode
          term-mode
          comint-mode
          cfrs-input-mode
          image-mode
	  magit-diff-mode
          diff-mode
          )
         ;; Disable hl-line for some modes
         . (lambda () (setq-local global-hl-line-mode nil)))
  :custom
  (global-hl-line-sticky-flag nil)
  :config
  (global-hl-line-mode t)
  )

;; Project manager
(use-package project
  ;; Use demand to load the package automatically
  :demand t
  :bind (:map global-map
              ("s-<0x10081247> s-?" . project-other-window-command)
              )
  :custom
  ;; Show project name on mode-line
  (project-mode-line t)
  (project-vc-extra-root-markers
   '(
     ".dir-locals.el"
     ".envrc"
     ".editorconfig"
     "flake.nix"
     "shell.nix"
     ))
  :config
  (define-key key-translation-map (kbd "s-<0x10081247> s-P") (kbd "C-x p"))
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
  (electric-indent-mode 1)
  )

;; Automatically show available commands
(use-package which-key
  :ensure nil
  :custom
  (which-key-idle-delay 5.0)
  :config
  (which-key-setup-side-window-right-bottom)
  ;; `prefix-help-command' becomes `which-key-C-h-dispatch'
  (which-key-mode)
  )

(use-package shr
  :custom
  (shr-use-colors t)
  (shr-use-fonts t)
  (shr-max-image-proportion 0.4)
  )

(use-package url-queue
  :custom
  (url-queue-timeout 30)
  )

(use-package shell
  ;; Set keys for shell
  :bind (:map global-map
              ("H-s s b" . shell)
              ))

(use-package eshell
  ;; Set keys for eshell
  :bind (:map global-map
              ("H-s e b" . eshell)
              ("H-s e c" . eshell-command)
              ))

(use-package term
  ;; Set keys for term
  :bind (:map global-map
              ("H-s t b" . term)
              ))

(use-package sort
  :bind (:map global-map
              ;; Delete duplicate lines using Hyper + ALT + <backspace>
              ("H-M-<backspace>" . delete-duplicate-lines)
	      ;; Alphabetically sort lines in region.
              ("H-x s" . sort-lines)
              ))

(use-package ibuffer
  :bind (:map global-map
              ;; Define key for ibuffer
              ("C-x M-b" . ibuffer)
              ))

(use-package tabify
  :bind (:map global-map
              ;; Convert tabs to spaces or spaces to tabs
              ("H-x s-<tab>" . tabify)
              ("H-x s-<backspace>" . untabify)
              ("H-x s-<iso-lefttab>" . untabify)
              ))

(use-package find-func
  :bind (:map global-map
              ("C-h M-l" . find-library)
              ))

(use-package help-fns
  :bind (:map global-map
              ("C-h M-k" . describe-keymap)
              ("C-h M-f" . describe-face)
              )
  )

;; Abbrev config
(use-package abbrev
  :custom
  (abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))
  (save-abbrevs 'silently)
  )

;; Dabbrev config
(use-package dabbrev
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)
         ("C-M-<Ungrab>" . dabbrev-expand)
	 ("C-s-<kp-divide>" . dabbrev-completion)
	 )
  :custom
  (dabbrev-upcase-means-case-search t)
  (dabbrev-case-fold-search 'case-fold-search)
  :config
  ;; (dabbrev-ignored-buffer-regexps
  ;;     '(;; - Buffers starting with a space (internal or temporary buffers)
  ;;       "\\` "
  ;;       ;; Tags files such as ETAGS, GTAGS, RTAGS, TAGS, e?tags, and GPATH,
  ;;       ;; including versions with numeric extensions like <123>
  ;;       "\\(?:\\(?:[EG]?\\|GR\\)TAGS\\|e?tags\\|GPATH\\)\\(<[0-9]+>\\)?"))
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\(?:\\(?:[EG]?\\|GR\\)TAGS\\|e?tags\\|GPATH\\)\\(<[0-9]+>\\)?")
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode)
  )



(use-package flymake
  :hook
  (((
     prog-mode
     eglot--managed-mode
     ) . flymake-mode))
  :bind (:map flymake-mode-map
              ("H-f l" . flymake-switch-to-log-buffer)
              ("H-f f" . flymake-show-buffer-diagnostics)
              ("H-f p" . flymake-show-project-diagnostics)
              )
  :custom
  (flymake-no-changes-timeout 0.5)
  (flymake-fringe-indicator-position 'left-fringe)
  (flymake-margin-indicator-position 'left-margin)
  (flymake-show-diagnostics-at-end-of-line t)
  (flymake-wrap-around t)
  (flymake-start-on-flymake-mode t)
  (flymake-start-on-save-buffer t)
  )



;; Org configuration
(use-package org
  :ensure nil
  :defer t
  :custom
  (org-startup-indented nil)
  (org-startup-folded 'overview)
  (org-persist-directory (expand-file-name ".cache/org-persist/" user-emacs-directory))
  (org-imenu-depth 8)
  (org-startup-truncated t)
  (org-fontify-todo-headline t)
  (org-fontify-done-headline t)
  (org-deadline-warning-days 35)
  ;;(org-return-follows-link nil)
  ;;(org-hide-emphasis-markers nil)
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
  ;;(org-refile-targets nil) ;;'((org-agenda-files . (:maxlevel . 3))))
  ;;(org-refile-use-outline-path nil);; 'file)
  ;;(org-outline-path-complete-in-steps t)
  ;;(org-refile-allow-creating-parent-nodes nil);;'confirm)
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

(use-package org-agenda
  :bind (:map global-map
              ("s-<0x10081247> s-U u" . org-agenda)
              ("s-<0x10081247> s-U l" . org-agenda-list)
              )
  :custom
  (org-agenda-include-diary t)
  (org-agenda-files '("~/.emacs.d/org-agenda/"))
  (org-agenda-start-on-weekday 1)
  (org-agenda-span 'month)
  (org-agenda-skip-scheduled-if-done nil)
  (org-agenda-skip-deadline-if-done nil)
  (org-agenda-show-future-repeats t)
  (org-agenda-inhibit-startup nil)
  (org-agenda-use-time-grid t)
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
  ;; To install all the grammars at once use this:
  ;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
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

;; Treesitter modes

(use-package json-ts-mode
  :defer t
  :config
  (require 'json-mode)
  )



;;;; eglot configuration
(use-package eglot
  :ensure nil
  :defer t
  :preface
  ;; https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc
  (defun my/mp-eglot-eldoc ()
    (setq eldoc-documentation-strategy
          'eldoc-documentation-compose-eagerly))
  :hook
  (eglot-managed-mode . my/mp-eglot-eldoc)
  ((nix-mode nix-ts-mode) . eglot-ensure)
  ((rust-mode rust-ts-mode) . eglot-ensure)
  ((java-mode java-ts-mode) . eglot-ensure)
  ((js-mode js-ts-mode) . eglot-ensure)
  ((python-mode python-ts-mode) . eglot-ensure)
  ((toml-mode toml-ts-mode) . eglot-ensure)
  ((yaml-mode yaml-ts-mode) . eglot-ensure)
  ((json-mode json-ts-mode) . eglot-ensure)
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
              ("s-e f m" . eglot-format)
              ("s-e f b" . eglot-format-buffer)
              ("s-e r n" . eglot-rename)
              ("s-e r w" . eglot-code-action-rewrite)
              ("s-e r c" . eglot-reconnect)
              )
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-edits '((eglot-rename . nil) (t . maybe-summary) (t . diff)))
  ;; async
  (eglot-sync-connect 0) ;; default: 3
  (eglot-connect-timeout 30)
  (eglot-ignored-server-capabilities nil) ;; examples: '(:inlayHintProvider), '(:documentHighlightProvider), '(:documentFormattingProvider :documentRangeFormattingProvider :documentOnTypeFormattingProvider :colorProvider :foldingRangeProvider), '(:hoverProvider :documentHighlightProvider :documentFormattingProvider :documentRangeFormattingProvider :documentOnTypeFormattingProvider :colorProvider :foldingRangeProvider)
  (eglot-events-buffer-config '(:size 2000000 :format full))
  (eglot-extend-to-xref t)
  (eglot-send-changes-idle-time 0.5)
  (eglot-report-progress t)
  :config
  ;; Enable cache busting, depending on if your server returns
  ;; sufficiently many candidates in the first place.
  ;; https://github.com/minad/corfu/wiki#continuously-update-the-candidates
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
                 ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))

  ;; You need a Java compiler to use the lsp server JDTLS.
  ;; You also need to concat the absolute path, not relative.
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

  ;; Alternative code (probably used as project-specific configuration):
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

  (add-to-list 'eglot-server-programs
               '((python-ts-mode python-mode) .
                 ("rass" "--" "ty" "server" "--" "ruff" "server")))

  (add-to-list 'eglot-server-programs
               '((toml-ts-mode toml-mode) .
                 ("tombi" "lsp"))) ;; "lint" "format" "completion"

  ;; (add-to-list 'eglot-server-programs
  ;;              '((c++-mode c-mode) .
  ;;                ("clangd")))

  (add-to-list 'eglot-server-programs
               '((nix-mode) .
                 ("nixd"
                  :initializationOptions
                  (:settings
                   (:nixd
                    (:formatting
                     (:command "nixfmt")
                     )
                    )
                   )
                  )))

  (add-to-list 'eglot-server-programs
               '((yaml-mode yaml-ts-mode) .
                 ("yaml-language-server" "--stdio")))
  )



;;;; Miscellaneous

;; ;; Increase zoom
;; (setq default-text-scale-mode-amount 2)
;;
;; (setq forbidden-prefixes-text-scale-mode '("dape" "dashboard-mode"))
;;
;; (add-hook 'after-change-major-mode-hook
;; 	  (lambda ()
;; 	    ;; (if (derived-mode-p 'treemacs-mode)
;;             (unless (cl-some (lambda (prefix)
;;                                (string-prefix-p prefix (symbol-name major-mode)))
;;                              forbidden-prefixes-text-scale-mode)
;;               ;; (text-scale-set 0)
;; 	      (text-scale-set default-text-scale-mode-amount))
;; 	    ))
;;
;; ;; Reduce text size when there are more than two windows
;; (advice-add 'split-window-right :after #'(lambda (&rest _)
;;                                            (setq default-text-scale-mode-amount 0)
;;                                            (text-scale-set default-text-scale-mode-amount)))
;;
;; (advice-add 'split-window-below :after #'(lambda (&rest _)
;;                                            (setq default-text-scale-mode-amount 0)
;;                                            (text-scale-set default-text-scale-mode-amount)))
;;
;; (advice-add 'delete-window :after #'(lambda (&rest _)
;;                                       (when (<= (length (window-list)) 1)
;;                                         (setq default-text-scale-mode-amount 2)
;;                                         (text-scale-set default-text-scale-mode-amount)
;;                                         )))
;;
;; (advice-add 'delete-other-windows :after #'(lambda (&rest _)
;;                                              (setq default-text-scale-mode-amount 2)
;;                                              (text-scale-set default-text-scale-mode-amount)))


;; Enable commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'widen 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; list-threads
;; erase-buffer
;; scroll-left
;; dired-find-alternate-file
;; set-goal-column


;; Tooltips at cursor point on minibuffer
(defun my/display-help-echo-at-point ()
  "Display the 'help-echo' text property at cursor point in the minibuffer."
  (interactive)
  (let ((help-text (get-text-property (point) 'help-echo)))
    (if help-text
        (message "%s" help-text)
      (message "No help-echo at point"))))

(define-key (current-global-map) (kbd "s-. p") 'my/display-help-echo-at-point)
(define-key (current-global-map) (kbd "s-. s-p") 'my/display-help-echo-at-point)


;; Display face used at point.
(defun my/display-face-property-at-point ()
  "Display the 'face' text property at cursor point on another window."
  (interactive)
  (let ((face-property (get-text-property (point) 'face)))
    (if face-property
        (describe-face face-property)
      (message "No 'face' text property at point"))))

(define-key (current-global-map) (kbd "s-<0x10081247> s-I") 'my/display-face-property-at-point)


;; Add alternative key for C-x
(define-key key-translation-map (kbd "s-<0x10081247> s-:") (kbd "C-x"))


(defun my/delete-lines-without-specific-string (STR)
  "Delete all lines in the buffer that do not contain the string STR."
  (interactive "sEnter string: ")
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (if (not (search-forward STR (line-end-position) t))
          (delete-region (line-beginning-position) (line-end-position)))
      (forward-line 1))))


(defun my/delete-duplicate-lines-in-directory (dir)
  "Non-recursively remove duplicate lines in all files under DIR."
  (interactive "DDirectory: ")
  (let ((files (directory-files dir t "^[^.].*")))
    (dolist (file files)
      (when (file-regular-p file)
        (with-current-buffer (find-file-noselect file)
          (delete-duplicate-lines (point-min) (point-max))
          (save-buffer)
          (kill-buffer))))))


;; Global lexical-binding (Emacs-31)
;; (set-default-toplevel-value 'lexical-binding t)

;; Enable Semantic Font Lock for Emacs (Emacs-31)
;; (setq elisp-fontify-semantically t)

;; Review policy (Emacs-31)
;; (setq package-review-policy t
;;      package-review-diff-command '("git" "diff" "--no-index" "--color=never" "--diff-filter=d"))

;; Like 'global-auto-revert-mode' but limited to VCS-tracked files (Emacs-31)
;; (vc-auto-revert-mode)

;; Ask to save relevant vc buffers (Emacs-31)
;; (setopt vc-dir-save-some-buffers-on-revert t)

;; C-x v I' and 'C-x v O' become prefix commands (Emacs-31)
;; (setopt vc-use-incoming-outgoing-prefixes t)
