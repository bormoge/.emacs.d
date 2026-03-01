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

  :config
  ;; Disable compact font caches
  (setq inhibit-compacting-font-caches t)
  ;; Change default cursor type
  (setq-default cursor-type t)
  ;; (setq-default cursor-type '(bar . 7)) ;; default: t
  ;; Default image scaling
  (setq-default image-scaling-factor 'auto)
  )

;; Maximize Emacs on start
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))
;;(toggle-frame-fullscreen)
(toggle-frame-maximized)

;; Change font
;; (set-frame-font "Adwaita Mono 12" nil t)
;; (set-frame-font "JetBrainsMono Nerd Font Mono 12" nil t)
;; (set-frame-font "Hack Nerd Font Mono 12" nil t)
;; (set-frame-font "Iosevka Nerd Font Mono 14" nil t)
;; (set-frame-font "FiraCode Nerd Font Mono Light 12" nil t)
;; (set-frame-font "Ioskeley Mono Light 12" nil t)
(set-frame-font "Inconsolata Nerd Font Mono 14" nil t)

;; Enable syntax highlighting
(global-font-lock-mode t)

;; Bidirectional editing config
(setq-default bidi-display-reordering 'left-to-right)
(setq-default bidi-paragraph-direction 'left-to-right)
(if (version<= "27.1" emacs-version)
    (setq bidi-inhibit-bpa t))

;; Package for miscellaneous features
(use-package simple
  :custom
  (line-number-mode t)
  (column-number-mode t)
  (size-indication-mode t)
  (read-extended-command-predicate #'command-completion-default-include-p) ;; Other value(s): nil, transient-command-completion-not-suffix-only-p
  ;; Show character name in ‘what-cursor-position’
  (what-cursor-show-names t)
  (copy-region-blink-predicate #'region-indistinguishable-p); default: #'region-indistinguishable-p
  ;;(save-interprogram-paste-before-kill t)
  (kill-whole-line t)
  (line-move-visual t)
  (set-mark-command-repeat-pop t)
  :config
  ;; Truncate long lines
  ;;(setq-default truncate-lines t)
  (global-visual-line-mode t)
  ;; Use spaces for indentation
  ;; Alternatively, you can modify the variable `tab-width'
  (setq-default indent-tabs-mode nil)
  ;; Show trailing whitespaces
  (setq-default show-trailing-whitespace t)
  ;; (add-hook 'before-save-hook #'delete-trailing-whitespace)
  ;; Auto-Save-Mode
  (auto-save-mode +1)
  )

;; Show number of the lines
(global-display-line-numbers-mode +1)
(setq display-line-numbers-width nil)

;; Enable use of system clipboard
(setq select-enable-clipboard t)

;; Tab Bars
(tab-bar-mode +1)
(setq tab-bar-history-mode nil)
(setq tab-bar-auto-width-max '((300) 30))
;; tab-bar-tab-name-current, tab-bar-tab-name-current-with-count, tab-bar-tab-name-truncated, tab-bar-tab-name-all
(setq tab-bar-tab-name-function #'tab-bar-tab-name-current)
(setq tab-bar-tab-name-truncated-max 20)
(setq tab-bar-tab-name-ellipsis t)
;;(setq tab-bar-auto-width nil)

;; Tab Lines
(global-tab-line-mode +1)
;; tab-line-tab-name-buffer, tab-line-tab-name-truncated-buffer
(setq tab-line-tab-name-function #'tab-line-tab-name-buffer)
(setq tab-line-tab-name-truncated-max 20)
(setq tab-line-tab-name-ellipsis t)

;; Enable menus
(menu-bar-mode +1)
(tool-bar-mode -1)
;;(modifier-bar-mode t)

;; Enable tooltips
(tooltip-mode +1)

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

;; Enable Vertical Scroll Bar
(scroll-bar-mode 'right)

;; Replace a selected area with typed text
(use-package delsel
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
(setq warning-minimum-level :warning)

;; Disables docstring warnings
(setq byte-compile-warnings
      '(not docstrings))

;; Display time and date on the minibuffer (neat stuff)
(setq display-time-day-and-date t)
(setq display-time-default-load-average nil)
(setq display-time-format "%I:%M %a %d-%m-%Y")
(display-time-mode t)

;; Save minibuffer history. By default it will be on ~/.emacs.d/history
(use-package savehist
  :defer t
  :custom
  (savehist-save-minibuffer-history t)
  (savehist-file (concat user-emacs-directory "history"))
  ;;(savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (savehist-additional-variables '((kill-ring . 20) search-ring regexp-search-ring)) ;;(recentf-list . 50)
  :config
  (setq history-length 150)
  (setq history-delete-duplicates t)
  (savehist-mode +1)
  )

;; Modify the appearance of the region
;; (custom-set-faces '(region ((t :extend t)))) ;; Use ':extend t' or ':extend nil' to modify if region covers entire line.

;; Change cursor's appearance
(setq blink-cursor-mode t)

;; Auto-refresh buffers. If a file was changed on disk, revert changes on buffer.
(use-package autorevert
  :custom
  (global-auto-revert-ignore-modes '(doc-view-mode pdf-view-mode))
  (auto-revert-remote-files nil)
  (auto-revert-verbose t)
  (auto-revert-interval 5)
  (auto-revert-avoid-polling nil)
  (global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode +1)
  )

;; Scroll settings
(setq scroll-conservatively 100)
(setq scroll-margin 6)
(setq scroll-step 1)
(setq scroll-preserve-screen-position nil)

;; Config for vertico package
(setq completion-in-region-function #'consult-completion-in-region) ;;default: #'completion--in-region

;; Enable right click menu
(context-menu-mode +1)

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
  (ediff-split-window-function 'split-window-vertically)
  (ediff-window-setup-function 'ediff-setup-windows-plain) ;;default: 'ediff-setup-windows-default
  )

;; More detailed completions
(setq completions-detailed t)

;; Use real function / variables names when customizing
(setq custom-unlispify-tag-names nil)

;; When created, change focus to Help buffer
(setopt help-window-select nil)

;; Reuse Help window in contexts other than another Help buffer
(setopt help-window-keep-selected t)

;; EditorConfig (https://editorconfig.org/)
;; You can also use .dir-locals.el and .dir-locals-2.el, both alongside and as alternatives to .editorconfig files
(editorconfig-mode t)

;; Short yes or no answer
(setq use-short-answers t)

;; Enable Transient Mark Mode
(transient-mark-mode +1)

;; ElDoc config
(setq eldoc-idle-delay 0.5)
(setq eldoc-echo-area-display-truncation-message nil)
(global-eldoc-mode +1)

;; Enable commands
(put 'narrow-to-region 'disabled nil)
(put 'widen 'disabled nil)

;; Display battery status
(display-battery-mode +1)

;; Display name of a "function" (depends of the context)
(setq which-func-update-delay 0.5)
(setq which-func-display 'header)
(which-function-mode +1)

;; Follow the compilation buffer
(use-package compile
  ;; Add key for compile
  :bind (:map global-map
              ("s-<0x10081247> s-c" . compile))
  :custom
  (compilation-scroll-output t)
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
  :config
  (setq calc-group-digits t)
  )

;; Highlight pairs of parentheses
(use-package paren
  :custom
  (show-paren-style 'mixed) ;; default: 'parenthesis
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :config
  (show-paren-mode +1)
  )

(use-package vc
  :defer t
  :custom
  (vc-follow-symlinks 'ask)
  (vc-git-diff-switches '("-u" "--histogram")) ;; default: t
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
  :bind
  (:map recentf-mode-map
        ("s-<0x10081247> s-r" . recentf-open))
  :config
  (recentf-mode +1)
  )

;; Backup config. Instead of automatically generating backup files, choose when and where to generate them.
(use-package files
  :custom
  ;; If nil, disable backups
  (make-backup-files nil)
  ;; Whether to create auto-save files or not
  (auto-save-default t)
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

  (define-key (current-global-map) (kbd "s-<0x10081247> s-b f") 'force-backup-of-file)
  (define-key (current-global-map) (kbd "s-<0x10081247> s-b b") 'enable-or-disable-backups)

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

;; Global lexical-binding (Emacs 31)
;; (set-default-toplevel-value 'lexical-binding t)

;; Enable Semantic Font Lock for Emacs (Emacs 31)
;; (setq elisp-fontify-semantically t)
