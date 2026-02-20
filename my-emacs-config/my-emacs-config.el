(require 'cl-lib)

;; Remove startup screen
(setq inhibit-startup-screen t)

;; Maximize Emacs on start
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Enable syntax highlighting
(global-font-lock-mode t)

;; Bidirectional editing config
(setq-default bidi-display-reordering 'left-to-right)
(setq-default bidi-paragraph-direction 'left-to-right)
(if (version<= "27.1" emacs-version)
    (setq bidi-inhibit-bpa t))

;; Disable compact font caches
(setq inhibit-compacting-font-caches t)

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
  :config
  ;; Truncate long lines
  ;;(setq-default truncate-lines t)
  (global-visual-line-mode t)

  ;; Use spaces for indentation
  ;; Alternatively, you can modify the variable `tab-width'
  (setq-default indent-tabs-mode nil)

  ;; (add-hook 'before-save-hook #'delete-trailing-whitespace)
  )

;; Show number of the lines
(global-display-line-numbers-mode 1)
(setq display-line-numbers-width nil)

;; Enable use of system clipboard
(setq select-enable-clipboard t)

;; Disable use of system trash
(setq delete-by-moving-to-trash nil)

;; Tab Bars
(tab-bar-mode t)
(setq tab-bar-history-mode nil)
(setq tab-bar-auto-width-max '((300) 30))
;; tab-bar-tab-name-current, tab-bar-tab-name-current-with-count, tab-bar-tab-name-truncated, tab-bar-tab-name-all
(setq tab-bar-tab-name-function #'tab-bar-tab-name-current)
(setq tab-bar-tab-name-truncated-max 20)
(setq tab-bar-tab-name-ellipsis t)
;;(setq tab-bar-auto-width nil)

;; Tab Lines
(global-tab-line-mode t)
;; tab-line-tab-name-buffer, tab-line-tab-name-truncated-buffer
(setq tab-line-tab-name-function #'tab-line-tab-name-buffer)
(setq tab-line-tab-name-truncated-max 20)
(setq tab-line-tab-name-ellipsis t)

;; Enable menus
(menu-bar-mode t)
(tool-bar-mode 0)
;;(modifier-bar-mode t)

;; Enable tooltips
(tooltip-mode t)

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

;; Change font
;; (set-frame-font "Adwaita Mono 12" nil t)
;; (set-frame-font "JetBrainsMono Nerd Font Mono 12" nil t)
;; (set-frame-font "Hack Nerd Font Mono 12" nil t)
;; (set-frame-font "Iosevka Nerd Font Mono 14" nil t)
;; (set-frame-font "FiraCode Nerd Font Mono Light 12" nil t)
;; (set-frame-font "Ioskeley Mono Light 12" nil t)
(set-frame-font "Inconsolata Nerd Font Mono 14" nil t)

;; Enable Vertical Scroll Bar
(scroll-bar-mode 'right)

;; Enable Horizontal Scroll Bar
;;(horizontal-scroll-bar-mode t)

;; Replace a selected area with typed text
(setopt delete-selection-mode t)

;; Add directory to desktop-path
(with-eval-after-load 'desktop
  (add-to-list 'desktop-path (expand-file-name "desktop-sessions/" user-emacs-directory))
  (setq desktop-dirname (expand-file-name "desktop-sessions/" user-emacs-directory)))

;; Turn off clickable text highlight
(setq mouse-highlight nil)

;; Display warnings depending of level
(setq warning-minimum-level :warning)

;; Nice to have if you are dealing with packages. Disables docstring warnings
(setq byte-compile-warnings
      '(not docstrings))

;; Display time and date on the minibuffer (neat stuff)
(setq display-time-day-and-date t)
(setq display-time-default-load-average nil)
(setq display-time-format "%I:%M %a %d-%m-%Y")
(display-time-mode t)

;; I'm going to disable savehist for now to see if it improves or worsens my workflow.

;; Save minibuffer history. By default it will be on ~/.emacs.d/history
;; (savehist-mode)
;; (setq savehist-file (concat user-emacs-directory "history")
;;   history-length 150
;;   history-delete-duplicates t
;;   savehist-save-minibuffer-history t
;;   ;;savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
;;   ;;save-interprogram-paste-before-kill t
;;   )

;; Modify the appearance of the region
;; (custom-set-faces '(region ((t :extend t)))) ;; Use ':extend t' or ':extend nil' to modify if region covers entire line.

;; Change cursor's appearance
(setq blink-cursor-mode t)
(set-default 'cursor-type t)
;; (set-default 'cursor-type '(bar . 7)) ;; default: t

;; Auto-refresh buffers. If a file was changed on disk, revert changes on buffer.
(setq global-auto-revert-ignore-modes '(doc-view-mode pdf-view-mode))
(setq auto-revert-remote-files nil)
(setq auto-revert-verbose t)
(setq auto-revert-interval 5)
(setq auto-revert-avoid-polling nil)
(setq global-auto-revert-non-file-buffers nil)
(global-auto-revert-mode t) ;; Nil by default

;; Scroll settings
(setq scroll-conservatively 100)
(setq scroll-margin 6)
(setq scroll-step 1)
(setq scroll-preserve-screen-position nil)

;; Config for vertico package
(setq completion-in-region-function #'consult-completion-in-region) ;;default: #'completion--in-region
(setq enable-recursive-minibuffers t)
(setq minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)) ;; Original value: (read-only t face minibuffer-prompt)

;; Enable right click menu
(context-menu-mode t)

;; Always generate an empty line at the end of the file
(setq require-final-newline t)

;; Display differences between files / buffers
(use-package diff
  :defer t
  :custom
  (diff-font-lock-syntax 'hunk-also) ;;default: t
  (diff-font-lock-prettify t)
  (diff-switches "-u")
  ;; (diff-switches '("--color=never"))
  )

;; Interactively display differences between files / buffers
(use-package ediff
  :defer t
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
(setopt transient-mark-mode t)

;; ElDoc config
(setq eldoc-idle-delay 0.5)
(setq eldoc-echo-area-display-truncation-message nil)
(setopt global-eldoc-mode t)

;; Enable commands
(put 'narrow-to-region 'disabled nil)
(put 'widen 'disabled nil)

;; Display battery status
(setopt display-battery-mode t)

;; Display name of a "function" (depends of the context)
(setq which-func-update-delay 0.5)
(setq which-func-display 'header)
(setopt which-function-mode t)

;; Blink the screen
(setq visible-bell t)

;; Follow the compilation buffer
(setq compilation-scroll-output t)

;; Native compilation for Emacs
(setq native-comp-jit-compilation t)
(setq native-comp-speed 2)

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
(setq grep-command "grep --color=auto -nH --null -r -i ") ;; Original: "grep --color=auto -nH --null -e "

;; Rectangle mode config
(use-package rect
  :defer t
  :bind
  (:map rectangle-mark-mode-map
        ("s-a" . string-rectangle))
  :defer t)

;; Control how you want to show info using `what-cursor-position'
;; (setq describe-char-unidata-list '(name old-name general-category decomposition canonical-combining-class bidi-class decimal-digit-value digit-value numeric-value mirrored iso-10646-comment uppercase lowercase titlecase))
(setq describe-char-unidata-list t)

;; Calc config
(use-package calc
  :defer t
  :config
  (setq calc-group-digits t))

;; Global lexical-binding (Emacs 31)
;; (set-default-toplevel-value 'lexical-binding t)

;; Highlight pairs of parentheses
(setq show-paren-style 'parenthesis) ;;'mixed
(show-paren-mode 1)

(use-package vc
  :defer t
  :custom
  (vc-follow-symlinks 'ask)
  (vc-git-diff-switches t) ;;'("--histogram")
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

;; The default image scaling
(setq-default image-scaling-factor 'auto)

;; No backup files
(setq make-backup-files nil)

;; Create lock files
;; (setq create-lockfiles t)

;; Inhibit backups
;; (setq backup-inhibited t)

;; Auto-Save-Mode
;; (auto-save-mode 1)

;; No auto-save files
;;(setq auto-save-default nil)

;; Set backup directory
;; (setq backup-directory-alist `(("." . "~/.saves")))

;; Backup by copying instead of renaming original file
;; (setq backup-by-copying t)

;; More backups by default
;; (setq delete-old-versions t
;;   kept-new-versions 6
;;   kept-old-versions 2
;;   version-control t)

;; Forced backups
;; (defun force-backup-of-buffer ()
;;   (setq buffer-backed-up nil))

;; (add-hook 'before-save-hook  'force-backup-of-buffer)

;; ;; Replace boring scratch buffer with custom buffer that contains links to files using flnkf.el
;; (defun check-if-file-at-startup ()
;;   "Check if a file is being opened at startup."
;;   (if (or (buffer-file-name) (memq major-mode '(dired-mode)))
;;       (message "Skipping flnkf buffer.")
;;     (flnkf-open-default-buffer-list 4)
;;     ))

;; ;; Run the check after Emacs initialization
;; (add-hook 'emacs-startup-hook 'check-if-file-at-startup)
