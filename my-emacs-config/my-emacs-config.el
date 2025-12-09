;; Remove startup screen
(setq inhibit-startup-screen t)

;; Maximize Emacs on start
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Note to self: DO NOT USE M-s-s (The orca screen reader)

;; To invoke Hyper key: CTRL + x @ h

;; Set keys for changing window focus
(define-key (current-global-map) (kbd "H-<left>") 'windmove-left)
(define-key (current-global-map) (kbd "H-<right>") 'windmove-right)
(define-key (current-global-map) (kbd "H-<up>") 'windmove-up)
(define-key (current-global-map) (kbd "H-<down>") 'windmove-down)

;; Set key for swapping windows
(define-key (current-global-map) (kbd "C-x M-o") 'window-swap-states)

;; Set keys for shell, eshell, term
(define-key (current-global-map) (kbd "H-c e b") 'eshell)
(define-key (current-global-map) (kbd "H-c e c") 'eshell-command)
(define-key (current-global-map) (kbd "H-c s b") 'shell)
(define-key (current-global-map) (kbd "H-c s c") 'shell-command)
(define-key (current-global-map) (kbd "H-c t b") 'term)

;; Delete duplicate lines using Hyper + ALT + <backspace>
(define-key (current-global-map) (kbd "H-M-<backspace>") 'delete-duplicate-lines)

;; Set key for replace-string
(define-key (current-global-map) (kbd "H-f") 'replace-string)

;; Set key for grep
(define-key (current-global-map) (kbd "H-g") 'grep)

;; Count words in a region
(define-key (current-global-map) (kbd "H-w") 'count-words)

;; Set key for finding built-in Emacs libraries / files
(define-key (current-global-map) (kbd "H-h l") 'find-library)

;; Set key for undo-only
(define-key (current-global-map) (kbd "C-S-/") 'undo-only)

;; Define help keys for `find-library', `describe-keymap', `describe-char', and `describe-face'
(define-key (current-global-map) (kbd "C-h M-l") 'find-library)
(define-key (current-global-map) (kbd "C-h M-k") 'describe-keymap)
(define-key (current-global-map) (kbd "C-h M-c") 'describe-char)
(define-key (current-global-map) (kbd "C-h M-f") 'describe-face)

;; Define key for ibuffer
(define-key (current-global-map) (kbd "C-x M-b") 'ibuffer)

;; Define key for bury-buffer
(define-key (current-global-map) (kbd "H-x k") 'bury-buffer)

;; Replace regexp
(define-key (current-global-map) (kbd "M-s-r r") 'query-replace-regexp)

;; Replace string
(define-key (current-global-map) (kbd "M-s-r s") 'query-replace)

;; Open flnkf buffer list of links
(define-key (current-global-map) (kbd "H-o") 'flnkf-open-default-buffer-list)

;; Open full-calc
(define-key (current-global-map) (kbd "<Calculator>") 'full-calc)

;; Define keys for library repeat.el
(define-key (current-global-map) (kbd "s-<") 'repeat)
(define-key (current-global-map) (kbd "s->") 'repeat-mode)
(define-key (current-global-map) (kbd "s-z") 'describe-repeat-maps)

;; Define keys for diff and ediff
(define-key (current-global-map) (kbd "H-d 1") 'diff)
(define-key (current-global-map) (kbd "H-d 2") 'diff-buffers)
(define-key (current-global-map) (kbd "H-d 3") 'ediff)
(define-key (current-global-map) (kbd "H-d 4") 'ediff-buffers)

;; Convert tabs to spaces or spaces to tabs
(define-key (current-global-map) (kbd "H-x s-<tab>") 'tabify)
(define-key (current-global-map) (kbd "H-x s-<backspace>") 'untabify)
(define-key (current-global-map) (kbd "H-x s-<iso-lefttab>") 'untabify)
(define-key (current-global-map) (kbd "H-x s-w") 'whitespace-mode)

;; Enable syntax highlighting
(global-font-lock-mode t)

;; Bidirectional editing config
(setq-default bidi-paragraph-direction 'left-to-right)
(if (version<= "27.1" emacs-version)
    (setq bidi-inhibit-bpa t))

;; Show number of the lines
(global-display-line-numbers-mode 1)
(setq display-line-numbers-width nil)
(setq line-number-mode t)
(setq column-number-mode t)
(setq size-indication-mode t)

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
;;(add-hook 'after-change-major-mode-hook (lambda () (text-scale-set 3)))
(setq-default text-scale-mode-amount 2)
(add-hook 'after-change-major-mode-hook
	  (lambda ()
	    (if (derived-mode-p 'treemacs-mode)
		(text-scale-set 0)
	      (text-scale-set text-scale-mode-amount))
	    ))

;; Change font
;;(set-frame-font "Adwaita Mono 12" nil t)
;;(set-frame-font "JetBrainsMono Nerd Font Mono 12" nil t)
;;(set-frame-font "Inconsolata Nerd Font Mono 14" nil t)
;;(set-frame-font "Hack Nerd Font Mono 12" nil t)
;;(set-frame-font "Iosevka Nerd Font Mono 14" nil t)
;;(set-frame-font "FiraCode Nerd Font Mono Light 12" nil t)
(set-frame-font "Ioskeley Mono Light 12" nil t)

;; Truncate long lines
;;(setq-default truncate-lines t)
(global-visual-line-mode t)

;; Enable Vertical Scroll Bar
(scroll-bar-mode 'right)

;; Enable Horizontal Scroll Bar
;;(horizontal-scroll-bar-mode t)

;; Replace a selected area with typed text
(delete-selection-mode 1)

;; Add directory to desktop-path
(with-eval-after-load 'desktop
  (add-to-list 'desktop-path (expand-file-name "desktop-sessions/" user-emacs-directory)))

;; Turn off clickable text highlight
(setq mouse-highlight nil)

;; At startup, show the messages
;; (view-echo-area-messages)
;; (with-current-buffer (messages-buffer)
;;   (enlarge-window 10)
;;   )

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

;; Save minibuffer history. By default it will be on ~/.emacs.d/history
(savehist-mode)
(setq savehist-file "~/.emacs.d/history"
  history-length 100
  history-delete-duplicates t
  savehist-save-minibuffer-history t
  ;;savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
  )

;; Modify the appearance of the region
(custom-set-faces '(region ((t :extend t)))) ;; Use ':extend t' or ':extend nil' to modify if region covers entire line.

;; Change cursor's appearance
(setq blink-cursor-mode t)
(set-default 'cursor-type t)
;; (set-default 'cursor-type '(bar . 7)) ;; default: t

;; Auto-refresh buffers. If a file was changed on disk, revert changes on buffer.
(setq global-auto-revert-ignore-modes '(doc-view-mode pdf-view-mode))
(global-auto-revert-mode t) ;; Nil by default

;; Scroll settings
(setq scroll-conservatively 100)
(setq scroll-margin 6)
(setq scroll-step 1)
(setq scroll-preserve-screen-position nil)

;; Config for vertico package
(setq enable-recursive-minibuffers t)
(setq read-extended-command-predicate #'command-completion-default-include-p) ;; Other value(s): nil, transient-command-completion-not-suffix-only-p
(setq minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)) ;; Original value: (read-only t face minibuffer-prompt)

;; Show character name in ‘what-cursor-position’
(setq what-cursor-show-names t)

;; Enable right click menu
(when (display-graphic-p)
  (context-menu-mode))

;; Always generate an empty line at the end of the file
(setq require-final-newline t)

;; Pass argument -u to command diff
(setq diff-switches "-u")

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
;;(editorconfig-mode t)

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

;; Use spaces for indentation
;; Alternatively, you can modify the variable `tab-width'
(setq-default indent-tabs-mode nil)

;; Follow the compilation buffer
(setq compilation-scroll-output t)

;; No backup files
(setq make-backup-files nil)

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

;; Dired config
;; (defun dired-mode-personal-setup ()
;;   "My personal config for 'dired-mode'."
;;   (dired-hide-details-mode t) ;; Set to 't' to hide permissions, authors, timestamps, etc.
;;   (dired-omit-mode t) ;; Set to 't' to hide . and ..
;;   )

;; (add-hook 'dired-mode-hook #'dired-mode-personal-config)

;; Replace boring scratch buffer with custom buffer that contains links to files using flnkf.el
(defun check-if-file-at-startup ()
  "Check if a file is being opened at startup."
  (if (or (buffer-file-name) (memq major-mode '(dired-mode)))
      (message "Skipping flnkf buffer.")
    (flnkf-open-default-buffer-list 4)
    ))

;; Run the check after Emacs initialization
(add-hook 'emacs-startup-hook 'check-if-file-at-startup)
