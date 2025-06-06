;; Remove startup screen
(setq inhibit-startup-screen t)

;; Maximize Emacs on start
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; DO NOT USE M-s-s

;; To invoke Hyper key: CTRL + x @ h

;; Set keys for changing window focus
(define-key (current-global-map) (kbd "H-<left>") 'windmove-left)
(define-key (current-global-map) (kbd "H-<right>") 'windmove-right)
(define-key (current-global-map) (kbd "H-<up>") 'windmove-up)
(define-key (current-global-map) (kbd "H-<down>") 'windmove-down)

;; Set keys for shell, eshell, term
(define-key (current-global-map) (kbd "H-c e b") 'eshell)
(define-key (current-global-map) (kbd "H-c e c") 'eshell-command)
(define-key (current-global-map) (kbd "H-c s b") 'shell)
(define-key (current-global-map) (kbd "H-c s c") 'shell-command)
(define-key (current-global-map) (kbd "H-c t b") 'term)

;; Set keys for tnwmt.el
(define-key (current-global-map) (kbd "H-r") 'tnwmt-read)
(define-key (current-global-map) (kbd "H-s") 'tnwmt-save)

;; Set keys for the-entire-line.el
(global-set-key (kbd "M-s-w") 'copy-the-entire-line)
(global-set-key (kbd "M-s-y") 'copy-paste-the-entire-line)
(global-set-key (kbd "M-s-d") 'delete-the-entire-line)

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

;; Show number of the lines
(global-display-line-numbers-mode 1)

;; Enable use of system clipboard
(setq select-enable-clipboard t)

;; Tab Bars
(tab-bar-mode 1)
(setq tab-bar-history-mode nil)

;; Window Tab Lines
(global-tab-line-mode 1)

;; Enable menus
(menu-bar-mode t)
(tool-bar-mode t)

;; Enable tooltips
(tooltip-mode t)

;; Increase zoom
;;(add-hook 'after-change-major-mode-hook (lambda () (text-scale-set 3)))
(add-hook 'after-change-major-mode-hook
	  (lambda ()
	    (unless (derived-mode-p 'treemacs-mode)
	      (text-scale-set 3))))

;; Change font
(set-frame-font "Source Code Pro 10")

;; Truncate long lines
;;(setq-default truncate-lines t)
(global-visual-line-mode t)

;; Enable Vertical Scroll Bar
(scroll-bar-mode 'right)

;; Enable Horizontal Scroll Bar
(horizontal-scroll-bar-mode 1)

;; Replace a selected area with typed text
(delete-selection-mode 1)

;; Add directory to desktop-path
(with-eval-after-load 'desktop
  (add-to-list 'desktop-path (expand-file-name "desktop-sessions/" user-emacs-directory)))

;; Turn off clickable text highlight
(setq mouse-highlight nil)

;; At startup, show the messages
(view-echo-area-messages)
(with-current-buffer (messages-buffer)
  (enlarge-window 9)
  )

;; Display warnings depending of level
(setq warning-minimum-level :warning)

;; Nice to have if you are dealing with packages. Disables docstring warnings
(setq byte-compile-warnings
      '(not docstrings))

;; Display time and date on the minibuffer (neat stuff)
(setq display-time-day-and-date t)
(display-time-mode t)

;; Save minibuffer history. By default it will be on ~/.emacs.d/history
(savehist-mode)
(setq savehist-file "~/.emacs.d/history"
  history-length 50
  history-delete-duplicates t
  savehist-save-minibuffer-history t
  ;;savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
  )

;; Modify the appearance of the region
(custom-set-faces '(region ((t :extend t)))) ;; Use ':extend t' or ':extend nil' to modify if region covers entire line.

;; Change cursor's appearance
(setq blink-cursor-mode t)
(set-default 'cursor-type '(bar . 7))

;; Auto-refresh buffers. If a file was changed on disk, revert changes on buffer.
(global-auto-revert-mode t) ;; Nil by default

;; Scroll settings
(setq scroll-conservatively 100)
(setq scroll-margin 1)
(setq scroll-step 1)
(setq scroll-preserve-screen-position nil)

;; No backup files
(setq make-backup-files nil)

;; Config for vertico package
(setq enable-recursive-minibuffers t)
(setq read-extended-command-predicate #'command-completion-default-include-p) ;; Other value(s): nil, transient-command-completion-not-suffix-only-p
(setq minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)) ;; Original value: (read-only t face minibuffer-prompt)

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
  (if (or (buffer-file-name) load-file-name)
      (message "A file is opened, skipping desktop sessions buffer.")
    (flnkf-open-default-buffer-list)
    ))
;;    (tnwmt-open-buffer-list)))

;; Run the check after Emacs initialization
(add-hook 'emacs-startup-hook 'check-if-file-at-startup)
