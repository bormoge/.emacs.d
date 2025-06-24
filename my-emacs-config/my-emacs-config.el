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

;; Define help keys for `find-library', `describe-keymap', `describe-char', and `describe-face'
(define-key (current-global-map) (kbd "C-h M-l") 'find-library)
(define-key (current-global-map) (kbd "C-h M-k") 'describe-keymap)
(define-key (current-global-map) (kbd "C-h M-c") 'describe-char)
(define-key (current-global-map) (kbd "C-h M-f") 'describe-face)

;; Define key for ibuffer
(define-key (current-global-map) (kbd "C-x M-b") 'ibuffer)

;; Enable syntax highlighting
(global-font-lock-mode t)

;; Show number of the lines
(global-display-line-numbers-mode 1)

;; Enable use of system clipboard
(setq select-enable-clipboard t)

;; Disable use of system trash
(setq delete-by-moving-to-trash nil)

;; Tab Bars
(tab-bar-mode t)
(setq tab-bar-history-mode nil)
(setq tab-bar-auto-width-max '((300) 30))
;;(setq tab-bar-auto-width nil)

(set-face-attribute 'tab-bar-tab nil
		    :background "#68217A"
		    :distant-foreground "#D4D4D4"
		    :foreground "#D4D4D4"
		    :box '(:line-width (3 . 3) :color "black" :style flat-button)
		    :weight 'heavy
		    :height 130)

(set-face-attribute 'tab-bar-tab-inactive nil
		    :background "#252526"
		    :foreground "#AEAFAD"
		    :height 130
		    :box '(:line-width (3 . 3) :color "black" :style flat-button))

;; This gives problems on NixOS because of the location of the file plus_16.svg
;; (setq tab-bar-new-button '#(" " 0 1
;; 			    (rear-nonsticky t help-echo "New tab" face shadow display
;; 					    (image :type svg :file
;; 						   "/usr/share/emacs/30.1/etc/images/symbols/plus_16.svg"
;; 						   :height (1 . em) :scale 1.6 :margin 1 :ascent
;; 						   center :transform-smoothing t)))) ;; Original value is the same except :scale 1

;; Tab Lines
(global-tab-line-mode t)

(set-face-attribute 'tab-line-tab nil
		    :background "#252526"
		    :foreground "#AEAFAD"
		    :box '(:line-width (3 . 3) :color "black" :style flat-button)
		    :height 110)

(set-face-attribute 'tab-line-tab-current nil
		    :background "#68217A"
		    :distant-foreground "#D4D4D4"
		    :foreground "#D4D4D4"
		    :weight 'heavy
		    :box '(:line-width (3 . 3) :color "black" :style flat-button)
		    :height 110)

(set-face-attribute 'tab-line-tab-inactive nil
		    :background "#252526"
		    :foreground "#AEAFAD"
		    :box '(:line-width (3 . 3) :color "black" :style flat-button)
		    :height 110)

(set-face-attribute 'tab-line-tab-modified nil
		    :foreground "#E54568"
		    :background "#130034")

;; Enable menus
(menu-bar-mode t)
(tool-bar-mode 0)
;;(modifier-bar-mode t)

;; Enable tooltips
(tooltip-mode t)

;; Increase zoom
;;(add-hook 'after-change-major-mode-hook (lambda () (text-scale-set 3)))
(add-hook 'after-change-major-mode-hook
	  (lambda ()
	    (unless (derived-mode-p 'treemacs-mode)
	      (text-scale-set 2))))

;; Change font
;; (set-frame-font "Source Code Pro 10")

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
(set-default 'cursor-type t)
;; (set-default 'cursor-type '(bar . 7)) ;; default: t

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

;; Show character name in ‘what-cursor-position’
(setq what-cursor-show-names t)

;; Enable right click menu
(when (display-graphic-p)
  (context-menu-mode))

;; Always generate an empty line at the end of the file
(setq require-final-newline t)

;; Pass argument -u to command diff
(setq diff-switches "-u")

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
