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

;; Window Tab Lines
(global-tab-line-mode 1)

;; Increase zoom
;;(add-hook 'after-change-major-mode-hook (lambda () (text-scale-set 3)))

;; Change font
(set-frame-font "Source Code Pro 10")

;; Truncate long lines
;;(setq-default truncate-lines t)
(global-visual-line-mode t)

;; Enable Horizontal Scroll Bar
(horizontal-scroll-bar-mode 1)

;; Replace a selected area with typed text
(delete-selection-mode 1)

;; At startup, show the messages
(view-echo-area-messages)
(with-current-buffer (messages-buffer)
  (enlarge-window 9)
  )

;; Nice to have if you are dealing with packages. Disables docstring warnings
(setq byte-compile-warnings
      '(not docstrings))

;; Display time and date on the minibuffer (neat stuff)
(setq display-time-day-and-date t)
(display-time-mode t)

;; Save minibuffer history. By default it will be on ~/.emacs.d/history
(savehist-mode)

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

;; Replace boring scratch buffer with custom buffer that contains links to sessions (~/.emacs.d/desktop-sessions) using tnwmt.el
(defun check-if-file-at-startup ()
  "Check if a file is being opened at startup."
  (if (or (buffer-file-name) load-file-name)
      (message "A file is opened, skipping desktop sessions buffer.")
    (tnwmt-open-buffer-list)))

;; Run the check after Emacs initialization
(add-hook 'emacs-startup-hook 'check-if-file-at-startup)
