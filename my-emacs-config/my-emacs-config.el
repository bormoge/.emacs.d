;; Maximize Emacs on start
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Change focus of windows and frames using Alt+o and Alt+shift+o
;;(global-set-key (kbd "M-o") #'other-window)
;;(global-set-key (kbd "M-O") #'other-frame)

;; DO NOT USE M-s-s

;; To invoke Hyper key: CTRL + x @ h

;; Set keys for changing window focus
(define-key (current-global-map) (kbd "H-<left>") 'windmove-left)
(define-key (current-global-map) (kbd "H-<right>") 'windmove-right)
(define-key (current-global-map) (kbd "H-<up>") 'windmove-up)
(define-key (current-global-map) (kbd "H-<down>") 'windmove-down)

;; Set keys for my-desktop-sessions.el
(define-key (current-global-map) (kbd "H-r") 'my-desktop-read)
(define-key (current-global-map) (kbd "H-s") 'my-desktop-save)

;; Set keys for copy-the-entire-line.el
(global-set-key (kbd "M-s-w") 'copy-the-entire-line)
(global-set-key (kbd "M-s-y") 'copy-paste-the-entire-line)
(global-set-key (kbd "M-s-d") 'delete-the-entire-line)

;; Delete duplicate lines using Hyper + ALT + <backspace>
(define-key (current-global-map) (kbd "H-M-<backspace>") 'delete-duplicate-lines)

;; Set key for replace-string
(define-key (current-global-map) (kbd "H-f") 'replace-string)

;; Set key for grep
(define-key (current-global-map) (kbd "H-g") 'grep)

;; Set key for undo-only
;;(define-key (current-global-map) (kbd "C-¿") 'undo-only)

;; Set key for revert-buffer (discard all changes)
;;(define-key (current-global-map) (kbd "H-?") 'revert-buffer)

;; Show number of the lines
(global-display-line-numbers-mode 1)

;; Enable use of system clipboard
;;(setq x-select-enable-clipboard t)

;; Tab Bars
(tab-bar-mode 1)

;; Window Tab Lines
(global-tab-line-mode 1)

;; Increase zoom
(add-hook 'after-change-major-mode-hook (lambda () (text-scale-set 3)))

;; Change font
(set-frame-font "Source Code Pro 10")

;; Truncate long lines
;;(setq-default truncate-lines t)
(global-visual-line-mode t)

;; Enable Horizontal Scroll Bar
(horizontal-scroll-bar-mode 1)

;; No backup files
(setq make-backup-files nil)

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
