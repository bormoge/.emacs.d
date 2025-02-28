;; Maximize Emacs on start
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Change focus of windows and frames using Alt+o and Alt+shift+o
;;(global-set-key (kbd "M-o") #'other-window)
;;(global-set-key (kbd "M-O") #'other-frame)
(define-key (current-global-map) (kbd "H-<left>") 'windmove-left)
(define-key (current-global-map) (kbd "H-<right>") 'windmove-right)
(define-key (current-global-map) (kbd "H-<up>") 'windmove-up)
(define-key (current-global-map) (kbd "H-<down>") 'windmove-down)

;; Delete duplicate lines using ALT + SUPER (normally Windows Key) + <backspace>
(define-key (current-global-map) (kbd "M-s-<backspace>") 'delete-duplicate-lines)

;; Show number of the lines
(global-display-line-numbers-mode 1)

;; Enable use of system clipboard
;;(setq x-select-enable-clipboard t)

;; Tab Bars
(tab-bar-mode 1)

;; Window Tab Lines
(global-tab-line-mode 1)

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
