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
(global-display-line-numbers-mode)

;; Enable use of system clipboard
;;(setq x-select-enable-clipboard t)
