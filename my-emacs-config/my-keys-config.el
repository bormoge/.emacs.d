;; Note to self: DO NOT USE M-s-s (The orca screen reader)

;; To invoke Hyper key: CTRL + x @ h

;; If you wonder what s-<0x10081247> is, it's the copilot key that comes with the new thinkpads.

;; Set key for swapping windows
(define-key (current-global-map) (kbd "C-x M-o") 'window-swap-states)

;; Set keys for shell, eshell, term
(define-key (current-global-map) (kbd "H-s e b") 'eshell)
(define-key (current-global-map) (kbd "H-s e c") 'eshell-command)
(define-key (current-global-map) (kbd "H-s s b") 'shell)
(define-key (current-global-map) (kbd "H-s s c") 'shell-command)
(define-key (current-global-map) (kbd "H-s t b") 'term)

;; Delete duplicate lines using Hyper + ALT + <backspace>
(define-key (current-global-map) (kbd "H-M-<backspace>") 'delete-duplicate-lines)

;; Set key for replace-string
(define-key (current-global-map) (kbd "H-r") 'replace-string)

;; Set key for finding built-in Emacs libraries / files
(define-key (current-global-map) (kbd "H-h l") 'find-library)

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

;; Define keys for library repeat.el
(define-key (current-global-map) (kbd "s-<") 'repeat)
(define-key (current-global-map) (kbd "s->") 'repeat-mode)
(define-key (current-global-map) (kbd "s-z") 'describe-repeat-maps)

;; Convert tabs to spaces or spaces to tabs
(define-key (current-global-map) (kbd "H-x s-<tab>") 'tabify)
(define-key (current-global-map) (kbd "H-x s-<backspace>") 'untabify)
(define-key (current-global-map) (kbd "H-x s-<iso-lefttab>") 'untabify)

;; Define easy key for eldoc documentation
(define-key (current-global-map) (kbd "s-. .") 'eldoc)
