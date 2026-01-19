;; Note to self: DO NOT USE M-s-s (The orca screen reader)

;; To invoke Hyper key: CTRL + x @ h

;; Set keys for changing window focus
(define-key (current-global-map) (kbd "s-<left>") 'windmove-left)
(define-key (current-global-map) (kbd "s-<right>") 'windmove-right)
(define-key (current-global-map) (kbd "s-<up>") 'windmove-up)
(define-key (current-global-map) (kbd "s-<down>") 'windmove-down)

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
(define-key (current-global-map) (kbd "H-x s-q") 'indent-tabs-mode)

;; Define keys for package functions
(define-key (current-global-map) (kbd "H-q l") 'list-packages)
