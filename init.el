(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(misterioso)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

;; Funtion for copying a line
(defun copy-the-entire-line ()
  "Execute the functions `move-beginning-of-line', `set-mark', `move-end-of-line', and `kill-ring-save'.\n
Copies the entire line of text."
  (interactive)
  (move-beginning-of-line 1)
  (set-mark (point))
  ;; (next-line)
  (move-end-of-line 1)
  (kill-ring-save (point) (mark)))

(global-set-key (kbd "M-s-w") 'copy-the-entire-line)

;; Funtion for copying and pasting a line
(defun copy-paste-the-entire-line ()
  "Execute the functions `copy-the-entire-line', `open-line', `next-line', and `yank'.\n
Copies the entire line of text, creates a new line, and pastes the copied line in the new line."
  (interactive)
  (copy-the-entire-line)
  (open-line 1)
  (next-line)
  (yank))

(global-set-key (kbd "M-s-y") 'copy-paste-the-entire-line)
