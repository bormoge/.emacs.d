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

;; Funtion for copying and pasting a line
(defun copy-paste-the-entire-line ()
  "Execute the functions `copy-the-entire-line', `open-line', `next-line', and `yank'.\n
Copies the entire line of text, creates a new line, and pastes the copied line in the new line."
  (interactive)
  (copy-the-entire-line)
  (open-line 1)
  (next-line)
  (yank))

(defun delete-the-entire-line ()
  "Execute the functions `kill-whole-line' and `left-char'.\n
Deletes the entire line of text and goes back to the previous line, specifically after the last character."
  (interactive)
  ;; (backward-delete-char-untabify 1)
  (kill-whole-line 1)
  (left-char))


;; (defun my-line-save ()
;;   (interactive)
;;   (let ((l(substring (thing-at-point 'line)0 -1)))
;;     (kill-new l)
;;     (message "saved : %s" l)))

;; (local-set-key (kbd "C-c w") #'my-line-save)
