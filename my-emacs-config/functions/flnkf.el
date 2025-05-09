;;; -*- lexical-binding: t; -*-
;; Necessary for tnwmt-link-text

;; flnkf (File linker)
;; A file that exists for the purpose of outputting links to files.

(defvar flnkf-file
  (concat (cond ((boundp 'user-emacs-directory)
		 user-emacs-directory)
		((boundp 'user-init-directory)
		 user-init-directory)
		(t "~/.emacs.d/"))
	  "flnkf-file.txt")
  "Base file with the necessary files to read and link.")

(defun flnkf-file-reader ()
  "Reads flnkf-file and returns a buffer with the valid paths that exist inside flnkf-file-paths."
  (if (file-regular-p flnkf-file)
      (let ((flnkf-buffer (generate-new-buffer "*List of files*")))
        (with-current-buffer flnkf-buffer
          (erase-buffer)
	  ;; Implicitely set global minor modes assigning major mode (Inheritance of minor modes perhaps?)
	  (text-mode)
	  ;; Explicitely set minor mode related to global mode
	  ;; (If you don't set major mode, you need to set all minor mode variables)
	  ;; (display-line-numbers-mode nil)
	  (text-scale-set 5)
          (insert-file-contents flnkf-file)
          (goto-char (point-min))
          (while (not (eobp))
            (let ((line (thing-at-point 'line t)))
              (when (and line (file-regular-p (string-trim line)))
                ;; Replace the line with a linked version
                (beginning-of-line)
                (delete-region (line-beginning-position) (line-end-position))
                (flnkf-link-text (string-trim line))))
            (forward-line 1))
	  (read-only-mode 1)
	  flnkf-buffer))
    (switch-to-buffer "*scratch*")))

(defun flnkf-link-text (text-to-link)
  "Put a clickable link in the text to open the file."
  (let ((file-path (string-trim text-to-link)))
    (insert-text-button file-path
                        'action (lambda (x) (find-file file-path))
                        'follow-link t)))

(defun flnkf-open-buffer-list ()
  "Directly open the buffer to get a list of all sessions"
  (interactive)
  (if (get-buffer "*List of files*")
      (switch-to-buffer "*List of files*")
    (switch-to-buffer (flnkf-file-reader))))

(defun flnkf-open-buffer-list-right ()
  "Get a list of all sessions in a split window."
  (interactive)
  (split-window-right)
  (other-window 1)
  (flnkf-open-buffer-list)
  (other-window 1))
