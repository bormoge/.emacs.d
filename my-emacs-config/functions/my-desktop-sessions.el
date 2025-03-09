;;; -*- lexical-binding: t; -*-
;; Necessary for my-desktop-link-text

(defvar my-desktop-session-dir
  (concat (getenv "HOME") "/.emacs.d/desktop-sessions/")
  "Set a directory to save desktop sessions.")

(defvar my-desktop-session-hist nil
  "Variable to represent desktop session history.")

(defun my-desktop-save (&optional name)
  "Save a desktop session."
  (interactive)
  (unless name
    (setq name (my-desktop-get-session-name "Save session as: ")))
  (make-directory (concat my-desktop-session-dir name) t)
  (desktop-save (concat my-desktop-session-dir name) t))

(defun my-desktop-read (&optional name)
  "Read a saved desktop session."
  (interactive)
  (unless name
    (setq name (my-desktop-get-session-name "Load session: ")))
  (desktop-read (concat my-desktop-session-dir name)))

(defun my-desktop-delete (&optional name)
  "Delete a saved desktop session."
  (interactive)
  (unless name
    (let ((delete-by-moving-to-trash nil)) ;; You can set this to t if you fancy trying to move it to Trash. Seems to have some delay tho.
      (setq name (my-desktop-get-session-name "Delete session: "))
      (delete-directory (concat my-desktop-session-dir name) t nil)))) ;; You can change the 3rd arg (nil) to t to move to Trash, provided you also changed delete-by-moving-to-trash to t.


(defun my-desktop-get-session-name (prompt)
  "Get the session using its name."
  (completing-read prompt (and (file-exists-p my-desktop-session-dir)
                               (directory-files my-desktop-session-dir))
                   nil nil nil my-desktop-session-hist))

(defun my-desktop-buffer-list-sessions ()
  "Get a list of all sessions."
  (if (file-directory-p my-desktop-session-dir)
      (let ((session-buffer (generate-new-buffer "*List of sessions*")))
	(with-current-buffer session-buffer
          (erase-buffer)
	  ;; Implicitely set global minor modes assigning major mode (Inheritance of minor modes perhaps?)
	  (text-mode)
	  ;; Explicitely set minor mode related to global mode
	  ;; (If you don't set major mode, you need to set all minor mode variables)
	  ;; (display-line-numbers-mode nil)
	  (insert "Sessions saved:\n\n")
          (let ((dir-list (directory-files my-desktop-session-dir)))
            (dolist (value dir-list)
              (unless (member value '("." ".."))
		(my-desktop-link-text (format "%s" value))
		(insert "\n"))))
          (read-only-mode 1)
          ;; (setq-local
          ;;  split-width-threshold 0
          ;;  split-height-threshold nil)
	  session-buffer))
    (switch-to-buffer "*scratch*")))

(defun my-desktop-open-buffer-list ()
  "Directly open the buffer to get a list of all sessions"
  (interactive)
  (switch-to-buffer (my-desktop-buffer-list-sessions)))

(defun my-desktop-open-buffer-list-right ()
  "Get a list of all sessions in a split window."
  (interactive)
  (split-window-right)
  (other-window 1)
  (my-desktop-open-buffer-list)
  (other-window 1))

(defun my-desktop-link-text (text-to-link)
  "Put a link in the text"
  (insert-text-button text-to-link
		      'action (let ((linked-text text-to-link))
				(lambda (x) (my-desktop-read linked-text)))
		      'follow-link t))
