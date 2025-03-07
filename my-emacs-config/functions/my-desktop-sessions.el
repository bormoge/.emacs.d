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

(defun my-desktop-get-session-name (prompt)
  "Get the session using its name."
  (completing-read prompt (and (file-exists-p my-desktop-session-dir)
                               (directory-files my-desktop-session-dir))
                   nil nil nil my-desktop-session-hist))

;; I'll see what I do with this

;; (defun my-desktop-list-sessions ()
;;   "Get a list of all sessions."
;;   (interactive)
;;   (switch-to-buffer "*List of sessions*")
;;   (erase-buffer)
;;   (insert "Sessions saved:\n\n")
;;   (let ((dir-list (directory-files my-desktop-session-dir)))
;;       (dolist (value dir-list)
;; 	(unless (member value '("." ".."))
;; 	  ;;(insert (format "%s\n" value))
;; 	  (my-desktop-link-text (format "%s" value))
;; 	  (insert "\n")
;; 	  )
;; 	)
;;       )
;;   (read-only-mode 1)
;;   )

(defun my-desktop-buffer-list-sessions ()
  "Get a list of all sessions."
  (let ((session-buffer (generate-new-buffer "*List of sessions*")))
    (with-current-buffer session-buffer
      (erase-buffer)
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
    session-buffer)))

(defun my-desktop-open-buffer-list ()
  "Directly open the buffer to get a list of all sessions"
  (interactive)
  (switch-to-buffer (my-desktop-buffer-list-sessions))
  )

(defun my-desktop-open-buffer-list-right ()
  "Get a list of all sessions in a split window."
  (interactive)
  (split-window-right)
  (other-window 1)
  (my-desktop-open-buffer-list)
  (other-window 1)
  )

(defun my-desktop-link-text (text-to-link)
  "Put a link in the text"
  (insert-text-button text-to-link
		      'action (let ((linked-text text-to-link))
				(lambda (x) (my-desktop-read linked-text)))
		      'follow-link t)
  )
