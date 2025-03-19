;;; -*- lexical-binding: t; -*-
;; Necessary for tnwmt-link-text

;; tnwmt AKA Tis not worth my time
;; Mostly a frontend for desktop.el that serves to standarize my personal workflow.

(defvar tnwmt-session-dir
  (concat (getenv "HOME") "/.emacs.d/desktop-sessions/")
  "Set a directory to save desktop sessions.")

(defvar tnwmt-session-hist nil
  "Variable to represent desktop session history.")

(defun tnwmt-save (&optional name)
  "Save a desktop session."
  (interactive)
  (unless name
    (setq name (tnwmt-get-session-name "Save session as: ")))
  (make-directory (concat tnwmt-session-dir name) t)
  (desktop-save (concat tnwmt-session-dir name) t))

(defun tnwmt-read (&optional name)
  "Read a saved desktop session."
  (interactive)
  (unless name
    (setq name (tnwmt-get-session-name "Load session: ")))
  (desktop-read (concat tnwmt-session-dir name)))

(defun tnwmt-delete (&optional name)
  "Delete a saved desktop session."
  (interactive)
  (unless name
    (let ((delete-by-moving-to-trash nil)) ;; You can set this to t if you fancy trying to move it to Trash. Seems to have some delay tho.
      (setq name (tnwmt-get-session-name "Delete session: "))
      (delete-directory (concat tnwmt-session-dir name) t nil)))) ;; You can change the 3rd arg (nil) to t to move to Trash, provided you also changed delete-by-moving-to-trash to t.

(defun tnwmt-get-session-name (prompt)
  (let ((name (completing-read prompt (and (file-exists-p tnwmt-session-dir)
                                           (directory-files tnwmt-session-dir))
                               nil nil nil 'tnwmt-session-hist)))
    (add-to-history 'tnwmt-session-hist name)
    name))

(defun tnwmt-buffer-list-sessions ()
  "Get a list of all sessions."
  (if (file-directory-p tnwmt-session-dir)
      (let ((session-buffer (generate-new-buffer "*List of sessions*")))
	(with-current-buffer session-buffer
          (erase-buffer)
	  ;; Implicitely set global minor modes assigning major mode (Inheritance of minor modes perhaps?)
	  (text-mode)
	  ;; Explicitely set minor mode related to global mode
	  ;; (If you don't set major mode, you need to set all minor mode variables)
	  ;; (display-line-numbers-mode nil)
	  (text-scale-set 5)
	  (insert "Sessions saved:\n\n")
          (let ((dir-list (directory-files tnwmt-session-dir)))
            (dolist (value dir-list)
              (unless (member value '("." ".."))
		(tnwmt-link-text (format "%s" value))
		(insert "\n"))))
	  (previous-line)
	  (read-only-mode 1)
          ;; (setq-local
          ;;  split-width-threshold 0
          ;;  split-height-threshold nil)
	  session-buffer))
    (switch-to-buffer "*scratch*")))

(defun tnwmt-open-buffer-list ()
  "Directly open the buffer to get a list of all sessions"
  (interactive)
  (if (get-buffer "*List of sessions*")
      (switch-to-buffer "*List of sessions*")
    (switch-to-buffer (tnwmt-buffer-list-sessions))))

(defun tnwmt-open-buffer-list-right ()
  "Get a list of all sessions in a split window."
  (interactive)
  (split-window-right)
  (other-window 1)
  (tnwmt-open-buffer-list)
  (other-window 1))

(defun tnwmt-link-text (text-to-link)
  "Put a link in the text"
  (insert-text-button text-to-link
		      'action (let ((linked-text text-to-link))
				(lambda (x) (tnwmt-read linked-text)))
		      'follow-link t))
