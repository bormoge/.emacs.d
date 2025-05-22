;;; -*- lexical-binding: t; -*-
;; Necessary for flnkf-link-text

;; flnkf (File linker)
;; An elisp file that exists for the purpose of outputting links to files.

(defvar flnkf-file
  (concat (cond ((boundp 'user-emacs-directory)
		 user-emacs-directory)
		((boundp 'user-init-directory)
		 user-init-directory)
		(t "~/.emacs.d/"))
	  "flnkf/flnkf-file.txt")
  "Base file with the necessary files to read and link.")

(defvar flnkf-directory
  (concat (cond ((boundp 'user-emacs-directory)
		 user-emacs-directory)
		((boundp 'user-init-directory)
		 user-init-directory)
		(t "~/.emacs.d/"))
	  "flnkf/")
  "Base directory with the files to read and link.")

(defun flnkf-default-file-reader ()
  (flnkf-file-reader flnkf-file)
  )

(defun flnkf-file-reader (path-of-file)
  "Reads flnkf-file and returns a buffer with the valid paths that exist inside flnkf-file-paths."
  (if (file-regular-p path-of-file)
      (let ((flnkf-buffer (generate-new-buffer path-of-file)))
        (with-current-buffer flnkf-buffer
          (erase-buffer)
	  ;; Implicitely set global minor modes assigning major mode (Inheritance of minor modes perhaps?)
	  (text-mode)
	  ;; Explicitely set minor mode related to global mode
	  ;; (If you don't set major mode, you need to set all minor mode variables)
	  ;; (display-line-numbers-mode nil)
	  (text-scale-set 5)
          (insert-file-contents path-of-file)
          (goto-char (point-min))
          (while (not (eobp))
            (let ((line (thing-at-point 'line t)))
	      ;; 'file-regular-p' for regular files // 'file-exists-p' for any type of file // 'file-directory-p' for directories
              (when (and line (file-exists-p (string-trim line)))
                ;; Replace the line with a linked version
                (beginning-of-line)
                (delete-region (line-beginning-position) (line-end-position))
                (flnkf-link-text (string-trim line))))
            (forward-line 1))
	  (read-only-mode 1)
	  flnkf-buffer))
    (switch-to-buffer "*scratch*")))

(defun flnkf-link-text (text-to-link)
  "Put a clickable link in the text to open the file in a new tab."
  (let ((file-path (string-trim text-to-link)))
    (insert-text-button file-path
                        'action (lambda (x)
                                  (let ((path file-path))
				    ;; Change relative path for absolute path
                                    (let ((expanded-path (expand-file-name path)))
                                      ;; Check if the path is inside ~/.emacs.d/flnkf/
                                      (if (string-prefix-p (expand-file-name flnkf-directory) expanded-path)
                                          (progn
					    (tab-new)
					    (let ((flnkf-buffer (flnkf-file-reader path)))
					      (switch-to-buffer flnkf-buffer)))
                                        (progn
					  (tab-new)
					  (find-file path)
					  )))))
                        'follow-link t)))

(defun flnkf-open-default-buffer-list ()
  "Directly open the buffer to get a list of all sessions"
  (interactive)
  (switch-to-buffer (flnkf-default-file-reader)))
  ;; (if (get-buffer "*List of files*")
  ;;     (switch-to-buffer "*List of files*")
  ;;   (switch-to-buffer (flnkf-default-file-reader))))

(defun flnkf-open-default-buffer-list-right ()
  "Get a list of all sessions in a split window."
  (interactive)
  (split-window-right)
  (other-window 1)
  (flnkf-default-open-buffer-list)
  (other-window 1))

(defun flnkf-open-buffer-list (&optional path-of-file)
  "Directly open the buffer to get a list of all sessions"
  (interactive)
  (when (not path-of-file)
    (setq path-of-file (read-file-name "Enter value: ")))
  (if (and (file-exists-p path-of-file) (file-regular-p path-of-file))
      (switch-to-buffer (flnkf-file-reader (expand-file-name path-of-file)))
    (message "Not a file.")))
  ;; (if (get-buffer "*List of files*")
  ;;     (switch-to-buffer "*List of files*")
  ;;   (switch-to-buffer (flnkf-default-file-reader))))

(defun flnkf-open-buffer-list-right (&optional path-of-file)
  "Get a list of all sessions in a split window."
  (interactive)
  (when (not path-of-file)
    (setq path-of-file (read-file-name "Enter value: ")))
  (if (and (file-exists-p path-of-file) (file-regular-p path-of-file))
      (progn
	(split-window-right)
	(other-window 1)
	(flnkf-open-buffer-list path-of-file)
	(other-window 1))
    (message "Not a file.")))
