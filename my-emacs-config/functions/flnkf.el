;;; -*- lexical-binding: t; -*-
;; Necessary for flnkf-link-text

;; flnkf (File linker)
;; This elisp code file exists for the purpose of outputting links to files.

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

(defun flnkf-default-file-reader (&optional text-scale-number)
  "Default file flnkf-file.txt inside ~/.emacs.d/flnkf/"
  (flnkf-file-reader flnkf-file text-scale-number)
  )

(defun flnkf-file-reader (path-of-file &optional text-scale-number tab-bar-or-tab-line bury-current-buffer)
  "Reads a regular file and returns a buffer with links to the valid file paths found inside the read file."
  (if (file-regular-p (expand-file-name path-of-file))
      (if (get-buffer (expand-file-name path-of-file))
	  (switch-to-buffer (expand-file-name path-of-file))
	(let ((flnkf-buffer (switch-to-buffer (expand-file-name path-of-file))))
	  (with-current-buffer flnkf-buffer
            (erase-buffer)
	    ;; Implicitely set global minor modes assigning major mode (Inheritance of minor modes perhaps?)
	    (text-mode)
	    ;; Explicitely set minor mode related to global mode
	    ;; (If you don't set major mode, you need to set all minor mode variables)
	    ;; (display-line-numbers-mode nil)
	    (let ((text-scale-number (or text-scale-number text-scale-mode-amount)))
	      (text-scale-set text-scale-number))
            (insert-file-contents path-of-file)
            (goto-char (point-min))
            (while (not (eobp))
              (let ((line (thing-at-point 'line t)))
		;; 'file-regular-p' for regular files // 'file-exists-p' for any type of file // 'file-directory-p' for directories
		(when (and line (file-exists-p (string-trim line)))
		  ;; Replace the line with a linked version
		  (beginning-of-line)
		  (delete-region (line-beginning-position) (line-end-position))
		  (flnkf-link-text (string-trim line) text-scale-number tab-bar-or-tab-line bury-current-buffer)))
              (forward-line 1))
	    (read-only-mode 1)
	    (beginning-of-buffer)
	    flnkf-buffer)))
	(switch-to-buffer "*scratch*")))

(defun flnkf-link-text (text-to-link &optional text-scale-number tab-bar-or-tab-line bury-current-buffer)
  "Put a clickable link in the text to open the file in a new tab.\nIf the path includes `flnkf-directory', it uses `flnkf-file-reader'."
  (let ((file-path (string-trim text-to-link)))
    (insert-text-button file-path
                        'action (lambda (x)
                                  (let ((path file-path))
				    ;; Change relative path for absolute path
                                    (let ((expanded-path (expand-file-name path)))
                                      ;; Check if the path is inside ~/.emacs.d/flnkf/
                                      (if (string-prefix-p (expand-file-name flnkf-directory) expanded-path)
                                          (progn
					    (let ((bury-current-buffer (or bury-current-buffer 0)))
					      (when (> bury-current-buffer 0)
						(bury-buffer)))
					    (let ((tab-bar-or-tab-line (or tab-bar-or-tab-line 0)))
					      (when (> tab-bar-or-tab-line 0)
						(tab-new)))
					    ;;(tab-new)
					    (let ((flnkf-buffer (flnkf-file-reader path text-scale-number)))
					      (switch-to-buffer flnkf-buffer)))
                                        (progn
					  (let ((bury-current-buffer (or bury-current-buffer 0)))
					      (when (> bury-current-buffer 0)
						(bury-buffer)))
					  (let ((tab-bar-or-tab-line (or tab-bar-or-tab-line 0)))
					      (when (> tab-bar-or-tab-line 0)
						(tab-new)))
					  ;;(tab-new)
					  (find-file path)
					  )))))
                        'follow-link t)))

(defun flnkf-open-default-buffer-list (&optional text-scale-number)
  "Directly open the buffer in current window to get links of found paths.\nIt uses `flnkf-file' as default."
  (interactive)
  (switch-to-buffer (flnkf-default-file-reader text-scale-number)))

(defun flnkf-open-default-buffer-list-right ()
  "Open the buffer in a split window to get links of found paths.\nIt uses `flnkf-file' as default."
  (interactive)
  (split-window-right)
  (other-window 1)
  (flnkf-open-default-buffer-list)
  (other-window 1))

(defun flnkf-open-buffer-list (&optional path-of-file)
  "Directly open the buffer in current window to get links of found paths in a chosen file.\nHas an optional argument, otherwise it asks for user input."
  (interactive)
  (when (not path-of-file)
    (setq path-of-file (read-file-name "Enter value: ")))
  (if (and (file-exists-p path-of-file) (file-regular-p path-of-file))
      (switch-to-buffer (flnkf-file-reader (expand-file-name path-of-file)))
    (message "Not a regular file.")))

(defun flnkf-open-buffer-list-right (&optional path-of-file)
  "Open the buffer in a split window to get links of found paths in a chosen file.\nHas an optional argument, otherwise it asks for user input."
  (interactive)
  (when (not path-of-file)
    (setq path-of-file (read-file-name "Enter value: ")))
  (if (and (file-exists-p path-of-file) (file-regular-p path-of-file))
      (progn
	(split-window-right)
	(other-window 1)
	(flnkf-open-buffer-list path-of-file)
	(other-window 1))
    (message "Not a regular file.")))
