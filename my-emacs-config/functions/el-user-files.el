(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))


(defun load-user-file (file)
  "Load a file in current user's configuration directory."
  (interactive "f")
  (load-file (expand-file-name file user-init-dir)))


(defun file-metadata ()
  (interactive)
  (let* ((fname (buffer-file-name))
         (data (file-attributes fname))
         (access (current-time-string (nth 4 data)))
         (mod (current-time-string (nth 5 data)))       ;; When the file content was last edited.
         (change (current-time-string (nth 6 data)))    ;; When the file's metadata (permissions, name, etc.) was last modified.
         (size (nth 7 data))
         (mode (nth 8 data)))
    (message
     "%s:
  Accessed: %s
  Modified: %s
  Changed: %s
  Size: %s bytes
  Mode: %s"
     fname access mod change size mode)))
