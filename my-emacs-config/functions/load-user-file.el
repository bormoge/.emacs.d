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
