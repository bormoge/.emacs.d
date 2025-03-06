;; Increase the amount of data which Emacs reads from the process
;; Original value: 4096
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Change Emacs garbage collector threshold
(setopt gc-cons-threshold 100000000 ;; most-positive-fixnum
	gc-cons-percentage 0.6)

;; (defun my-minibuffer-setup-hook ()
;;   (setq gc-cons-threshold most-positive-fixnum))

;; (defun my-minibuffer-exit-hook ()
;;   (setq gc-cons-threshold 800000))

;; (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
;; (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; Necessary to avoid white flashing at startup
(setq default-frame-alist '((background-color . "#000000")
			    (ns-appearance . dark)
			    (ns-transparent-titlebar . t)))
