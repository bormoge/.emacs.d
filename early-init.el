;;; early-init.el --- Early init -*- lexical-binding: t; -*-

;; Increase the amount of data which Emacs reads from the process
;; Original value: 4096
(setq read-process-output-max (* 4 1024 1024)) ;; 4mb

;; Change Emacs garbage collector threshold
(setopt gc-cons-threshold 100000000 ;; most-positive-fixnum
	gc-cons-percentage 0.6)

;; Paraphrasing the emacs-lsp project:
;; "plists provide better performance in deserialization and also put less presure than hash-tables."
(setenv "LSP_USE_PLISTS" "true")

;; End:

;;; early-init.el ends here
