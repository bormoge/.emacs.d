;; LSP, DAP, linter and formatter installer
(use-package mason
  :ensure t
  :config
  (mason-ensure
   (lambda ()
     (dolist (pkg '("rassumfrassum" "codelldb"))
       (unless (mason-installed-p pkg)
	 (ignore-errors (mason-install pkg))))))
  (mason-setup))

;; Syntax checking using Flycheck
(use-package flycheck
  :ensure t
  :init
  ;;(global-flycheck-mode)
  )

;; Packages to manage PostgreSQL
;; To upgrade use package-vc-upgrade

;; pgmacs
(use-package pgmacs
  :vc (:url "https://github.com/emarsden/pgmacs"
       :rev :newest
       :branch "main"
       :vc-backend Git)
  :ensure t
  :defer t
  )

;; pgmacs
(use-package pg
  :vc (:url "https://github.com/emarsden/pg-el"
       :rev :newest
       :branch "main"
       :vc-backend Git)
  :ensure t
  :defer t
  )

;; Integration for uv, the Python package manager
(use-package uv-mode
  :ensure t
  :hook (python-mode . uv-mode-auto-activate-hook)
  )

;; Package to integrate various python tools. I'll leave this here just in case I end up using it.
;; (use-package pet
;;   :ensure t
;;   :config
;;   (add-hook 'python-base-mode-hook 'pet-mode -10)
;;   ;; (add-hook 'python-mode-hook
;;   ;;         (lambda ()
;;   ;;           (setq-local python-shell-interpreter (pet-executable-find "python")
;;   ;;                       python-shell-virtualenv-root (pet-virtualenv-root))))
;;   )

(use-package combobulate
  :vc (:url "https://github.com/mickeynp/combobulate"
       :rev :newest
       :branch "master"
       :vc-backend Git)
  :ensure t
  :defer t
  ;; :custom
  ;; (combobulate-key-prefix "s-o") ;; It's bugged, probably related to mickeynp/combobulate#117
  :hook ((prog-mode . combobulate-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; LSP configuration

;; Allows linting, formatting, auto-completion, semantic editing, etc.
(use-package lsp-mode
  :ensure t
  :init
  ;; Set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  ;; (setq lsp-keymap-prefix "C-c l") ;; Default: s-l
  ;; (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  ;; (define-key lsp-mode-map (kbd "s-l") nil)
  (setq lsp-idle-delay 0.500) ;; lsp-idle-delay determines how often lsp-mode will refresh.
  (setq lsp-completion-provider :capf)
  (setq lsp-diagnostics-provider :auto) ;;:flymake ;; Use Flymake or Flycheck for diagnostics
  :config
  (lsp-enable-which-key-integration t)
  ;;(setq lsp-client-packages '(lsp-clients lsp-XXX))
  :hook (
	 ((java-mode java-ts-mode) . lsp-deferred)
	 ((clojure-mode clojure-ts-mode) . lsp-deferred)
	 ((css-mode css-ts-mode) . lsp-deferred)
	 ((html-mode html-ts-mode) . lsp-deferred)
	 ((js-mode javascript-mode js2-mode js-ts-mode) . lsp-deferred)
	 ((typescript-mode typescript-ts-mode) . lsp-deferred)
	 ((js-json-mode json-ts-mode) . lsp-deferred)
	 (lsp-completion-mode . corfu-mode)
	 (lsp-completion-at-point-functions . lsp-completion-at-point)
	 (lsp-mode . lsp-lens-mode)
	 (lsp-mode . lsp-enable-which-key-integration)
	 (lsp-mode . lsp-inlay-hints-mode)
	 )
  :commands (lsp lsp-deferred))

;; To determine which server is used for which extension: lsp-language-id-configuration

;; lsp-ui to show higher abtraction interfaces for lsp-mode
(when (package-installed-p 'lsp-mode)
  (use-package lsp-ui
  :ensure t
  :after lsp-mode
  :config
  (setq lsp-ui-sideline-show-code-actions nil)
  :commands lsp-ui-mode)
  )

;; (add-to-list 'lsp-client-packages 'lsp-XXX)

;; lsp-java
(use-package lsp-java
  :ensure t
  :defer t
  :after lsp-mode
  :config
  ;; (add-hook 'java-mode-hook 'lsp)
  ;; (add-hook 'java-ts-mode-hook 'lsp)
  ;; (add-to-list 'lsp-language-id-configuration '(java-ts-mode . "java"))
  
  ;; Here you need a Java compiler to use the lsp server JDTLS. Also, you need to pass the absolute path, not relative.
  (setenv "JAVA_HOME" (file-truename (concat user-emacs-directory "java-lts/jdk-21")))
  (setq lsp-java-java-path (file-truename (concat user-emacs-directory "java-lts/jdk-21/bin/java")))
  )

;; lsp-clojure
(use-package lsp-clojure
  :defer t
  :after lsp-mode)

;; lsp-javascript (javascript / typescript)
(use-package lsp-javascript
  :defer t
  :after lsp-mode)

;; lsp-html
(use-package lsp-html
  :defer t
  :after lsp-mode)

;; lsp-css
(use-package lsp-css
  :defer t
  :after lsp-mode)

;; lsp-css
(use-package lsp-json
  :defer t
  :after lsp-mode)

;; LSP booster
;; To make this code work you need to install the emacs-lsp-booster Rust package
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))

(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Dap mode for debugging
(use-package dap-mode
  :ensure t)

;; Dap-mode
;;(require 'dap-java)
;; (dap-register-debug-template "Java Runner"
;;                              (list :type "java"
;;                                    :request "launch"
;;                                    :args ""
;;                                    :vmArgs "-ea -Dmyapp.instance.name=myapp_1"
;;                                    :projectName "myapp"
;;                                    :mainClass "com.domain.AppRunner"
;;                                    :env '(("DEV" . "1"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Clojure config

;; clojure-mode / clojure-ts-mode
(use-package clojure-mode
  :ensure t
  :defer t
  :commands (clojure-mode))

(use-package clojure-ts-mode
  :ensure t
  :defer t
  :commands (clojure-ts-mode))

(use-package cider
  :ensure t
  :defer t
  :commands (cider-jack-in))

;; Other packages to consider: clj-refactor.el, inf-clojure

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Rust config
(use-package rust-mode
  :ensure t)
  ;; :init
  ;; (setq rust-mode-treesitter-derive t)
  ;; :hook
  ;; (rust-mode . flymake-mode))
  ;; ;; (rust-ts-mode . flymake-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; LaTeX and pdf configuration

;; AUCTeX
(use-package auctex
  :ensure t
  :defer t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  ;; (setq TeX-view-program-selection '(((output-dvi has-no-display-manager) "dvi2tty")
  ;; 				     ((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi")
  ;; 				     (output-pdf "PDF Tools") (output-html "xdg-open"))
  ;; 	) ;; changed (output-pdf "Evince") to (output-pdf "PDF Tools")
  ;; (setq TeX-source-correlate-start-server t)
  ;; (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  ;; (setq TeX-PDF-mode t) ;; use PDFTeX by default
  ;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  ;; (setq reftex-plug-into-AUCTeX t)
  )

;; RefTeX
(use-package reftex ;; Not necessary, just a formality
  :defer t)

;; pdf-tools
(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install) ;; To uninstall use the function `pdf-tools-uninstall'
  ;; Dependencies (Fedora Linux): autoconf automake gcc libpng-devel make poppler-devel poppler-glib-devel zlib-devel
  (setq pdf-info-epdfinfo-program "~/.emacs.d/elpa/pdf-tools-1.1.0/epdfinfo")
  ;;(setq-default pdf-view-display-size 'fit-page)
  ;;(setq pdf-annot-activate-created-annotations t)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; eglot configuration
(use-package eglot
  :ensure nil
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-edits '((eglot-rename . nil)
                                (t . maybe-summary)
                                (t . diff)))
  :bind (:map eglot-mode-map
              ("s-e c a" . eglot-code-actions)
              ("s-e o i" . eglot-code-action-organize-imports)
              ("s-e q f" . eglot-code-action-quickfix)
              ("s-e s d" . eglot-shutdown)
              ("s-e f t" . eglot-find-typeDefinition)
              ("s-e f b" . eglot-format-buffer)
              ("s-e f r" . eglot-format)
              ("s-e r n" . eglot-rename)
              ("s-e r w" . eglot-code-action-rewrite)
              ("s-e r c" . eglot-reconnect)
              )
  :config
  (setopt eglot-send-changes-idle-time 0.5
          eglot-extend-to-xref nil)

  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
                 ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))
              ;; ("rust-analyzer"))))
  :hook
  ((rust-mode rust-ts-mode) . eglot-ensure)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; dape configuration
(use-package dape
  :ensure t
  :config
  (setq dape-buffer-window-arrangement 'right))

;; Put a soft link on debug-adapters directory so dape sees where codelldb is located.
(unless (file-exists-p (expand-file-name "~/.emacs.d/debug-adapters/codelldb/"))
  (make-symbolic-link (expand-file-name "~/.emacs.d/mason/packages/codelldb/") (expand-file-name "~/.emacs.d/debug-adapters/codelldb/")))

;; For now this won't be necessary
;; (add-to-list 'dape-configs
;;              `(codelldb-rust-2 modes (rust-mode rust-ts-mode) command-args
;;                                ("--port" :autoport "--settings"
;;                                 "{\"sourceLanguages\":[\"rust\"]}")
;;                                ensure dape-ensure-command command-cwd
;;                                dape-command-cwd command "codelldb" port :autoport :type "lldb"
;;                                :request "launch" :cwd "." :program
;;                                (file-name-concat "target" "debug"
;;                                                  (car
;;                                                   (last
;;                                                    (file-name-split
;;                                                     (directory-file-name (dape-cwd))))))))
