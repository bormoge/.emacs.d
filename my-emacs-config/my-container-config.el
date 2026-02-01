;; checking for missing cli
(defun missing-cli (cli-list)
  (seq-filter
   (lambda (cli)
     (not (executable-find cli)))
   cli-list))

(defun cli-sanity-check (cli-list)
  (let ((mcli (missing-cli cli-list)))
    (when mcli
      (display-warning
       'environment
       (format "Missing CLI commands: %s"
               (string-join mcli ", "))
       :warning))))

(cli-sanity-check '("rustup" "uv" "java" "node"))

;; LSP, DAP, linter and formatter installer
(use-package mason
  :ensure t
  :config
  (mason-ensure
   (lambda ()
     (dolist (pkg
              '("rassumfrassum"
                "rust-analyzer"
                "codelldb"
                "jdtls"
                "java-debug-adapter"
                ))
       (unless (mason-installed-p pkg)
	 (ignore-errors (mason-install pkg))))))
  (mason-setup))

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

;; ;;;; typescript-mode config
;; (use-package typescript-mode
;;   :ensure t
;;   :defer t
;;   :config
;;   (require 'ansi-color)
;;   (defun colorize-compilation-buffer ()
;;     (ansi-color-apply-on-region compilation-filter-start (point-max)))
;;   (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
;;   )

;; ;; tide package config
;; (use-package tide
;;   :ensure t
;;   :defer t
;;   :config
;;   (defun setup-tide-mode ()
;;     (interactive)
;;     (tide-setup)
;;     (eldoc-mode +1)
;;     (tide-hl-identifier-mode +1))

;;   ;; ;; formats the buffer before saving
;;   ;; (add-hook 'before-save-hook 'tide-format-before-save)

;;   ;; if you use typescript-mode
;;   (add-hook 'typescript-mode-hook #'setup-tide-mode)
;;   ;; if you use treesitter based typescript-ts-mode (emacs 29+)
;;   (add-hook 'typescript-ts-mode-hook #'setup-tide-mode)
;;   )

;; ;; web-mode package config
;; (use-package web-mode
;;   :ensure t
;;   :defer t
;;   )

;; ;;;; tsx-mode config
;; (use-package tsx-mode
;;   :vc (:url "https://github.com/orzechowskid/tsx-mode.el"
;;        :rev :newest
;;        :branch "emacs30"
;;        :vc-backend Git)
;;   :ensure t
;;   :defer t
;;   )

;; ;; json-mode config
;; (use-package json-mode
;;   :ensure t
;;   :defer t
;;   )

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
  ;; Enable cache busting, depending on if your server returns
  ;; sufficiently many candidates in the first place.
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

  (setopt eglot-send-changes-idle-time 0.5
          eglot-extend-to-xref nil)

  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
                 ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))

  ;; You need a Java compiler to use the lsp server JDTLS.
  ;; Also, you need to concat the absolute path, not relative.
  ;; (setenv "PATH" (concat (expand-file-name "~/.emacs.d/java-lts/jdk-21/bin:") (getenv "PATH")))

  ;; You can find the settings for JDTLS here: marketplace dot visualstudio dot com / items ?itemName=redhat.java

  (add-to-list 'eglot-server-programs
               `((java-mode java-ts-mode) .
                 ("jdtls"
                  :initializationOptions
                  (:bundles
                   ;; This needs to be the absolute path to java-debug-adapter
                   [,(expand-file-name "~/.emacs.d/mason/packages/java-debug-adapter/extension/server/com.microsoft.java.debug.plugin-0.53.2.jar")]
                   :settings
                   (:java
                    (:format
                     (:settings
                      (:url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")
                      :enabled t)
                     :completion
                     (:maxResults "100")))))))

  ;;   (add-to-list 'eglot-server-programs
  ;;              `((java-mode java-ts-mode) .
  ;;                ("jdtls"
  ;;                 :initializationOptions
  ;;                 (:bundles
  ;;                  ;; This needs to be the absolute path to java-debug-adapter
  ;;                  [,(expand-file-name "~/.emacs.d/mason/packages/java-debug-adapter/extension/server/com.microsoft.java.debug.plugin-0.53.2.jar")]))))

  ;; (setq-default eglot-workspace-configuration
  ;;               `(:java
  ;;                 (:format
  ;;                  (:settings
  ;;                   (:url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")
  ;;                   :enabled t)
  ;;                  :completion
  ;;                  (:maxResults 70))))

  :hook
  ((rust-mode rust-ts-mode) . eglot-ensure)
  ((java-mode java-ts-mode) . eglot-ensure)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; dape configuration
(use-package dape
  :ensure t
  :config
  (setq dape-buffer-window-arrangement 'right))

;; Put symlinks on debug-adapters directory so dape sees where the debuggers are located.
(unless (file-exists-p (expand-file-name "~/.emacs.d/debug-adapters/codelldb/"))
  (make-symbolic-link (expand-file-name "~/.emacs.d/mason/packages/codelldb/") (expand-file-name "~/.emacs.d/debug-adapters/codelldb/")))

(unless (file-exists-p (expand-file-name "~/.emacs.d/debug-adapters/com.microsoft.java.debug.plugin-0.53.2.jar"))
  (make-symbolic-link (expand-file-name "~/.emacs.d/mason/packages/java-debug-adapter/extension/server/com.microsoft.java.debug.plugin-0.53.2.jar") (expand-file-name "~/.emacs.d/debug-adapters/com.microsoft.java.debug.plugin-0.53.2.jar")))

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
