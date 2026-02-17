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

(cli-sanity-check '("rustup" "uv" "java" "node")) ;"asdf"

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
                "typescript-language-server"
                "js-debug-adapter"
                "ruff"
                "ty"
                ;; "debugpy" ;; couldn't make mason-installed debugpy work, so I ended up installing it through uv.
                "tombi"
                "quick-lint-js"
                "prettier"
                "clangd"
                "html-lsp"
                "css-lsp"
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
  :hook (python-ts-mode . uv-mode-auto-activate-hook)
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

;; Note to myself: look how to configure it.
(use-package apheleia
  :ensure t
  :config
  ;; (apheleia-global-mode)
  :bind (
         ("s-<0x10081247> s-a" . 'apheleia-format-buffer)
         )
  )

(use-package disaster
  :ensure t
  :defer t
  :config
  ;; (define-key fortran-mode-map (kbd "s-d") 'disaster)
  )

(use-package cc-mode
  :config
  (define-key c-mode-map (kbd "s-d") 'disaster)
  (define-key c++-mode-map (kbd "s-d") 'disaster)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; toml config
(use-package toml-mode
  :ensure t
  :defer t
  :config
  (require 'tomlparse)
  )

(use-package toml-ts-mode
  :ensure t
  :defer t
  :config
  (require 'tomlparse)
  )

(use-package toml
  :ensure t
  :defer t
  )

(use-package tomlparse
  :ensure t
  :defer t
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

;;;; Elixir / Erlang config

;; Elixir
;; (use-package elixir-mode
;;   :ensure t
;;   :defer t
;;   )

;; (use-package mix
;;   :ensure t
;;   :defer t
;;   :config
;;   (add-hook 'elixir-mode-hook 'mix-minor-mode)
;;   (add-hook 'elixir-ts-mode-hook 'mix-minor-mode)
;;   )

;; (use-package exunit
;;   :ensure t
;;   :defer t
;;   :config
;;   (add-hook 'elixir-mode-hook 'exunit-mode)
;;   (add-hook 'elixir-ts-mode-hook 'exunit-mode)
;;   )

;; (use-package inf-elixir
;;   :ensure t
;;   :defer t
;;   :bind (("C-c i i" . 'inf-elixir)
;;          ("C-c i p" . 'inf-elixir-project)
;;          ("C-c i l" . 'inf-elixir-send-line)
;;          ("C-c i r" . 'inf-elixir-send-region)
;;          ("C-c i b" . 'inf-elixir-send-buffer)
;;          ("C-c i R" . 'inf-elixir-reload-module))
;;   )

;; (use-package ob-elixir
;;   :ensure t
;;   :defer t
;;   :config
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '((emacs-lisp . t)
;;      (elixir . t)))
;;   )

;; (use-package heex-ts-mode
;;   :ensure t
;;   :defer t
;;   )

;; Erlang
;; (use-package erlang
;;   :ensure t
;;   :defer t
;;   :config
;;   ;; (setq load-path (cons "/usr/local/otp/lib/tools/emacs" ;;"/usr/local/otp/lib/tools-<ToolsVer>/emacs"
;;   ;;                       load-path))
;;   ;; (setq erlang-root-dir "/usr/local/otp")
;;   ;; (setq exec-path (cons "/usr/local/otp/bin" exec-path))
;;   ;; (require 'erlang-start)
;;   ;; (require 'erlang-flymake)
;;   )

;; (use-package erlang-ts
;;   :ensure t
;;   ;; :mode ("\\.erl\\'" . erlang-ts-mode)
;;   :defer t
;;   )

;; (use-package edts
;;   :ensure t
;;   :defer t
;;   )

;; (use-package ob-erlang
;;   :vc (:url "https://github.com/B7rian/ob-erlang"
;;        :rev :newest
;;        :branch "master"
;;        :vc-backend Git)
;;   :ensure t
;;   :defer t
;;   )

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

;; json-mode config
(use-package json-mode
  :ensure t
  :defer t
  )

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
  ;; For more info about dependencies check ~/.emacs.d/elpa/pdf-tools-1.3.0/server/autobuild
  ;; Dependencies (Fedora Linux): autoconf automake gcc libpng-devel make poppler-devel poppler-glib-devel zlib-devel
  ;; Dependencies (NixOS Linux): automake autoconf pkg-config libpng zlib poppler
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
  (eglot-confirm-server-edits '((eglot-rename . nil) (t . maybe-summary) (t . diff)))
  (eglot-sync-connect 2)
  (eglot-connect-timeou1t 30)
  (eglot-ignored-server-capabilities nil); Examples: '(:inlayHintProvider), '(:documentHighlightProvider)'
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


  (add-to-list 'eglot-server-programs
               '(((js-mode :language-id "javascript")
                  (js-ts-mode :language-id "javascript")
                  (js-jsx-mode :language-id "javascriptreact")
                  (tsx-ts-mode :language-id "typescriptreact")
                  (typescript-ts-mode :language-id "typescript")
                  (typescript-mode :language-id "typescript")) . ("rass" "--" "typescript-language-server" "--stdio" "--" "quick-lint-js" "--lsp-server")))

  (add-to-list 'eglot-server-programs
               '((python-ts-mode python-mode) .
                 ("rass" "--" "ty" "server" "--" "ruff" "server")))

  (add-to-list 'eglot-server-programs
               '((toml-ts-mode toml-mode) .
                 ("tombi" "lsp"))) ;; "lint" "format" "completion"

  ;; (add-to-list 'eglot-server-programs
  ;;              '((c++-mode c-mode) .
  ;;                ("clangd")))

  :hook
  ((rust-mode rust-ts-mode) . eglot-ensure)
  ((java-mode java-ts-mode) . eglot-ensure)
  ((js-mode js-ts-mode) . eglot-ensure)
  ((python-mode python-ts-mode) . eglot-ensure)
  ((toml-mode toml-ts-mode) . eglot-ensure)
  ((c-mode c++-mode) . eglot-ensure)
  ((html-mode html-ts-mode) . eglot-ensure)
  ((css-mode css-ts-mode) . eglot-ensure)
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

(unless (file-exists-p (expand-file-name "~/.emacs.d/debug-adapters/js-debug-adapter"))
  (make-symbolic-link (expand-file-name "~/.emacs.d/mason/bin/js-debug-adapter") (expand-file-name "~/.emacs.d/debug-adapters/js-debug-adapter")))

;; (setenv "PATH" (concat "~/.emacs.d/mason/bin:" (getenv "PATH")))
;; (setq exec-path (cons "~/.emacs.d/mason/bin" exec-path))

(add-to-list 'dape-configs
             `(js-debug-adapter
               modes (js-mode js-ts-mode typescript-mode typescript-ts-mode)
               command "node"
               command-args (,(expand-file-name "~/.emacs.d/mason/packages/js-debug-adapter/js-debug/src/dapDebugServer.js") "8123")
               :type "pwa-node"
               :request "launch"
               :cwd "${workspaceFolder}"
               :program (if (buffer-file-name) ;; if buffer is visiting file, return file name, else nil
                            (setq js-debug-buffer-filename (buffer-file-name)) ;; if buffer-file-name = non-nil set the file name
                          js-debug-buffer-filename) ;; else either use nil or assume last file visited is target
               ;; for some reason ${file} didn't work on :program
               ensure dape-ensure-command command-cwd
               dape-command-cwd command "js-debug-adapter"
               host "localhost"
               port 8123))

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
