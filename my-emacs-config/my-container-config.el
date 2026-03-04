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
              '(
                "clangd"
                "cljfmt"
                "clojure-lsp"
                "codelldb"
                "css-lsp"
                ;; "debugpy" ;; couldn't make mason-installed debugpy work, so I ended up installing it through uv.
                "html-lsp"
                "java-debug-adapter"
                "jdtls"
                "js-debug-adapter"
                "prettier"
                "rassumfrassum"
                "ruff"
                "rust-analyzer"
                "tombi"
                "ty"
                "typescript-language-server"
                "yaml-language-server"
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
  :if (executable-find "uv")
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

;; Note to myself: look how to configure it.
(use-package apheleia
  :ensure t
  :config
  ;; (apheleia-global-mode)
  :bind (
         ("s-<0x10081247> s-A" . 'apheleia-format-buffer)
         )
  )

(use-package disaster
  :ensure t
  :defer t
  :config
  ;; (define-key fortran-mode-map (kbd "s-d s") 'disaster)
  )

(use-package cc-mode
  :config
  (define-key c-mode-map (kbd "s-d s") 'disaster)
  (define-key c++-mode-map (kbd "s-d s") 'disaster)
  (setq-default c-basic-offset 4)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; json config
(use-package json-mode
  :ensure t
  :defer t
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

;;;; yaml config
(use-package yaml
  :ensure t)

(use-package yaml-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Clojure config

;; clojure-mode / clojure-ts-mode
(use-package clojure-mode
  :ensure t
  :defer t
  :custom
  (clojure-indent-style 'always-align)
  :commands (clojure-mode)
  )

(use-package clojure-ts-mode
  :ensure t
  :defer t
  :commands (clojure-ts-mode)
  )

(use-package cider
  :ensure t
  :after (clojure-mode)
  :custom
  (cider-preferred-build-tool nil) ;clj
  :config
  (setq cider-repl-pop-to-buffer-on-connect nil)
  (setq cider-show-error-buffer t)
  (setq cider-auto-select-error-buffer t)
  (setq cider-repl-history-file nil)
  (setq cider-repl-wrap-history nil)
  (setq cider-prompt-for-symbol nil)
  (setq cider-repl-use-pretty-printing t)
  (setq nrepl-log-messages nil)
  (setq cider-font-lock-dynamically (macro core deprecated))
  (setq cider-eldoc-display-for-symbol-at-point t)
  (setq cider-prompt-for-symbol nil)
  (setq cider-use-xref t)
  :commands (cider cider-jack-in)
  )

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Rust config
(use-package rust-mode
  :ensure t)
;; :init
;; (setq rust-mode-treesitter-derive t)
;; ;; (setq rust-format-on-save t) ;; You can also use Apheleia as an alternative for this variable.
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
