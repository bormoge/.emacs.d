;; Recompile packages if necessary
;; (byte-recompile-directory package-user-dir nil 'force)

;; Priority for installation
(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
	("nongnu"       . "https://elpa.nongnu.org/nongnu/")
        ("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities
      '(("gnu"          . 4)
        ("nongnu"       . 3)
        ("melpa"        . 2)
        ("melpa-stable" . 1)))

;; Doom themes
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dark+ t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; default "doom-atom"; use "doom-colors" for less minimal icon theme
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; Used to highlight lines changed
(use-package diff-hl
  :ensure t)

;; Package used to manage git
(use-package magit
  :ensure t)

;; Package used to manage git forges (GitHub, GitLab, Codeberg, etc...)
(use-package forge
  :ensure t
  :defer t
  :after magit)

;; Project manager
(use-package projectile
  :ensure t
  :init
  (setq projectile-project-search-path '("~/projects/" "~/work/" "~/playground"))
  :config
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
  (global-set-key (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  )

;; File and project explorer
;; Most of this config is taken from Alexander-Miller's repository
(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; Note from treemacs author Alexander-Miller:
    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

;;    (treemacs-hide-gitignored-files-mode nil)
    )
  :bind
  (:map global-map
        ("H-0"       . treemacs-select-window)
        ("H-x t 1"   . treemacs-delete-other-windows)
        ("H-x t t"   . treemacs)
        ("H-x t d"   . treemacs-select-directory)
        ("H-x t B"   . treemacs-bookmark)
        ("H-x t C-t" . treemacs-find-file)
        ("H-x t M-t" . treemacs-find-tag))
  
  ) ;; Yes, this is the end of the use-package treemacs.

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package all-the-icons ;; IMPORTANT: to see the icons you need to install them using all-the-icons-install-fonts
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package treemacs-icons-dired
  :after (all-the-icons-dired)
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

;; (treemacs-start-on-boot)

;; Treemacs integration with LSP
(use-package lsp-treemacs
  :ensure t
  :after lsp
  :commands lsp-treemacs-errors-list)

;; Vim mode for Emacs
(use-package evil
  :ensure t
  :defer t)

;; Minimap
(use-package minimap
  :ensure t)

;; YASnippet for shortcuts
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets"))
  )

;; Packages to manage PostgreSQL
;; To upgrade use package-vc-upgrade
(unless (package-installed-p 'pg)
  (package-vc-install "https://github.com/emarsden/pg-el" nil nil 'pg))
(unless (package-installed-p 'pgmacs)
  (package-vc-install "https://github.com/emarsden/pgmacs" nil nil 'pgmacs))

(require 'pg)
(require 'pgmacs)

;; Package to fold code
;; To upgrade use package-vc-upgrade
(unless (package-installed-p 'treesit-fold)
  (package-vc-install "https://github.com/emacs-tree-sitter/treesit-fold" nil nil 'treesit-fold))

(require 'treesit-fold)

;; Syntax checking using Flycheck
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

;; Focus on selected text
(use-package focus
  :ensure t)

(use-package lsp-focus
  :ensure t
  :after (lsp focus))

;; Multiline cursors.
(use-package multiple-cursors
  :ensure t
  :defer t)

;; Loading, configuring, and ensuring that org-mode is installed.
(use-package org
  :ensure t
  :defer t
  ;;:config
  ;; (setq org-startup-indented t
  ;;       org-hide-emphasis-markers t
  ;;       org-agenda-files '("~/org/"))
  ;; (org-babel-do-load-languages
  ;;  'org-babel-load-languages
  ;;  '((js . t)))
  )

(use-package kind-icon
  :ensure t
  :after corfu
  ;:custom
  ;;(kind-icon-use-icons nil)
  ;;(kind-icon-blend-background t)
  ;;(kind-icon-default-face 'corfu-default) ; only needed with blend-background
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; avy
;; Note to self: it has embark integration
;; TBW

;; ledger-mode
(use-package ledger-mode
  :ensure t
  :defer t)

;; flycheck intregration for ledger-mode
(use-package flycheck-ledger
  :ensure t
  :after (flycheck ledger))

;; Dashboard for Emacs
;; (use-package dashboard
;;   :ensure t
;;   :defer t
;;   :config
;;   (dashboard-setup-startup-hook)
;;   (setq dashboard-startup-banner 'logo))

;; Automatically show available commands
(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 10.0)
  (which-key-setup-side-window-right-bottom)
  (which-key-mode))

;; Directory-specific environments
(use-package direnv
  :ensure t
  :defer t
  ;; :config
  ;; (direnv-mode)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Corfu + Cape + Vertico + Consult + Marginalia + Orderless + Embark (minad-oantolin stack AKA the corfuverse)
;; There is also TempEl, an alternative to YASnippet developed by minad.

;; Corfu

(use-package corfu
  :ensure t
  :hook (((java-mode
	  java-ts-mode
	  emacs-lisp-mode) . corfu-mode)
	 )
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.2)
  (corfu-preselect 'first)
  (corfu-preview-current 'insert)
  ;; For Emacs 30 and newer
  ;; (text-mode-ispell-word-completion nil)
  :bind
  (:map corfu-map
        ("TAB" . corfu-insert)
        ([tab] . corfu-insert)
        ("ESC" . corfu-quit)
        ([esc] . corfu-quit)
	("H-<tab>" . yas-expand)
	("H-d" . corfu-popupinfo-mode)
	("H-t" . corfu-popupinfo-toggle)
	))

;; Cape

;; (use-package cape
;;   ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
;;   ;; Press C-c p ? to for help.
;;   :bind (("C-c C-r m" . cape-prefix-map)
;; 	 ("C-c C-r d" . cape-dabbrev)
;; 	 ("C-c C-r h" . cape-history)
;; 	 ("C-c C-r f" . cape-file))
;;   :init
;;   ;; Add to the global default value of `completion-at-point-functions' which is
;;   ;; used by `completion-at-point'.  The order of the functions matters, the
;;   ;; first function returning a result wins.  Note that the list of buffer-local
;;   ;; completion functions takes precedence over the global list.
;;   (add-hook 'completion-at-point-functions #'cape-dabbrev)
;;   (add-hook 'completion-at-point-functions #'cape-file)
;;   (add-hook 'completion-at-point-functions #'cape-elisp-block)
;;   (add-hook 'completion-at-point-functions #'cape-history)
;; )


;; Vertico
;; TBW


;; Consult
;; TBW


;; Marginalia
;; TBW


;; Orderless

(use-package orderless
  :ensure t
  ;;:after vertico
  :custom
  ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic)) ;;partial-completion
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))) ;;basic
  )


;; Embark
;; TBW


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Allows linting, formatting, auto-completion, semantic editing, etc.
(use-package lsp-mode
  :ensure t
  :init
  ;; Set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-idle-delay 0.500) ;; lsp-idle-delay determines how often lsp-mode will refresh.
  (setq lsp-completion-provider :capf)
  :config
  (lsp-enable-which-key-integration t)
  ;;(setq lsp-client-packages '(lsp-clients lsp-XXX))
  :hook (((java-mode
	   java-ts-mode) . lsp-deferred)
	 (lsp-completion-mode-hook . corfu-mode)
	 (lsp-completion-at-point-functions . lsp-completion-at-point)
	 (lsp-mode . lsp-lens-mode)
	 (lsp-mode . lsp-enable-which-key-integration)
	 )
  :commands (lsp lsp-deferred))

;; lsp-ui to show higher abtraction interfaces for lsp-mode
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode)

;; lsp clients: To Be Installed
;; lsp-python
;; lsp-javascript / lsp-typescript

;; (add-to-list 'lsp-client-packages 'lsp-XXX)

;; lsp-java
(use-package lsp-java
  :ensure t
  :after lsp-mode
  :config
  ;; (add-hook 'java-mode-hook 'lsp)
  ;; (add-hook 'java-ts-mode-hook 'lsp)
  ;; (add-to-list 'lsp-language-id-configuration '(java-ts-mode . "java"))
  )

;; Here you need a Java compiler to use the lsp server JDTLS. Also, you need to pass the absolute path, not relative.
(setenv "JAVA_HOME" (file-truename (concat user-emacs-directory "java-lts/jdk-21")))
(setq lsp-java-java-path (file-truename (concat user-emacs-directory "java-lts/jdk-21/bin/java")))

;; LSP booster
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


;; Treesitter

;; Treesitter grammar repositories.
(setq treesit-language-source-alist
      '((java "https://github.com/tree-sitter/tree-sitter-java")))

;; Replace normal mode with its equivalent treesitter mode (ts-mode).
(setq major-mode-remap-alist
      '((java-mode . java-ts-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Packages configuration

;; diff-hl
;; Activate in all buffers diff-hl.
(global-diff-hl-mode)

;; To integrate diff-hl with magit.
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; Magit
;; Add tracked files to magit-status.
(magit-add-section-hook
   'magit-status-sections-hook
   'magit-insert-tracked-files
   nil
   'append)

;; Minimap
;; Minimap config. By default the package is globally activated, but it only works on modes derived from 'prog-mode'.
(minimap-mode 1)
(setq minimap-window-location 'right)
(setq minimap-width-fraction '0.10)

;; evil-mode
;; Set C-z for evil-mode
(global-unset-key (kbd "C-z")) ;; Originally suspend-frame, it also uses C-x C-z
(global-set-key (kbd "C-z") 'evil-mode)

;; YASnippets
;; Remap the snippet expansion from TAB to H-TAB
(define-key yas-minor-mode-map [(tab)] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "H-<tab>") 'yas-expand)

;; Treemacs
;; Increase zoom for all modes except treemacs
(add-hook 'after-change-major-mode-hook
	  (lambda ()
	    (unless (derived-mode-p 'treemacs-mode)
	      (text-scale-set 3))))

;; Focus (elements it can focus: org-element, paragraph, sentence, sexp, symbol, word)
(add-to-list 'focus-mode-to-thing '(java-ts-mode . paragraph))
(add-hook 'focus-mode-hook #'lsp-focus-mode)

;; multiple-cursors.el
;; Add a cursor to each line of an active region.
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; Add multiple cursors not based on continuous lines, but based on keywords.
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C-<mouse-1>") 'mc/add-cursor-on-click)

;; Dashboard
;; Items to show
;; (setq dashboard-items '((recents   . 30)
;;                         (projects  . 5)))

;; (setq dashboard-item-shortcuts '((recents   . "r")
;;                                  (projects  . "p")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
