;; Recompile packages if necessary
;; (byte-recompile-directory package-user-dir nil 'force)

;;(use-package magit
;;  :ensure t)

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

;; Used to highlight lines changed
(use-package diff-hl
  :ensure t)
(require 'diff-hl)

;; Package used to manage git
(use-package magit
  :ensure t)
(require 'magit)

;;(package-initialize)
