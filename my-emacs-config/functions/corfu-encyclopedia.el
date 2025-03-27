;; corfu-encyclopedia.el
;; Package to view documentation about an element in a Corfu menu.

(require 'corfu)

(defvar-local corfu-encyclopedia-selected-candidate nil)
(defvar-local corfu-encyclopedia-documentation-flag nil)

(defun corfu-encyclopedia-get-selected-candidate ()
  (setq corfu-encyclopedia-selected-candidate (concat corfu--base (nth corfu--index corfu--candidates)))
  corfu-encyclopedia-selected-candidate)

(defun corfu-encyclopedia-print-selected-candidate ()
  (interactive)
  (message (corfu-encyclopedia-get-selected-candidate)))

(defun corfu-encyclopedia-view-documentation ()
  (interactive)
  (setq corfu-encyclopedia-documentation-flag t)
  (corfu-encyclopedia-get-selected-candidate)
  (cond
   ((fboundp (intern corfu-encyclopedia-selected-candidate))
    (message "This is a function: %s." corfu-encyclopedia-selected-candidate)
    (describe-function (intern corfu-encyclopedia-selected-candidate)))
   ((boundp (intern corfu-encyclopedia-selected-candidate))
    (message "This is a variable: %s." corfu-encyclopedia-selected-candidate)
    (describe-variable (intern corfu-encyclopedia-selected-candidate)))
   (t (message "No documentation was found using both fboundp and boundp."))))

(defun corfu-encyclopedia-go-down ()
  (interactive)
  (corfu-next 1)
  (when corfu-encyclopedia-documentation-flag
    (corfu-encyclopedia-view-documentation)))

(defun corfu-encyclopedia-go-up ()
  (interactive)
  (corfu-previous 1)
  (when corfu-encyclopedia-documentation-flag
    (corfu-encyclopedia-view-documentation)))

(defun corfu-encyclopedia-quit-list ()
  (interactive)
  (when corfu-encyclopedia-documentation-flag
    (when (get-buffer "*Help*")
      (progn
	(switch-to-buffer "*Help*")
	(kill-buffer-and-window))))
  (setq corfu-encyclopedia-documentation-flag nil)
  (corfu-quit))

(defvar-local corfu-encyclopedia-store-down-key nil)
(defvar-local corfu-encyclopedia-store-up-key nil)
(defvar-local corfu-encyclopedia-store-escape-key nil)

(defvar-keymap corfu-encyclopedia-mode-map
  "H-d" #'corfu-encyclopedia-view-documentation)

(defun corfu-encyclopedia-enable-package-hook ()
  "Enable keybindings for corfu-encyclopedia."
  (setq corfu-encyclopedia-store-down-key (key-binding (kbd "<down>")))
  (setq corfu-encyclopedia-store-up-key (key-binding (kbd "<up>")))
  (setq corfu-encyclopedia-store-escape-key (key-binding (kbd "<escape>")))
  (define-key corfu-map (kbd "<down>") nil)
  (define-key corfu-map (kbd "<up>") nil)
  (define-key corfu-map (kbd "<escape>") nil)
  (define-key corfu-map (kbd "<down>") #'corfu-encyclopedia-go-down)
  (define-key corfu-map (kbd "<up>") #'corfu-encyclopedia-go-up)
  (define-key corfu-map (kbd "<escape>") #'corfu-encyclopedia-quit-list))

(defun corfu-encyclopedia-disable-package-hook ()
  "Restore original keybindings for Corfu."
  (define-key corfu-map (kbd "<down>") nil)
  (define-key corfu-map (kbd "<up>") nil)
  (define-key corfu-map (kbd "<escape>") nil)
  (define-key corfu-map (kbd "<down>") corfu-encyclopedia-store-down-key)
  (define-key corfu-map (kbd "<up>") corfu-encyclopedia-store-up-key)
  (define-key corfu-map (kbd "<escape>") corfu-encyclopedia-store-escape-key))

(define-minor-mode corfu-encyclopedia-mode ()
  "Minor mode to always show a candidate's documentation when using Corfu."
  :init-value nil
  (if corfu-encyclopedia-mode
      (corfu-encyclopedia-enable-package-hook)
    (corfu-encyclopedia-disable-package-hook))
  )
