;; corfu-view-candidate-documentation.el
;; Package to view documentation about an element in a Corfu menu.


(defvar corfu-selected-candidate nil)

(defun corfu-get-selected-candidate ()
  (setq corfu-selected-candidate (concat corfu--base (nth corfu--index corfu--candidates)))
  corfu-selected-candidate)

(defun corfu-print-selected-candidate ()
  (interactive)
  (message (corfu-get-selected-candidate)))

(defun corfu-view-function-or-variable-documentation ()
  (interactive)
  (corfu-get-selected-candidate)
  (cond
   ((fboundp (intern corfu-selected-candidate))
    (message "This is a function: %s." corfu-selected-candidate)
    (describe-function (intern corfu-selected-candidate)))
   ((boundp (intern corfu-selected-candidate))
    (message "This is a variable: %s." corfu-selected-candidate)
    (describe-variable (intern corfu-selected-candidate)))
   (t (message "No documentation was found using both fboundp and boundp."))))
