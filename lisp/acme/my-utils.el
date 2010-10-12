;; Time-stamp: <2010-10-11 17:03:04 cmauclai>


;; ** No-errors **
(defmacro no-errors (&rest body)
  "Ignore errors in BODY."
  `(condition-case nil (progn ,@body) (error nil)))

(defmacro no-errors-beep (&rest body)
  "Ignore errors in BODY, but beep if there were."
  `(condition-case nil (progn ,@body) (error (beep))))


;; ** Search & filter in lists **
(defun my:filter-list (pred list)
  "Returns the list of values from LIST that PRED verifies."
  (let ((r nil))
    (dolist (item list (nreverse r))
      (when (funcall pred item) (push item r)))))

(defun my:find-first (pred list &optional default)
  "Finds the first element that satisfies PRED in LIST, or DEFAULT if none."
  (catch 'done
    (dolist (x list)
      (when (funcall pred x) (throw 'done x)))
    default))


;; ** Compiling **
(defun my:byte-compile-this-file ()
  "Compile the file the buffer is visiting."
  (interactive)
  (byte-compile-file (expand-file-name buffer-file-name)))


;; ** Shell **
(defun my:shell-term (terminal)
  (interactive)
  (defvar term-default-bg-color (face-attribute 'default :background))
  (defvar term-default-fg-color (face-attribute 'default :foreground))
  (term terminal))



;; Local Variables:
;; eval:(set-regexp-face "^\s*;;\\([{}]*\\)?\\( =+ .* =+\\)?$" 'VioletRed4-bold-italic)
;; eval:(set-regexp-face "^\s*;;\\([{}]*\\)?\\( \\*+ .* \\*+\\)?$" 'Green4-bold-italic)
;; eval:(set-regexp-face "^\s*;;\\([{}]*\\)?\\( \\[+ .* \\]+\\)?$" 'h00688b-bold-italic)
;; End:
