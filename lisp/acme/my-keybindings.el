;; Time-stamp: <2010-04-08 13:47:01 cmauclai>


(defun my:define-keys (&rest keys/funcs-or-maps)
  "Define many keys.
The KEYS/FUNCS-OR-MAPS arguments are a list of:
  - (KEY (LIST OF FUNC))     - bind KEY to a lambda function wrapping LIST OF FUNC (not yet functionnal)
  - (KEY FUNC)               - bind KEY to function FUNC
  - (KEY FUNC ARG1 ARG2 ...) - bind KEY to function (FUNC ARG1 ARG2 ...)
  - (FUNC1 FUNC2)            - replace FUNC1 globally-bound keys with FUNC2 in the map
  - (FUNC1 FUNC2 t)          - same but replace locally bound FUNC1 keys
  - keymap                   - use this keymap from now on.
  - keymap-symbol            - a symbol that should evaluate to a keymap, 'global or
    'local (error if the symbol's value is not a keymap).
  - nil                      - silently ignored."
  (let ((keymap (current-global-map)) x)
    (while keys/funcs-or-maps
      (setq x (car keys/funcs-or-maps)
            keys/funcs-or-maps (cdr keys/funcs-or-maps))
      (cond
       ((null x) nil) ; this allow nils to appear (good for conditionals)
       ((keymapp x) (setq keymap x))
       ((symbolp x) (setq keymap (cond ((eq x 'global) (current-global-map))
                                       ((eq x 'local) (current-local-map))
                                       ((boundp x) (eval x))
                                       (t nil)))
        (when (and keymap (not (keymapp keymap)))
          (error "%S did not evaluate to a keymap" x)))
       ;; makes keymap symbols that are unbound have no effect
       ((null keymap) nil)
       ((and (listp x) (symbolp (car x)) (commandp (car x)))
        (substitute-key-definition
         (car x) (cadr x) keymap
         (if (eq t (nth 2 x)) keymap (current-global-map))))
       ((and (listp x) (= (length x) 2) (stringp (car x)))
        (eval `(define-key keymap (kbd ,(car x)) (cadr x))))
       ((and (listp x) (= (length x) 2))
        (eval `(define-key keymap ,(car-safe x) (cadr x))))
       ((and (listp x) (> (length x) 2) (stringp (car x)))
        (eval `(define-key keymap (kbd ,(car x))
                 (lambda (arg) (interactive "p") ,(cdr x)))))
       (t
        (eval `(define-key keymap ,(car-safe x)
                 (lambda (arg) (interactive "p") ,(cdr x)))))))))

(defun my:get-key (prompt &optional not-this-command)
  "Like `read-key-sequence-vector', but arranges for the cursor to be in the
echo area, does the usual thing for C-g, and possibly returns nil if you use the
same key as for `this-command'."
  (let* ((cursor-in-echo-area t)
         (key (read-key-sequence-vector prompt nil t))
         (bind (key-binding key)))
    (message nil)
    (cond ((and not-this-command (eq bind this-command)) nil)
          ((eq bind 'keyboard-quit) (keyboard-quit))
          (t key))))


;; Local Variables:
;; eval:(set-regexp-face "^\s*;;\\([{}]*\\)?\\( =+ .* =+\\)?$" 'VioletRed4-bold-italic)
;; eval:(set-regexp-face "^\s*;;\\([{}]*\\)?\\( \\*+ .* \\*+\\)?$" 'Green4-bold-italic)
;; eval:(set-regexp-face "^\s*;;\\([{}]*\\)?\\( \\[+ .* \\]+\\)?$" 'h00688b-bold-italic)
;; End:
