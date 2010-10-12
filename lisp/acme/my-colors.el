;; Time-stamp: <2010-04-08 13:46:49 cmauclai>


;; ** Helper functions **
(defun simple-make-face (spec &optional face-name)
  "Get a symbol or a string SPEC, and make it into a face if it doesn't exist.
In order for it to properly create the face, the following naming convention
must be used:
    attr[-attr][...]
Example: (simple-make-face 'yellow/blue4-bold) creates and returns an
appropriate face named `yellow/blue4-bold'.  (Actually, the color spec can be
anywhere.)

Attribute can be one of: ultra_condensed extra_condensed condensed
semi_condensed semi_expanded expanded extra_expanded ultra_expanded ultra_bold
extra_bold bold semi_bold semi_light light extra_light ultra_light underline
ununderline overline unoverline inverse uninverse italic oblique reverse_italic
reverse_oblique.  It can also be `normal', which will be used for width,
weight, and slant (but attributes are processed from left to right).
\(See `set-face-attribute'.)

Attributes can also be in these forms:
* `fgcolor/bgcolor', where each color can be
  - a color name,
  - `hexH' (H is some hex color), or `hH', or `#H'
  - `*' or `default', or just empty
* `box[N]' for a box with width N, defaults to 1
* `big[N]', `small[N]' for a multiplier or divider of N0%, defaults to 25%
* `scale[N]' (for a scale of N%)

An optional argument FACE-NAME will make it be defined as the result face, and
force the face to be modified if it exists (good for setting properties of
existing faces)."
  (let* ((spec  (if (stringp spec) spec (symbol-name spec)))
         (spec  (if (equal "" spec) (error "Empty face spec") spec))
         (face  (or face-name (intern spec)))
         (attrs (split-string spec "-"))
         (attrs (mapcar (lambda (a) (replace-regexp-in-string "_" "-" a t))
                        attrs))
         (error nil))
    ;; optimize re-generating a simple face with same specs
    (unless (and (not face-name) (equal spec (get face 'simple-face-spec)))
      (unless (memq face (face-list)) (make-face face))
      (save-match-data
        (dolist (attr attrs) (simple-set-face-attribute face attr)))
      (unless face-name (put face 'simple-face-spec spec)))
    face))

(defun simple-set-face-attribute (face attr)
  (let* ((a (intern attr))
         (as (cond ((eq a 'normal)
                    (list :width a :weight a :slant a))
                   ((memq a '(ultra-condensed extra-condensed condensed
					      semi-condensed semi-expanded expanded
					      extra-expanded ultra-expanded))
                    (list :width a))
                   ((memq a '(ultra-bold extra-bold bold semi-bold
					 semi-light light extra-light ultra-light))
                    (list :weight a))
                   ((memq a '(italic oblique reverse-italic reverse-oblique))
                    (list :slant a))
                   ((eq a 'underline)   '(:underline t))
                   ((eq a 'ununderline) '(:underline nil))
                   ((eq a 'overline)    '(:overline t))
                   ((eq a 'unoverline)  '(:overline nil))
                   ((eq a 'inverse)     '(:inverse t))
                   ((eq a 'uninverse)   '(:inverse nil))
                   (t (simple-face-parse-compound-attr attr)))))
    (message "%s -- %s" face as)
    (no-errors (apply 'set-face-attribute face nil as))))

(defun simple-face-parse-compound-attr (attr)
  (cond
   ((string-match "\\`\\(big\\|small\\|scale\\)\\([0-9]+\\)?\\'" attr)
    (let* ((op    (match-string 1 attr))
	   (arg   (match-string 2 attr))
	   (arg   (and arg (string-to-number arg)))
	   (scale (/ (if (equal op "scale")
			 (or arg 100)
		       (+ 100 (if arg (* 10 arg) 25)))
		     100.0))
	   (scale (if (equal op "small") (/ 1.0 scale) scale)))
      (list :height scale)))
   ((string-match "\\`box\\([0-9]+\\)?\\'" attr)
    (list :box (if (match-beginning 1)
		   (list :line-width (string-to-number (match-string 1 attr)))
		 t)))
   ;; split `.../...' color spec to `.../' and `/...'
   ((string-match "\\`\\([^/]+\\)/\\([^/]+\\)\\'" attr)
    (let ((fg (concat (match-string 1 attr) "/"))
	  (bg (concat "/" (match-string 2 attr))))
      (append (simple-face-parse-compound-attr fg)
	      (simple-face-parse-compound-attr bg))))
   ;; default colors in several forms, do nothing
   ((member attr '("" "/" "*" "*/" "/*" "default" "default/" "/default"))
    nil)
   ;; fg/bg colors
   ((and (string-match
	  "\\`\\(/\\)?\\(#\\|[hH]\\|hex\\|HEX\\)?\\([^/]*\\)\\(/\\)?\\'" attr)
	 ;; only `.../' or `/...'
	 (if (match-beginning 1) (not (match-beginning 4)) (match-beginning 4)))
    (let* ((rgb   (match-beginning 2))
	   (color (match-string 3 attr))
	   (color (if rgb (concat "#" color) color)))
      (list (if (match-beginning 1) :background :foreground) color)))
   ;; some color with no `/', used as fg
   ((string-match "\\`\\(?:#\\|[hH]\\|hex\\|HEX\\)\\([^/]*\\)\\'" attr)
    (list :foreground (concat "#" (match-string 1 attr))))
   ((assoc (downcase attr) color-name-rgb-alist) ; (defined-colors) better?
    (list :foreground attr))
   (t (error "Bad component in face spec: %S" attr))))

(defun simple-make-face-if-undefined (face)
  (let ((face (if (stringp face) (intern face) face)))
    (if (facep face) face (simple-make-face face))))

(defvar read-face-history nil
  "History variable for reading faces and colors.")

;; Redefined from "faces.el"
(defun read-face-name (prompt &optional string-describing-default multiple)
  "Read a face, defaulting to the face or faces on the char after point.
If it has the property `read-face-name', that overrides the `face' property.
PROMPT should be a string that describes what the caller will do with the face;
it should not end in a space.
STRING-DESCRIBING-DEFAULT should describe what default the caller will use if
the user just types RET; you can omit it.
If MULTIPLE is non-nil, return a list of faces (possibly only one).
Otherwise, return a single face.

** Modified to accept anything, and use `simple-make-face-if-undefined'."
  (let ((faceprop (or (get-char-property (point) 'read-face-name)
		      (get-char-property (point) 'face)))
        (aliasfaces nil)
        (nonaliasfaces nil)
	faces)
    ;; Try to get a face name from the buffer.
    (if (memq (intern-soft (thing-at-point 'symbol)) (face-list))
	(setq faces (list (intern-soft (thing-at-point 'symbol)))))
    ;; Add the named faces that the `face' property uses.
    (if (and (listp faceprop)
	     ;; Don't treat an attribute spec as a list of faces.
	     (not (keywordp (car faceprop)))
	     (not (memq (car faceprop) '(foreground-color background-color))))
	(dolist (f faceprop)
	  (if (symbolp f)
	      (push f faces)))
      (if (symbolp faceprop)
	  (push faceprop faces)))
    (delete-dups faces)

    ;; Build up the completion tables.
    (mapatoms (lambda (s)
                (if (custom-facep s)
                    (if (get s 'face-alias)
                        (push (symbol-name s) aliasfaces)
                      (push (symbol-name s) nonaliasfaces)))))

    ;; If we only want one, and the default is more than one,
    ;; discard the unwanted ones now.
    (unless multiple
      (if faces
	  (setq faces (list (car faces)))))
    (require 'crm)
    (let* ((input
	    ;; Read the input.
	    (completing-read-multiple
	     (if (or faces string-describing-default)
		 (format "%s (default %s): " prompt
			 (if faces (mapconcat 'symbol-name faces ",")
			   string-describing-default))
	       (format "%s: " prompt))
	     (completion-table-in-turn nonaliasfaces aliasfaces)
	     nil nil nil 'read-face-history
	     (if faces (mapconcat 'symbol-name faces ","))))
	   ;; Canonicalize the output.
	   (output
	    (cond ((or (equal input "") (equal input '("")))
		   faces)
		  ((stringp input) (mapcar 'simple-make-face-if-undefined
                                           (split-string input ", *" t)))
		  ((listp input) (mapcar 'simple-make-face-if-undefined input))
		  (input))))
      ;; Return either a list of faces or just one face.
      (if multiple
	  output
	(car output)))))


;; ** Color functions **
(defun set-region-face (beg end face &optional force-overlays dont-make-face)
  "Set the face of a region.
Set the region from BEG to END to have face FACE.

Will use face properties, unless font-lock is active -- in that case it will
use overlays.  If the optional argument FORCE-OVERLAYS is t (prefix argument
when called interactively), will use overlays anyway.  If its value is 'both,
will use both overlays and properties (overlays don't stick in copied text)."
  (interactive (if mark-active
		   (list (region-beginning) (region-end) (read-face-name "Face")
			 (and current-prefix-arg t) nil)
                 (error "Use the mark, luke.")))
  (let* ((face (if dont-make-face face (simple-make-face face)))
         ;; These are taken from `save-buffer-state' - make sure that
         ;; we modify the buffer with no state changes.
         (modified                   (buffer-modified-p))
         (buffer-undo-list           t)
         (inhibit-read-only          t)
         (inhibit-point-motion-hooks t)
         (inhibit-modification-hooks t)
         (use-overlays   (or force-overlays
                             (and font-lock-mode font-lock-defaults)))
         (use-properties (or (not use-overlays) (eq 'both force-overlays)))
         deactivate-mark buffer-file-name buffer-file-truename)
    (when use-overlays
      (let ((overlay (make-overlay beg end)))
        (overlay-put overlay 'set-region-face t)
        (overlay-put overlay 'face face)))
    (when use-properties
      (put-text-property beg end 'face face))
    (unless modified (restore-buffer-modified-p nil))))
(put 'set-region-face 'safe-local-eval-function t)

(defun remove-set-face-overlays ()
  "Removes overlays set by `set-region-face'."
  (interactive)
  (remove-overlays nil nil 'set-region-face t))
(put 'remove-set-face-overlays 'safe-local-eval-function t)

(defun set-regexp-face (regexp face &optional force-overlays)
  "Set the face of all occurrences of REGEXP to FACE.
FACE can be a a list of pairs of a subexp number and its color: (SUB . COLOR).
Uses `set-region-face' to set the faces."
  ;; interactive like `add-color-pattern'
  (interactive (list (read-from-minibuffer
                      "Pattern to highlight: "
                      (and transient-mark-mode mark-active
                           (regexp-quote (buffer-substring-no-properties
                                          (region-beginning) (region-end))))
                      nil nil 'regexp-history)
                     (read-face-name "Face")
                     (and current-prefix-arg t)))
  (let ((face (if (listp face)
		  (progn (dolist (f face) (setcdr f (simple-make-face (cdr f))))
			 face)
                (simple-make-face face))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t nil)
        (if (listp face)
	    (dolist (f face)
	      (set-region-face (match-beginning (car f)) (match-end (car f))
			       (cdr f) force-overlays t))
          (set-region-face (match-beginning 0) (match-end 0) face
                           force-overlays t))))))
(put 'set-regexp-face 'safe-local-eval-function t)

(defvar added-color-patterns nil
  "List of colors added using `add-color-pattern'.")

(defun add-color-pattern (regexp face &optional expnum override &rest more)
  "Add a pattern for this buffer's `font-lock-keywords'.
REGEXP is the pattern to be colored with FACE.  When called programatically - a
third integer argument EXPNUM specifies the regexp to hilight, and a fourth
argument OVERRIDE is the override flag.  Possible MORE arguments will specify
more faces, numbers and flags.

When called interactively, a positive prefix argument will make it an
overriding addition, negative prefix makes it a normal addition, and no prefix
make the style appended over any existing style.  When OVERRIDE is set to
anything other than `t' or `nil' in any way, the new pattern will be added to
the end of `font-lock-keywords'.
Examples:
 (add-color-pattern \"regexp\" 'face)
 (add-color-pattern \"\\\\(r\\\\)egexp\" 'face 1 t)
 (add-color-pattern \"\\\\(r\\\\)egex\\\\(p\\\\)\" 'face1 1 t 'face2 2 nil)"
  (interactive (list (read-from-minibuffer
                      "Pattern to highlight: "
                      (and transient-mark-mode mark-active
                           (regexp-quote (buffer-substring-no-properties
                                          (region-beginning) (region-end))))
                      nil nil 'regexp-history)
                     (read-face-name "Face")))
  (when (interactive-p)
    (setq override (cond ((not current-prefix-arg) 'prepend)
                         ((>= (prefix-numeric-value current-prefix-arg) 0) t)
                         (t nil))
          expnum 0))
  (let* ((face (simple-make-face face)) ; in case of calling from an expression
         (face (list 'quote face))
         (add (list (or expnum 0) face override t)))
    (while (and more (cdr more))
      (let* ((face (simple-make-face (pop more)))
             (new (list (pop more) (list 'quote face) (pop more) t)))
        (nconc add (list new))))
    (push regexp add)
    (make-local-variable 'added-color-patterns)
    (unless (member add added-color-patterns)
      (push add added-color-patterns)
      (font-lock-add-keywords
       nil (list add) (if (memq override '(t nil)) nil t))
      (unless font-lock-defaults
        ;; we have a misbehaved mode, set defaults and turn on again
        ;; another way to fix this is (setq-default font-lock-defaults '(nil))
        (font-lock-mode -1)
        (setq font-lock-defaults '(nil))
        (font-lock-mode 1))
      (font-lock-fontify-buffer))))
(put 'add-color-pattern 'safe-local-eval-function t)

(defun remove-added-color-pattern (&optional n)
  "Remove the a pattern added with `add-color-pattern'.
Automatically removes a single-added pattern, otherwise asks which one to
remove.  With a numeric prefix argumet, remove that number of patterns (last
one added first), if negative removes all."
  (interactive "P")
  (let ((n (and n (prefix-numeric-value n))))
    (cond
     ((null added-color-patterns) (message "No patterns to remove."))
     ((or (and n (< n 0)) (= 1 (length added-color-patterns)))
      (font-lock-remove-keywords nil added-color-patterns)
      (setq added-color-patterns nil))
     (n (while (and added-color-patterns (> n 0))
	  (font-lock-remove-keywords nil (list (pop added-color-patterns)))
	  (setq n (1- n))))
     (t (let* ((history-list (mapcar 'car added-color-patterns))
	       (rem (assoc (completing-read "Pattern to unhighlight: "
					    added-color-patterns nil t
					    (caar added-color-patterns)
					    (cons 'history-list 1))
			   added-color-patterns)))
	  (font-lock-remove-keywords nil (list rem))
	  (setq added-color-patterns (delq rem added-color-patterns))))))
  (font-lock-fontify-buffer))
(put 'remove-added-color-pattern 'safe-local-eval-function t)

;; ;; Switchable paren coloring modes
;; (defcustom paren-match-face nil "" :group 'mic-paren-matching)
;; (defcustom paren-sexp-mode nil "" :group 'mic-paren-matching)
;; (defvar paren-hilight-mode 1)
;; (defvar paren-hilight-modes
;;   [(show-paren-match . nil) (bold . t) (show-paren-match . t)])
;; (defun prev-paren-hilight-mode (arg)
;;   "Switch to the previous paren-hilight mode or to the ARGth one."
;;   (interactive "P")
;;   (set-paren-hilight-mode
;;    (or arg (mod (1- paren-hilight-mode) (length paren-hilight-modes)))))
;; (defun next-paren-hilight-mode (arg)
;;   "Switch to the next paren-hilight mode or to the ARGth one."
;;   (interactive "P")
;;   (set-paren-hilight-mode
;;    (or arg (mod (1+ paren-hilight-mode) (length paren-hilight-modes)))))
;; (defun set-paren-hilight-mode (n)
;;   (setq paren-hilight-mode (min n (1- (length paren-hilight-modes))))
;;   (let ((mode (elt paren-hilight-modes paren-hilight-mode)))
;;     (if mode
;;         (progn (setq paren-match-face (car mode) paren-sexp-mode (cdr mode))
;;                (paren-activate))
;;       (paren-deactivate))
;;     (message "Paren mode h%s" (1+ paren-hilight-mode))))


;; ** Cursor **
(defvar my:set-cursor-color-color "" "")
(defvar my:set-cursor-color-buffer "" "")

(defun my:set-cursor-color-according-to-mode ()
  "Change cursor color according to some minor modes."
  (let ((color
         (if buffer-read-only "purple1"
           (if overwrite-mode "red"
             "#55A096"))))  ;; insert mode
    (unless (and (string= color my:set-cursor-color-color)
                 (string= (buffer-name) my:set-cursor-color-buffer))
      (set-cursor-color (setq my:set-cursor-color-color color))
      (setq my:set-cursor-color-buffer (buffer-name)))))
(add-hook 'post-command-hook 'my:set-cursor-color-according-to-mode)


;; Local Variables:
;; eval:(set-regexp-face "^\s*;;\\([{}]*\\)?\\( =+ .* =+\\)?$" 'VioletRed4-bold-italic)
;; eval:(set-regexp-face "^\s*;;\\([{}]*\\)?\\( \\*+ .* \\*+\\)?$" 'Green4-bold-italic)
;; eval:(set-regexp-face "^\s*;;\\([{}]*\\)?\\( \\[+ .* \\]+\\)?$" 'h00688b-bold-italic)
;; End:
