;; Time-stamp: <2010-07-02 11:20:57 cmauclai>


;; ** Quick reference of keybindings **
(defun my:emacs-quickref ()
  "Display the AcmeEmacs Quick Reference."
  (interactive)
  (let ((qr (concat emacs-root "QuickRef.txt")))
    (if (file-readable-p qr)
        (view-file qr)
      (error "Couldn't find the file (%S)" qr))))



;; ** Blocks **
(defun my:big-block (prefix name)
  "Inserts a \"big\"-block-style separation line in the buffer."
  (interactive "*P\nMName: ")
  (let ((prefix (if (not prefix) 4 (prefix-numeric-value prefix))))
    (insert (if (= 1 (length comment-start)) (concat comment-start comment-start " ") comment-start)
            (make-string prefix ?=))
    (insert " " name " ")
    (insert (make-string (- fill-column (current-column)) ?=))
    (insert comment-end "\n")))

(defun my:small-block (prefix name)
  "Inserts a \"small\"-block-style separation line in the buffer."
  (interactive "*P\nMName: ")
  (let ((prefix (if (not prefix) 2 (prefix-numeric-value prefix))))
    (insert (if (= 1 (length comment-start)) (concat comment-start comment-start " ") comment-start)
            (make-string prefix 42)); `?*' bug en mode c-mode (et dérivés)
    (insert " " name " ")
    (insert (make-string prefix 42))
    (insert comment-end "\n")))


;; ** Advices **
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))


;; ** Case **
(defun my:toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles from 3 cases: UPPER CASE, lower case, Title Case,
in that cyclic order."
  (interactive)
  (let (pos1 pos2 (deactivate-mark nil) (case-fold-search nil))
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning)
              pos2 (region-end))
      (setq pos1 (car (bounds-of-thing-at-point 'word))
            pos2 (cdr (bounds-of-thing-at-point 'word))))

    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char pos1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps") )
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps") )
         (t (put this-command 'state "all lower") )
         )
        )
      )

    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region pos1 pos2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region pos1 pos2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region pos1 pos2) (put this-command 'state "all lower")))))


;; ** Moving **
;; remember last argument used, and the column we started with
(defvar my:last-scroll-arg nil)
(defvar my:scroll-column nil)
;; remember the last command and its group, so we can identify repeated uses
;; of groups even when this is invoked from other commands
(defvar last-scroll-command-and-group nil)
;; remembered positions: (list upward-positions downward-positions)
(defvar my:scroll-posns nil)
(make-variable-buffer-local 'my:scroll-posns)

(defvar my:track-eol nil)
(defvar my:line-movement-without-dings t)

(defun my:previous-line (&optional arg try-vscroll)
  "Like `previous-line', but ding only if cannot move + do my eol tracking."
  (interactive "^p\np")
  (or arg (setq arg 1))
  (if (bobp)
      (unless my:line-movement-without-dings (ding))
    (let ((p (point)))
      (line-move (- arg) t nil try-vscroll)
      (when (= p (point))
        (goto-char (point-min))
        (when (and (not my:line-movement-without-dings) (= p (point)))
          (ding)))))
  (setq this-command 'previous-line)
  nil)
(put 'my:previous-line 'CUA 'move)

(defun my:next-line (&optional arg try-vscroll)
  "Like `next-line', but ding only if cannot move + do my eol tracking."
  (interactive "^p\np")
  (or arg (setq arg 1))
  (if (eobp)
      (unless my:line-movement-without-dings (ding))
    (let ((p (point)))
      (line-move arg t nil try-vscroll)
      (when (= p (point))
        (goto-char (point-max))
        (when (and (not my:line-movement-without-dings) (= p (point)))
          (ding)))))
  (setq this-command 'next-line)
  nil)
(put 'my:next-line 'CUA 'move)

(defun my:get-scroll-position ()
  (list (point) (window-start) (window-hscroll)))
(defun my:set-scroll-position (posn)
  (goto-char (nth 0 posn))
  (set-window-start nil (nth 1 posn) t)
  (set-window-hscroll nil (nth 2 posn)))

(defun my:set-visual-column ()
  ;; same as the code at the top of `line-move-visual'
  (let ((posn (posn-at-point)))
    (setq my:scroll-column
          (if (eq (nth 1 posn) 'right-fringe) ; overflow-newline-into-fringe
              (- (window-width) 1)
            (let ((x (car (posn-x-y posn))))
              (and x (truncate (/ (float x) (frame-char-width)))))))))
(defun my:goto-visual-column ()
  (when my:scroll-column (vertical-motion (cons my:scroll-column 0))))

(defun my:do-scroll (arg isdown group)
  (let* ((direction (if isdown -1 +1))
         (repeated
          ;; see the above comment: this makes it possible for things to work
          ;; fine even when called through some other command
          (prog1 (and (eq (car last-scroll-command-and-group) last-command)
                      (eq (cdr last-scroll-command-and-group) group)
                      (memq current-prefix-arg '(nil -)))
            (setq last-scroll-command-and-group (cons this-command group))))
         (arg (if repeated
                  my:last-scroll-arg
                (progn (my:set-visual-column)
                       (setq my:last-scroll-arg arg))))
         (direction (if (or (eq arg '-) (< (prefix-numeric-value arg) 0))
                        (- direction) direction))
         (direction (if (> direction 0) 'down 'up))
         ;; these hold the referencing cons cell (so it can be modified)
         past-box future-box
         (curpos (my:get-scroll-position)))
    (unless (and repeated my:scroll-posns)
      (setq my:scroll-posns (list '() '())))
    ;; pull the right boxes
    (if (eq direction 'up)
        (setq future-box my:scroll-posns past-box   (cdr my:scroll-posns))
      (setq past-box   my:scroll-posns future-box (cdr my:scroll-posns)))
    ;; remember where we are now, unless it's in the same place as last time
    (unless (and (consp (car past-box)) (equal curpos (caar past-box)))
      (setcar past-box (cons curpos (car past-box))))
    ;; dump a future position that is the same as where we are (might happen
    ;; when we get to the buffer edges) see if there's a future position we
    ;; should go to (at most one, but still use `while' for safety)
    (while (and (consp (car future-box)) (equal curpos (caar future-box)))
      (setcar future-box (cdar future-box)))
    (cond
     ;; see if there's a future position we should go to
     ((consp (car future-box))
      (let ((posn (caar future-box)))
        (setcar future-box (cdar future-box))
        (my:set-scroll-position posn)))
     ;; we're at the edge so there is nothing to do
     ((if (eq direction 'up) (bobp) (eobp))
      nil)
     ;; otherwise try do the needed scroll if the edge is not visible...
     ((or (pos-visible-in-window-p
           (if (eq direction 'up) (point-min) (point-max)))
          (condition-case nil
              (progn
                (funcall (if isdown 'scroll-down 'scroll-up) arg)
                (my:goto-visual-column)
                ;; if we went down and now we see the bottom (and it we know
                ;; it wasn't visible before), then make it be the bottom
                (when (and (eq direction 'down)
                           (pos-visible-in-window-p (point-max)))
                  (save-excursion (goto-char (point-max)) (recenter -1)))
                nil)
            ((beginning-of-buffer end-of-buffer) t)))
      ;; ...but if the edge is visible (or scrolling failed), move instead
      (if (integerp arg)
          (let ((my:line-movement-without-dings t)
                ;; set a goal column, and make sure we do a visual movement
                (temporary-goal-column (float my:scroll-column))
                (line-move-visual t)
                ;; and fake a second call to use it
                (this-command 'previous-line))
            (if (eq direction 'up)
                (my:previous-line (abs arg))
              (my:next-line (abs arg)))
            (my:goto-visual-column))
        (goto-char (if (eq direction 'up) (point-min) (point-max))))))))

(defmacro defun-my:up/down (name-pat inter keep docstr)
  (let ((mk (lambda (u/d downp)
              (let* ((name   (intern (replace-regexp-in-string
                                      "XX" u/d (symbol-name name-pat) t)))
                     (docstr (replace-regexp-in-string "XX" u/d docstr t))
                     (doit `(my:do-scroll arg ,downp ',name-pat))
                     (doit (if keep
                               `(let ((p (point)))
                                  ,doit
                                  ;; go back only if possible
                                  (when (and (< p (window-end nil t))
                                             (<= (window-start) p))
                                    (goto-char p)))
                             doit)))
                `((defun ,name (arg) ,docstr (interactive ,inter) ,doit)
                  (put ',name 'CUA 'move)
                  (put ',name 'isearch-scroll t))))))
    `(progn ,@(funcall mk "up" nil) ,@(funcall mk "down" t))))

(defun-my:up/down my:scroll-XX "P" nil
  "Wrapper for `scroll-XX' that does a scroll-in-place.
Also:
- when reaching the edge, move the cursor instead of beeping,
- consecutive uses with no prefix use the first prefix in the sequence.")

(defun-my:up/down my:scroll-XX-1 "p" nil
  "Like `my:scroll-XX' with a default of one line.")

(defun-my:up/down my:scroll-XX-1-stay "p" t
  "Like `my:scroll-XX-1' but stay in the same place.")


;; ** Evals **
(defun my:eval-last-sexp-or-region ()
  "Like `eval-last-sexp' or `eval-region', depending on an active mark."
  (interactive)
  (setq this-command (if mark-active 'eval-region 'eval-last-sexp))
  (call-interactively this-command))


;; ** Comments **
(defvar my:comment-line-last-col nil)

(defun my:toggle-comment-line (n again)
  "Toggle a comment on current N line(s) (disable line by line)."
  (if comment-start
      (let* ((end    (cond ((or (not comment-end) (equal comment-end "")) "")
                           ((string-match "^ " comment-end) comment-end)
                           (t (concat " " comment-end))))
             (start  (cond ((string-match " $" comment-start) comment-start)
                           ((and (= (length comment-start) 1) (equal end ""))
                            (concat comment-start " "))
                           (t (concat comment-start " "))))
             (qstart (regexp-quote start))
             (qend   (regexp-quote end))
             (col    (and again my:comment-line-last-col))
             (mode   (and again (if col 'comment 'uncomment)))
             (direction (if (< 0 n) 1 -1))
             (n  (abs n)))
        (catch 'done
          (beginning-of-line)
          (if (< direction 0)
              (forward-line -1))
          (while (>= (setq n (1- n)) 0)
            (when (eobp) (throw 'done nil))
            (skip-chars-forward "\t ")
            (unless (eolp)
              (unless mode
                (setq mode (if (looking-at (concat qstart "\\(.*\\)" qend "$"))
                               'uncomment 'comment)))
              (let ((cur (current-column)))
                (cond ((and col (< col cur))
                       (move-to-column col t))
                      ((eq mode 'comment)
                       (setq col cur))))
              (cond ((eq mode 'comment)
                     (insert start) (end-of-line) (insert end))
                    ((eq mode 'uncomment)
                     (when (looking-at (concat qstart "\\(.*\\)" qend "$"))
                       (replace-match "\\1" t)))))
            (forward-line direction))
          (if (< direction 0)
              (forward-line 1)))
        (setq my:comment-line-last-col col))
    (message "Comments not available for this mode")))

(defun my:toggle-comment-line-and-go-down (n)
  "Toggle a comment on current N line(s) (disable line by line)."
  (interactive "p")
  (my:toggle-comment-line n
                            (eq last-command 'my:toggle-comment-line-and-go-down)))

(defun my:go-up-and-toggle-comment-line (n)
  "Toggle a comment on current N line(s) (disable line by line)."
  (interactive "p")
  (my:toggle-comment-line (- n)
                            (eq last-command 'my:go-up-and-toggle-comment-line)))


;; ** Selecting **
(defun my:mark-line (&optional arg allow-extend)
  "Put point at beginning of this line, mark end.
The line marked is the one that contains point or follows point.

With argument ARG, puts mark at end of a following line, so that
the number of lines marked equals ARG.

If ARG is negative, point is put at end of this line, mark is put
at beginning of this or a previous line

Interactively, if this command is repeated
or (in Transient Mark mode) if the mark is active,
it marks the next ARG lines after the ones already marked."
  (interactive "P\np")
  (cond ((and allow-extend
	      (or (and (eq last-command this-command) (mark t))
		  (region-active-p)))
	 (setq arg (if arg (prefix-numeric-value arg)
		     (if (< (mark) (point)) -1 1)))
	 (set-mark
	  (save-excursion
	    (goto-char (mark))
	    (forward-line arg)
	    (point))))
	(t
     (goto-char (point-at-bol))
	 (push-mark
	  (save-excursion
	    (forward-line (prefix-numeric-value arg))
	    (point))
	  nil t))))

(defun my:select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters are paired characters: ()[]{}<>«»“”‘’「」, including \"\"."
  (interactive)
  (let (b1 b2)
    (skip-chars-backward "^<(“{[「«\"‘`")
    (setq b1 (point))
    (skip-chars-forward "^>)”}]」»\"’'")
    (setq b2 (point))
    (set-mark b1)))

;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun semnav-up (arg)
  (interactive "p")
  (when (nth 3 (syntax-ppss))
    (if (> arg 0)
        (progn
          (skip-syntax-forward "^\"")
          (goto-char (1+ (point)))
          (setq arg (- arg 1)))
      (skip-syntax-backward "^\"")
      (goto-char (1- (point)))
      (setq arg (+ arg 1))))
  (up-list arg))

;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun my:extend-selection (arg &optional incremental)
  "Select the current word.
Subsequent calls expands the selection to larger semantic unit."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (or (and transient-mark-mode mark-active)
                         (eq last-command this-command))))
  (if incremental
      (progn
        (semnav-up (- arg))
        (forward-sexp)
        (mark-sexp -1))
    (if (> arg 1)
        (my:extend-selection (1- arg) t)
      (if (looking-at "\\=\\(\\s_\\|\\sw\\)*\\_>")
          (goto-char (match-end 0))
        (unless (memq (char-before) '(?\) ?\"))
          (forward-sexp)))
      (mark-sexp -1))))

(defun my:duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

;; ** Sanitizing files **
(defun my:sanitize ()
  "Delete all the trailing white spaces, and convert all tabs to
  multiple spaces across the current buffer."
  (interactive "*")
  (delete-trailing-whitespace)
  (untabify (point-min) (point-max))
  (font-lock-fontify-buffer))

;; Local Variables:
;; eval:(set-regexp-face "^\s*;;\\([{}]*\\)?\\( =+ .* =+\\)?$" 'VioletRed4-bold-italic)
;; eval:(set-regexp-face "^\s*;;\\([{}]*\\)?\\( \\*+ .* \\*+\\)?$" 'Green4-bold-italic)
;; eval:(set-regexp-face "^\s*;;\\([{}]*\\)?\\( \\[+ .* \\]+\\)?$" 'h00688b-bold-italic)
;; End:
