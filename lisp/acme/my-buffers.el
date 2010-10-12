;; Time-stamp: <2010-04-08 13:46:41 cmauclai>


;; ** Globals **
(my:define-keys 'global '(list-buffers electric-buffer-list))

(eval-after-load "ebuff-menu"
  '(progn
     (my:define-keys
       ;; Make the electric-buffer-menu a bit more user friendly (its a symbol so
       ;; nothing will happen if it is unbound when not using the electric version).
       'electric-buffer-menu-mode-map
       ;; Minimal protection against getting out by mistake
       '(previous-buffer             Electric-buffer-menu-quit)
       '(my:next-buffer-acyclic      Electric-buffer-menu-quit)
       '(next-buffer                 Electric-buffer-menu-quit)
       '(other-window                Electric-buffer-menu-quit)
       ;; '(burry-buffers                Electric-buffer-menu-quit)
       '(list-buffers                Electric-buffer-menu-quit)
       '(electric-buffer-list        Electric-buffer-menu-quit)
       ;; and make escape exit (if in a window system)
       (and window-system '([?\e]    Electric-buffer-menu-quit))
       (and window-system '([escape] Electric-buffer-menu-quit))
       (unless window-system '([?\e ?O] nil)) ; allow arrows in text mode
       '([?1]                        Electric-buffer-menu-select)
       '([?x]                        Buffer-menu-execute))

     ;; Add a hook to fix 'ebuff-menu.el' -- exit Ebuff when click outside (like in
     ;; ehelp) - should be done by gnu.
     (add-hook
      'electric-buffer-menu-mode-hook
      (lambda ()
        (make-local-hook 'mouse-leave-buffer-hook)
        (add-hook 'mouse-leave-buffer-hook 'Electric-buffer-menu-quit nil t)
        (make-local-hook 'kill-buffer-hook)
        (add-hook 'kill-buffer-hook
                  '(lambda ()
                     (condition-case nil (Electric-buffer-menu-quit)
                       (error nil)))
                  nil t)))))

(defun my:write-or-move-file (new)
  "Like `write-file', but with a prefix argument delete the original file."
  (interactive
   (let ((prompt (if (and current-prefix-arg buffer-file-name)
                     "Move to file: " "Write file: ")))
     (list
      (if buffer-file-name
          (read-file-name prompt nil nil nil nil)
        (read-file-name prompt default-directory
                        (expand-file-name (file-name-nondirectory (buffer-name))
                                          default-directory)
                        nil nil)))))
  (let ((old (buffer-file-name))
        (movep (and current-prefix-arg buffer-file-name)))
    (write-file new t)
    (when movep
      (setq new (buffer-file-name))
      (when (and old new (not (equal old new)))
        (delete-file old)
        (message "Moved to %s" new)))))

;; From Johan Andersson and Steve Yegge
(defun my:rename-file-and-buffer ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (message "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file name new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)))))))


;; ** Mini-buffer **
(minibuffer-electric-default-mode 1)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq minibuffer-prompt-properties
                  (nconc minibuffer-prompt-properties
                         '(point-entered minibuffer-avoid-prompt)))))

;; Use partial-completion
(require 'complete)

;; Redefine navigation keys so they place the cursor at the end and no errors
(defun my:previous-history-element (n)
  (interactive "p")
  (no-errors-beep (previous-history-element n))
  (goto-char (point-max)))

(defun my:next-history-element (n)
  (interactive "p")
  (no-errors-beep (next-history-element n))
  (goto-char (point-max)))

(defun my:next-complete-history-element (n)
  (interactive "p")
  (no-errors-beep (next-complete-history-element n))
  (goto-char (point-max)))

(defun my:previous-complete-history-element (n)
  (interactive "p")
  (no-errors-beep (previous-complete-history-element n))
  (goto-char (point-max)))

(defvar my:last-minibuf-string ".")

(defun my:next-history-contains (n)
  (interactive "p")
  (let ((search-str
         (cond
          ((and mark-active (not (= (point) (mark))))
           ;; mark is active and not empty - use selection
           (setq mark-active nil)
           (regexp-quote (buffer-substring (point) (mark))))
          ((not (and (eolp)
                     (memq last-command '(my:previous-history-contains
                                          my:next-history-contains))))
           ;; not end of line - use rest of line, but if at eoln and last
           ;; command was different, use the basic file name
           (when (eolp)
             (re-search-backward "\\(^\\|/\\)[^/]*\\="
                                 (save-excursion (beginning-of-line)
                                                 (point)) ; for fsf21
                                 nil)
             (when (looking-at "/") (goto-char (1+ (point)))))
           (concat (regexp-quote
                    (buffer-substring (point)
                                      (save-excursion (end-of-line) (point))))
                   "[^/]*$"))
          (t my:last-minibuf-string))))
    (setq my:last-minibuf-string search-str)
    (no-errors-beep (next-matching-history-element search-str n))
    (goto-char (point-max))))

(defun my:previous-history-contains (n)
  (interactive "p")
  (my:next-history-contains (- n)))

(let ((keys '(([(up)]           my:previous-history-element)
              ([(down)]         my:next-history-element)
              ;; C-up/down searches for an initial string
              ([(control up)]   my:previous-complete-history-element)
              ([(prior)]        my:previous-complete-history-element)
              ([(control down)] my:next-complete-history-element)
              ([(next)]         my:next-complete-history-element)
              ;; M-up/down searches for a contained string
              ([(meta up)]      my:previous-history-contains)
              ([(meta down)]    my:next-history-contains)
              ;; misc
              ([(control tab)]  other-window) ; don't want file caching
              ))
      (keymaps (list minibuffer-local-map
                     minibuffer-local-completion-map
                     minibuffer-local-must-match-map
                     minibuffer-local-ns-map
                     minibuffer-local-filename-completion-map
                     minibuffer-local-filename-must-match-map
                     minibuffer-local-isearch-map
                     read-expression-map)))
  (dolist (kmap keymaps)
    (when kmap
      (dolist (key+cmd keys)
        (apply 'define-key kmap key+cmd)))))

;; Make some minibuffer keys electric - original idea stolen from XEmacs.
(defun my:minibuffer-electric-slash ()
  (interactive)
  (save-restriction
    (narrow-to-region (field-beginning) (field-end))
    (if (and (= (point) (point-max)) (> (point) (point-min)))
        (let ((str (buffer-string)))
          (delete-region (point-min) (point-max))
          (insert
           (cond
            ;; permit `//hostname/path/to/file'
            ((equal "/" str) str)
            ;; permit `http://' but not `d://'
            ((string-match "\\([a-zA-Z][a-zA-Z]+:/\\)\\'" str)
             (match-string 0 str))
            ;; otherwise a second `/' starts a new absolute path
            ((string-match "/\\'" str) "")
            ;; make `../' go up, extend to any number of dots
            (t (catch 'done
                 (while (string-match
                         "\\`\\(\\|.*/\\)\\([^/]*\\)/\\(\\.*\\)\\.\\'"
                         ;;  <---prfx---><--path---> <--dots-->
                         str)
                   (let ((prfx (match-string 1 str))
                         (path (match-string 2 str))
                         (dots (match-string 3 str)))
                     (setq str
                           (cond
                            ((equal dots "") (concat prfx path))
                            ((and (eq system-type 'windows-nt)
                                  (string-match "\\`.:\\'" path))
                             ;; don't know what this is (windows drive)
                             (throw 'done nil))
                            ((string-match "\\`\\.+\\'" path)
                             ;; more dots - combine them
                             (concat prfx path dots))
                            ((string-match "^~" path)
                             ;; expand a user home dir
                             (let ((str (expand-file-name path)))
                               (if (string-match (regexp-quote path) str)
                                   (concat prfx dots) ; not expanded, go up
                                 (concat str "/." dots)))) ; cont with expansn
                            ((string-match "\\`\\$" path)
                             ;; expand an environment variable
                             (let ((str (no-errors
                                         (substitute-in-file-name path))))
                               (if str
                                   (concat str "/." dots) ; cont with expansion
                                 (concat prfx dots)))) ; not expanded, go up
                            ((equal prfx "")
                             ;; can't go higher
                             prfx)
                            ;; normal cases - go up
                            (t (concat prfx dots)))))))
               str))
           "/"))
      (insert "/"))))
(put 'my:minibuffer-electric-slash 'delete-selection t)

(defun my:minibuffer-electric-key (&optional from to)
  (interactive)
  (let* ((from (or from "\\`.*/\\'"))
         (to   (or to ""))
         (from (if (eq system-type 'windows-nt)
                   (replace-regexp-in-string "/" "[/\\]" from nil t)
                 from)))
    (save-restriction
      (narrow-to-region (field-beginning) (field-end))
      (when (= (point) (point-max))
        (save-excursion
          (save-match-data
            (goto-char (point-min))
            (while (re-search-forward from nil t) (replace-match to nil nil)))))
      (when last-command-event (insert last-command-event)))))
(put 'my:minibuffer-electric-key 'delete-selection t)

(defun my:minibuffer-electric-bslash () ; convert to slashes for windows
  (interactive)
  (let ((last-command-event nil)) (my:minibuffer-electric-key "\\\\" "/"))
  (my:minibuffer-electric-slash))
(put 'my:minibuffer-electric-bslash 'delete-selection t)

(defun my:minibuffer-electric-colon () ; ".../x:" -> "x:" (for windows)
  (interactive) (my:minibuffer-electric-key "\\`.*/\\([a-zA-Z]\\)\\'" "\\1"))
(put 'my:minibuffer-electric-colon 'delete-selection t)

(defvar my:electric-file-minibuffer-mode-map
  (let ((map (make-sparse-keymap)))
    (my:define-keys map
      '("/" my:minibuffer-electric-slash)
      (and (eq system-type 'windows-nt) '("\\" my:minibuffer-electric-bslash))
      '("~" my:minibuffer-electric-key) ; user names
      '("$" my:minibuffer-electric-key) ; variable at the beginning
      (and (eq system-type 'windows-nt) '(":" my:minibuffer-electric-colon)))
    map))


;; Make it convenient to navigate between file names using sexp commands.
(defvar my:electric-file-minibuffer-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\/ "." table)
    (when (eq system-type 'windows-nt)
      (modify-syntax-entry ?\\ "." table))
    (mapc (lambda (ch) (modify-syntax-entry ch "_" table))
          ",.? #^~':;@%!|")
    table))

(defun my:electric-file-minibuffer-setup ()
  "Setup electric minibuffer for `find-file' etc."
  (when (and (boundp 'minibuffer-completion-table)
             (eq minibuffer-completion-table 'read-file-name-internal))
    (when (and (boundp 'file-name-shadow-mode) file-name-shadow-mode)
      (file-name-shadow-mode -1)) ; no need for this
    (let* ((beg (minibuffer-prompt-end))
           (end (point-max))
           (cur (buffer-substring-no-properties beg end))
           (abbrev (abbreviate-file-name cur)))
      (unless (equal cur abbrev)
        (delete-region beg end)
        (insert abbrev)))
    (set-syntax-table my:electric-file-minibuffer-syntax-table)
    ;; this is the only way I could get it to work right, using
    ;; minor-mode-alist made the bindings be in effect when going out of the
    ;; minibuffer with other-window...
    (set-keymap-parent my:electric-file-minibuffer-mode-map
                       (current-local-map))
    (use-local-map my:electric-file-minibuffer-mode-map)))
(add-hook 'minibuffer-setup-hook 'my:electric-file-minibuffer-setup)


;; Get rid of the tramp autoloading message quickly
(eval-after-load "tramp"
  '(run-at-time 0.1 nil (lambda () (message nil))))


;; Friendlier shell command
;; This is for making these things silent and restore window configuration.
(defun my:complete-filenames ()
  "A silent version of `comint-dynamic-complete'."
  (interactive)
  (run-at-time 0.6 nil 'message nil)
  (save-window-excursion (comint-dynamic-complete)))

;; Mostly from XEmacs
(defvar read-shell-command-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\t"    'my:complete-filenames)
    (define-key map "\M-\t" 'my:complete-filenames)
    map)
  "Minibuffer keymap used by `my:shell-command' and related commands.")

(defun my:shell-command (command &optional output-buffer error-buffer)
  "Similar to `shell-command' but can do filename completions, and sets
environment variables $f, $F, and $d to the current file's name, fullname,and
directory name (if the current buffer is associated with a file)."
  (interactive (list (read-string
                      (if (and transient-mark-mode mark-active)
                          "Shell command on Region: "
                        "Shell command: "))
                     current-prefix-arg shell-command-default-error-buffer))
  (let ((max-mini-window-height 1) ; make it more predictable
        (process-environment process-environment))
    (when (buffer-file-name)
      (setenv "f" (file-name-nondirectory (buffer-file-name)))
      (setenv "F" (buffer-file-name))
      (setenv "d" (file-name-directory (buffer-file-name))))
    (setq command (substitute-env-vars command))
    (if (and transient-mark-mode mark-active)
        (shell-command-on-region
         (and transient-mark-mode mark-active (region-beginning))
         (and transient-mark-mode mark-active (region-end))
         (substitute-env-vars command) output-buffer
         (and current-prefix-arg t)
         error-buffer t)
      (shell-command command output-buffer error-buffer))))

;; ** Switch buffers **
(defun my:next-buffer-acyclic (n)
  "Like `next-buffer', but not cyclic.
A prefix argument determines how many buffers to skip (default is 1), if
negative, count from the end."
  (interactive "p")
  (unless (equal n 0)
    (let ((buffers (let ((bs (buffer-list)))
                     (if (and n (< n 0)) (nreverse bs) bs)))
          (n       (1- (abs (or n 1))))
          (curbuf  (current-buffer))
          (pred    (frame-parameter nil 'buffer-predicate))
          (found   nil))
      ;; This code is similar to the code in `get-next-valid-buffer',
      ;; specifically, the test for good buffers.
      (while (and (not found) buffers)
        (let* ((buf  (car buffers))
               (good (and (not (eq curbuf buf))
                          (buffer-live-p buf)
                          (or (null pred) (funcall pred buf))
                          (not (eq (aref (buffer-name buf) 0) ?\s))
                          (null (get-buffer-window buf 'visible)))))
          (when good (if (zerop n) (setq found buf) (setq n (1- n))))
          (setq buffers (cdr buffers))))
      (if found (switch-to-buffer found) (error "Not enough buffers")))))

(defun my:previous-buffer-acyclic (n)
  "Like `previous-buffer', but not cyclic."
  (interactive "p")
  (if n
      (let ((bufs (my:filter-list (lambda (b) (not (eq (elt (buffer-name b) 0) ? )))
                          (buffer-list))))
        (switch-to-buffer (nth (mod n (length bufs)) bufs)))
    (switch-to-buffer nil)))


;; Local Variables:
;; eval:(set-regexp-face "^\s*;;\\([{}]*\\)?\\( =+ .* =+\\)?$" 'VioletRed4-bold-italic)
;; eval:(set-regexp-face "^\s*;;\\([{}]*\\)?\\( \\*+ .* \\*+\\)?$" 'Green4-bold-italic)
;; eval:(set-regexp-face "^\s*;;\\([{}]*\\)?\\( \\[+ .* \\]+\\)?$" 'h00688b-bold-italic)
;; End:
