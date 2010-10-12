;; Time-stamp: <2010-10-12 13:01:09 cmauclai>


;; ==== Global (re)definitions =================================================

(my:define-keys 'global
  ;; ** Movements keys **
  '("<next>"  my:scroll-up)     ; scroll in place
  '("C-v"     my:scroll-up)     ; scroll in place
  '("<prior>" my:scroll-down)   ; scroll in place
  '("M-v"     my:scroll-down)   ; scroll in place
  '("M-p"     previous-line)      ; tired to go from "M-f/b" to "C-p"
  '("M-n"     next-line)          ; tired to go from "M-f/b" to "C-n" (my:next-line ?)
  '("C-M-p"   backward-paragraph) ; moved from "M-{"
  '("C-M-n"   forward-paragraph)  ; moved from "M-}"
  '("C-M-a"   backward-list)      ; moved from "C-M-p"
  '("C-M-e"   forward-list)       ; moved from "C-M-n"

  ;; ** Marks/kills **
  '("M-@"   cua-set-mark)    ; was `mark-word', moved to "M-+"
  '("M-+"   mark-word)       ; moved from "M-@"
  '("M-W"   kill-region)     ; tired to go from "M-f/b" to "C-k"
  '("C-S-w" kill-ring-save)  ; tired to go from "C-p/n/b/f" to "M-w"
  '("M-k"   kill-whole-line) ; was forward-sentence, moved to "M-K"
  '("M-K"   kill-sentence)   ; was free

  ;; ** Buffers **
  '("<backtab>"  my:next-buffer-acyclic)
    ; may need to change to [(shift tab)] or [(shit iso-lefttab)]
    ; depending on X server
  '("C-x C-y"    bs-show)
  '("C-x C-b"    ibuffer)
  '("C-x C-à"    switch-to-buffer)
  '("C-x y"      list-buffers)
  '(list-buffers electric-buffer-list)
  '("C-x C-k"    kill-this-buffer) ; swaped with `kmacro-keymap', moved to "C-x k"
  '("C-x k"      kmacro-keymap)    ; swaped with `kill-buffer',   moved to "C-x C-k"

  ;; ** Miscellanous **
  ;; TODO: split differently
  '("C-."   undo)
  '("RET"   newline-and-indent)
  '("M-j"   join-line)
  '("M-J"   join-line t)
  '("M-SPC" dabbrev-expand) ; swaped with just-one-space "M-/"
  '("M-/"   just-one-space) ; swaped with dabbrev-expand "M-SPC"
  '("M-ç"   my:select-text-in-quote)
  '("M-Ç"   my:extend-selection)
  '("M-%"   my:mark-line)
  '("M-D"   my:duplicate-current-line-or-region)
  '("M-#"   my:sanitize)
  '("C-M-r" recursive-edit) ; was `isearch-backward-regexp', moved to "M-r"
  '("<M-up>"   my:go-up-and-toggle-comment-line)
  '("<M-down>" my:toggle-comment-line-and-go-down)
  ;; TODO: look further
  ;; '([(control shift ?a)]        add-color-pattern)
  ;; '([(control shift ?r)]        remove-added-color-pattern)
  ;; '([(control ?x) (control shift ?a)] set-regexp-face)
  ;; '([(control ?x) (control shift ?r)] remove-set-face-overlays)
  ;; '([(meta ?O) (meta ?O)]             set-region-face)
  '("M-'" repeat)
  '("C-'" repeat)
  '("C-«" drag-stuff-left)
  '("C-»" drag-stuff-right)
  '("C-+" drag-stuff-up)
  '("C--" drag-stuff-down)

  ;; ** Insert pairs **
  '("M-*" my:toggle-letter-case)

  ;; ** Macros keys **
  '("M-z" macro-key)
  '("M-S-z" zap-to-char)
  ;; TODO: look further
  ;; '([(control ?`)] self-recording-macro-key)
  ;; '([(control ?~)] self-recording-macro-key)

  ;; TODO: move somewhere else
  ;; '([(control c) (a)]           org-agenda)
  ;; '([(control c) (l)]           org-store-link)

  ;; ** Mouse **
  '("<S-down-mouse-1>" nil)
  '("<S-mouse-1>"      mouse-save-then-kill)
  '("<mouse-3>"        nil)
  '("<down-mouse-3>"   mouse-major-mode-menu)
  '("<S-mouse-3>"      nil)
  '("<S-down-mouse-3>" mouse-set-font)

  ;; ** Function keys **
  ;; TODO: move to some classic keys + more convenient (e.g. M-% is now f5)
  ; f1 - file/buffer operations
  '("<C-f1>"   find-file-at-point)
  '("<S-f1>"   recentf-open-files)
  ; f4 - shell stuff
  '("<f4>"     my:shell-command)
  ;; '("<S-f4>"   term explicit-shell-file-name)
  '("<S-f4>"   my:shell-term explicit-shell-file-name)
  '("<C-f4>"   shell)
  ; f5 - replacing
  '("<f5>"     query-replace)
  '("<S-f5>"   query-replace-regexp)
  '("<M-f5>"   replace-string)
  '("<M-S-f5>" replace-regexp)
  ; f6 - dired stuff
  '("<f6>"     dired)
  '("<S-f6>"   dired default-directory)
  ; f9 - error
  '("<f9>"     next-error)
  ;; f11 - register stuff
  '("<S-f11>"  my:marker)
  '("<M-f11>"  my:marker-copy)
  ;; f12 - window misc
  '("<f12>"    visual-line-mode)
  '("<M-f12>"  linum-mode)
  '("<C-f12>"  hide-local-variable-section t)
  '("<S-f12>"  widen)

  emacs-lisp-mode-map
  '("<M-f8>"   my:byte-compile-this-file))

(my:define-keys
  ;; My help stuff
  help-map
  '("C-q"                       my:emacs-quickref)

  ;; isearch regexp keys:
  'global
  '([(meta ?s)]                 isearch-forward-regexp)
  '([(meta ?r)]                 isearch-backward-regexp)

  read-expression-map
  ;; tab completes a symbol when editing a minibuffer lisp expression
  '([(tab)]                     lisp-complete-symbol)

  isearch-mode-map
  ;; sane yanking
  '([(control ?w)]              isearch-yank-word)
  '([(control ?f)]              isearch-yank-char)
  '([(control ?k)]              isearch-yank-line)
  '([(control ?y)]              isearch-yank-kill)
  ;; some more stuff
  '([(control ?t)]              isearch-toggle-case-fold)
  ;; fix backspace & delete to behave the same in isearch mode
  '([(?\C-?)]                   isearch-delete-char)
  '([(delete)]                  isearch-delete-char)
  '("C-M-r"                     recursive-edit)) ; was `isearch-repeat-backward', moved to "M-r"


;; ==== Dired buffers ==========================================================

;; (if (fboundp 'dired-single-magic-buffer)
;;     (my:define-keys
;;      'global
;;      '("C-x d"    dired-single-magic-buffer)
;;      '("C-x C-d"  dired-single-magic-buffer))

;; (add-hook 'dired-mode-hook
;;           (lambda nil
;;             (my:define-keys 'local
;;                '("<return>" dired-single-buffer)
;;                '("C-m" dired-single-buffer)
;;                ; '([mouse-1] dired-single-buffer-mouse)
;;                ; '([down-mouse-1] dired-single-buffer-mouse)
;;                '("^"       dired-single-buffer "..")))))

;; ==== Info/help modes ========================================================

(add-hook 'Info-mode-hook
          (lambda nil
            (my:define-keys 'local
               '("<backtab>" nil)
               '("M-s"       nil))))


(add-hook 'help-mode-hook
          (lambda nil
            (my:define-keys 'local
               '("M-p"        help-go-back)
               '("M-n"        help-go-back)
               '("<backtab>"  nil)
               '("C-q"        my:emacs-quickref))))
;; (add-hook 'help-mode-hook (lambda nil (setq tm/dont-activate t)))

(eval-after-load "cus-edit"
  '(my:define-keys custom-mode-map
     '("<backtab>" nil)))


;; Local Variables:
;; eval:(set-regexp-face "^\s*;;\\([{}]*\\)?\\( =+ .* =+\\)?$" 'VioletRed4-bold-italic)
;; eval:(set-regexp-face "^\s*;;\\([{}]*\\)?\\( \\*+ .* \\*+\\)?$" 'Green4-bold-italic)
;; eval:(set-regexp-face "^\s*;;\\([{}]*\\)?\\( \\[+ .* \\]+\\)?$" 'h00688b-bold-italic)
;; End:
