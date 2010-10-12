; Time-stamp: <2010-10-12 11:33:00 cmauclai>

(when (load "auctex" t t)
  (defcustom ConTeXt-users-macro-list nil
    "User's macros list for insertion."
    :group 'ConTeXt-macros
    :type '(repeat (string)))

  (defcustom ConTeXt-define-list nil
    "Define commands list for insertion."
    :group 'ConTeXt-macros
    :type '(repeat (string)))

  (defcustom ConTeXt-setup-list nil
    "Setup commands list for insertion."
    :group 'ConTeXt-macros
    :type '(repeat (string)))

  (defcustom ConTeXt-referencing-list nil
    "Referencing commands list for insertion."
    :group 'ConTeXt-macros
    :type '(repeat (string)))

  (defcustom ConTeXt-other-macro-list nil
    "Other macros list for insertion."
    :group 'ConTeXt-macros
    :type '(repeat (string)))

  (add-to-list 'auto-mode-alist '("\\.mkiv\\'" . context-mode))
  (add-to-list 'auto-mode-alist '("\\.mkii\\'" . context-mode))

  (defmacro my-context:insert (name strbeg strend)
    `(defun ,(intern (concat "my:enclose-in-" (symbol-name name))) (beg end)
       (interactive "r")
       (if (not (region-active-p))
           (progn (setq beg (point))
                  (setq end (point))))
       (goto-char end)
       (insert ,strend)
       (goto-char beg)
       (insert ,strbeg)))

  (defvar etexshow-xml-files-alist nil)
  (when (load "etexshow" t t)
    (setq etexshow-xml-files-alist
      '(("~/emacs/lisp/etexshow/cont-en.xml" .
         "~/emacs/lisp/etexshow/cont-en.cache")
        ("~/emacs/lisp/etexshow/mycommands.xml" .
         "~/emacs/lisp/etexshow/mycommands.cache")))
    (add-hook 'etexshow-mode-hook
              '(lambda () (local-set-key (kbd "<f7>") 'etexshow-quit))))

  (my-context:insert doubt   "\\doute{" "}")
  (my-context:insert english "{\\english " "}")
  (my-context:insert quote   "\\quote{" "}")
  (my-context:insert inline  "|<|" "|>|")

  (defun my-context:view (&optional file command)
    (interactive)
    (let ((file (if (not file) "~/memoir/phd-thesis-memoir.pdf" file))
          (command (if (not command) "pdfopen -viewer xpdf" command)))
      (shell-command (concat command " " file))))


  (dolist (symbol ConTeXt-define-list)
    (TeX-add-symbols (list (concat "define" symbol) nil)))

  (dolist (symbol ConTeXt-setup-list)
    (TeX-add-symbols (list (concat "setup" symbol) nil)))

  (dolist (symbol ConTeXt-referencing-list)
    (TeX-add-symbols (list symbol nil)))

  (dolist (symbol ConTeXt-other-macro-list)
    (TeX-add-symbols (list symbol nil)))

  (dolist (symbol ConTeXt-users-macro-list)
    (TeX-add-symbols (list symbol nil)))

  ; FIXME
  ; (add-to-list 'ConTeXt-environment-helper
  ;              '("MPcode" . ConTeXt-mp-region))

  (defun my-hooks:context-mode ()
    (load "texmathp" t t)

    (when (load "etexshow" t t)
      (defvar etexshow-xml-files-alist
        '(("~/emacs/lisp/etexshow/cont-en.xml" .
           "~/emacs/lisp/etexshow/cont-en.cache")
          ("~/emacs/lisp/etexshow/mycommands.xml" .
           "~/emacs/lisp/etexshow/mycommands.cache")))
      (add-hook 'etexshow-mode-hook
                '(lambda () (local-set-key (kbd "<f7>") 'etexshow-quit))))

    ;; (local-set-key (kbd "C-c C-c") '(lambda () (interactive) (save-buffer) (compile "make -k")))
    ;; (local-set-key (kbd "C-c C-r") '(lambda () (interactive) (recompile)))
    ;; (local-set-key (kbd "C-c C-v") 'my:context-view-memoir)
    ;; (local-set-key (kbd "C-c i")   nil)
    ;; (local-set-key (kbd "C-c i d") 'my:enclose-in-doubt)
    ;; (local-set-key (kbd "C-c i e") 'my:enclose-in-english)
    ;; (local-set-key (kbd "C-c i q") 'my:enclose-in-quote)
    ;; (local-set-key (kbd "C-c i i") 'my:enclose-in-inline)
    ;; (local-set-key (kbd "<f7>")    'etexshow-cmd)
    ;; (local-set-key (kbd "<S-f7>")  'etexshow)
    ;; (local-set-key (kbd " ")       'my:latex-unbreakable-space)
    ;; (local-set-key (kbd "M-q")     'LaTeX-fill-paragraph)

    (my:define-keys 'local
      '("C-c C-c" (lambda () (interactive) (save-buffer) (my:compile "make -k")))
      '("C-c C-v" my:context-view-memoir)
      ; insert strings in the buffer
      '("C-c i"   nil)
      '("C-c i d" my:enclose-in-doubt)
      '("C-c i e" my:enclose-in-english)
      '("C-c i q" my:enclose-in-quote)
      '("C-c i i" my:enclose-in-inline)
      '(" "       my:latex-unbreakable-space)
      ; some help for the commands
      '("<f7>"    etexshow-cmd)
      '("<S-f7>"  etexshow)
      ; other
      '("M-q"     LaTeX-fill-paragraph))

    (auto-fill-mode t)
    (setq fill-column 77))

  (add-hook 'ConTeXt-mode-hook 'my-hooks:context-mode))
