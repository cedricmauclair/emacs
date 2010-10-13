; Time-stamp: <2010-10-13 16:18:06 cmauclai>

(when (load "auctex" t t)
  (defcustom ConTeXt-modes-list nil
    "List of modes to run ConTeXt with."
    :group 'AUCTeX
    :type '(repeat (string)))

  (defcustom ConTeXt-verbose t
    "Switch to turn lots of information at the end of a run."
    :group 'AUCTeX
    :type 'boolean)

  (defcustom ConTeXt-use-makefile t
    "Use a Makefile to compile ConTeXt."
    :group 'AUCTeX
    :type 'boolean)

  (defcustom ConTeXt-use-beta t
    "Use the beta branch of ConTeXt."
    :group 'AUCTeX
    :type 'boolean)

  (if ConTeXt-use-beta
      (setenv "PATH" "/DATA/context/tex/texmf-linux/bin:$PATH" t))

  (defun my:ConTeXt-expand-options ()
    "Expand options for texexec command."
    (concat
     "--autogenerate "
     (unless ConTeXt-verbose
       (format "--nostats "))
     (if TeX-PDF-mode
         (format "--pdf "))
     (unless (eq ConTeXt-current-interface "en")
       (format "--interface=%s " ConTeXt-current-interface))
     (unless TeX-interactive-mode
       ConTeXt-texexec-option-nonstop)
     (if (and ConTeXt-modes-list (not (eq ConTeXt-modes-list "")))
         (progn
           (defvar modes "")
           (setq modes "")
           (dolist (mode ConTeXt-modes-list)
             (setq modes (concat modes mode ",")))
           (format "--mode=%s " modes)))))

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
          '(("~/emacs/lisp/etexshow/cont-en.xml" . "~/emacs/lisp/etexshow/cont-en.cache")
            ("~/emacs/lisp/etexshow/mycommands.xml" . "~/emacs/lisp/etexshow/mycommands.cache")))
    (setq etexshow-comment-file "~/emacs/lisp/etexshow/cont-en-comments.xml" )
    (add-hook 'etexshow-mode-hook
              '(lambda () (local-set-key (kbd "<f7>") 'etexshow-quit))))

  (my-context:insert doubt   "\\doute{" "}")
  (my-context:insert english "{\\english " "}")
  (my-context:insert quote   "\\quote{" "}")
  (my-context:insert inline  "|<|" "|>|")

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

    (my:define-keys 'local
                    '("C-c C-c" (lambda ()
                                  (interactive) (save-buffer) (my:compile "make -k")))
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

  (defun ConTeXt-switch-makefile-AUCTeX ()
    (interactive)
    (if (not ConTeXt-use-makefile)
        (local-set-key (kbd "C-c C-c")
                       (lambda ()
                         (interactive) (save-buffer) (my:compile "make -k")))
      (local-set-key (kbd "C-c C-c") 'TeX-master-command))
    (setq ConTeXt-use-makefile (not ConTeXt-use-makefile)))

  (add-hook 'ConTeXt-mode-hook 'my-hooks:context-mode))
