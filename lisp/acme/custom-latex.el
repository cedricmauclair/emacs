;; Time-stamp: <2010-06-18 10:02:59 cmauclai>


(defvar TeX-shell-escape-mode nil
  "*Allow shell escapes or not.")
(defvar TeX-XeTeX-mode nil
  "Ugly hack used to make PDF ConTeXt work.")
(defvar my:display-math-mode nil
  "Wether to type display math or nat.")
(defvar my:display-math-beginning "\n\\[\n"
  "The string used to insert BEFORE a math display.")
(defvar my:display-math-end "\n\\]\n"
  "The string used to insert AFTER a math display.")
(defvar my:inline-math-beginning "\\( "
  "The string used to insert BEFORE a math inline.")
(defvar my:inline-math-end " \\)"
  "The string used to insert AFTER a math inline.")

(defvar cdlatex-math-modify-prefix 176)
(require 'cdlatex)

(defun my:latex-unbreakable-space ()
  "Insère '~' pour inserer une espace insécable sous LaTeX."
  (interactive)
  (insert "~"))

(defun my:insert-math-formula ()
  "Insère « \\(|\\) » (ou « \\[
|
\\] ») avec le cruseur à la place du « | »."
  (interactive)
  (if (eq last-command this-command)
      (progn
        (delete-char 3)
        (delete-char -3)
        (setq my:display-math-mode (not my:display-math-mode))))
  (if my:display-math-mode
      (insert "\\[

\\]")
    (insert "\\(  \\)"))
  (goto-char (- (point) 3)))


(defun my:toggle-shell-escape ()
  (interactive)
  "(Dés)active l'option de compilation de (La)TeX \"-shell-escape\""
  (setq TeX-shell-escape-mode (not TeX-shell-escape-mode))
  (message (concat "TeX shell escapes " (if TeX-shell-escape-mode "activated" "deactivated"))))

(eval-after-load "tex"
  '(progn
     (require 'font-latex)
     (require 'reftex)))

(eval-after-load "context"
  '(progn
     (require 'font-latex)
     (require 'reftex)))

(eval-after-load "latex"
  '(progn
     (require 'font-latex)
     (require 'reftex)
     (require 'preview-latex)
     (require 'bib-cite)))

(defun my:LaTeX-mode-hook ()
  "Setup LaTeX AUCTeX mode."
  ;; (turn-on-bib-cite) ?
  (turn-on-reftex)
  ;; (turn-on-filladapt-mode)
  (turn-on-auto-fill)
  (imenu-add-menubar-index)
  (local-set-key (kbd "M-$")         'my:insert-math-formula)
  (local-set-key (kbd "M-q")         'LaTeX-fill-paragraph)
  (local-set-key (kbd "<f9>")        'TeX-next-error)
  (local-set-key (kbd "C-c C-t C-t") 'my:toggle-shell-escape)
  (local-set-key (kbd "<")           'self-insert-command)
  (local-set-key (kbd ">")           'self-insert-command)
  (face-remap-add-relative 'font-lock-function-name-face
                           :foreground "OliveDrab")
                           ;; :weight normal)
  (folding-mode t))

(add-hook 'bibtex-mode-hook 'BibTeX-auto-store)
(add-hook 'LaTeX-mode-hook 'my:LaTeX-mode-hook)
