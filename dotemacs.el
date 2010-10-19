; Time-stamp: <2010-10-19 11:31:34 cmauclai>

;<< Server >>

(load "server" t t)
(when (and (functionp 'server-running-p) (not (server-running-p)))
  (server-start))

;>>


; ==== Variables and load-path ===================================== ;

;<< Load path >>
(eval-and-compile
  (defconst emacs-root
    (expand-file-name "emacs/" "~")
    "The root directory of your personnal emacs tree.")

  (defconst lisp-root
    (concat emacs-root "lisp/")
    "Root directory of your lisp tree.")

  (defconst more-lisp-dirs
    (list "acme" "auctex" "etexshow")
    "List of directories to add to the load path, relative to
    `lisp-root' or absolute."))

(add-to-list 'load-path lisp-root)
(dolist (dir more-lisp-dirs)
  (add-to-list 'load-path
               (if (file-name-absolute-p dir) dir
                 (concat lisp-root dir))))
;>>
;<< Local variables blocks >>
(defvar hide-local-variable-section nil
  "*Hide the local variables sections of a file.
This is used in the function `hide-local-variable-section' in
`find-file' hook.  The purpose of this variable is to be set in
the local variables section of a file when you want this section
to be hidden.")
(make-variable-buffer-local 'hide-local-variable-section)

(defun hide-local-variable-section (&optional force)
  "See the documentation for the variable `hide-local-variable-section'."
  (interactive)
  (when (or force hide-local-variable-section)
    (let ((locals-start
           (save-excursion
             (goto-char (point-max))
             (search-backward "\n\^L" (max (- (point-max) 3000) (point-min))
                              'move)
             ;; the `\040' is so this line would not be found - otherwise when
             ;; this file is opened we get random error msgs
             (search-forward "Local\040Variables:" nil t)
             (forward-line 0)
             (point))))
      (narrow-to-region (point-min) locals-start))))
;>>


; ==== Packages ==================================================== ;

;<< Macros >>
(eval-and-compile
  (defmacro my:include-content (file &optional ref)
    "Includes the content of `file' if it is readable. If `ref'
    is specified, path is relative to it. If it is nil (default)
    it is relative to `lisp-root'. Otherwise, the file path can
    be absolute."
    (let* ((ref (if (not ref) lisp-root))
           (file (if (not (file-name-absolute-p file))
     (concat ref file))))
      (if (and file (file-readable-p file))
          (with-temp-buffer
            (insert "(progn\n")
            (insert-file-contents file)
            (goto-char (point-max))
            (insert ")")
            (goto-char (point-min))
            (read (current-buffer)))))))
;>>
;<< Packages >>
(eval-and-compile
  (defconst packages-to-load
    (list "dired-x" "drag-stuff" "fic-mode" "filladapt" "folding"
          "gnuplot" "gnuplot-gui" "lua-mode" "mic-paren"
          "rainbow-mode" "textmate" "undo-tree")
    "List of packages to load automatically."))

(my:include-content "acme/my-utils.el")
(my:include-content "acme/my-keybindings.el")
(my:include-content "acme/my-buffers.el")
(my:include-content "acme/my-colors.el")
(my:include-content "acme/my-editing.el")
(my:include-content "acme/my-macros.el")
(my:include-content "acme/my-markers.el")
(my:include-content "acme/my-registers.el")

(dolist (package packages-to-load)
  (load package t t))
;>>


; ==== Hooks ======================================================= ;

;<< Hooks: write-file >>
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'write-file-hooks 'time-stamp)
;>>
;<< Hooks: find-file >>
(put 'hide-local-variable-section 'safe-local-variable 'booleanp)
; (add-hook 'find-file-hooks '(lambda () (hide-local-variable-section t)))
;>>
;<< Hooks: dired >>

(put 'dired-find-alternate-file 'disabled nil)
(setq dired-listing-switches "-al --time-style=long-iso")
(setq dired-omit-files (concat dired-omit-files "\\|.emacs.d\\|.bash.+\\|.rxvt.+\\|.recent.+\\|^\\.[^.]+$"))
(add-hook 'dired-mode-hook 'dired-omit-mode)

;>>
;<< Hooks: folding-mode >>
(defun my-hooks:folding-mode ()
  (local-set-key (kbd "M-g")     'folding-goto-line)
  (local-set-key (kbd "C-c f")    nil)
  (local-set-key (kbd "M-RET")   'folding-toggle-show-hide)
  (local-set-key (kbd "C-c f s") 'folding-show-current-subtree)
  (local-set-key (kbd "C-c f h") 'folding-hide-current-subtree)
  (local-set-key (kbd "C-c f o") 'folding-open-buffer)
  (local-set-key (kbd "C-c f c") 'folding-whole-buffer))
(add-hook 'folding-mode-hook 'my-hooks:folding-mode)
;>>
;<< Hooks: emacs-lisp-mode >>
(add-hook 'emacs-lisp-mode-hook 'folding-mode)
(add-hook 'emacs-lisp-mode-hook 'fic-mode)
(folding-add-to-marks-list 'emacs-lisp-mode ";<<" ";>>")
;>>
;<< Hooks: lua-mode >>
(add-to-list 'auto-mode-alist '("\\.mlua\\'" . lua-mode))
(defun my-hooks:lua-mode ()
  (tm/minor-mode-on)
  (folding-mode t)
  (local-set-key (kbd " ") (lambda nil (interactive) (insert " ")))
  (local-set-key (kbd "C-c C-r") 'lua-restart-with-whole-file))
(add-hook 'lua-mode-hook 'my-hooks:lua-mode)
(folding-add-to-marks-list 'lua-mode "--<<" "-->>")
;>>
;<< Hooks: context-mode >>
(folding-add-to-marks-list 'context-mode "%<<" "%>>")
;>>


; ==== Customs ===================================================== ;

;<< Customs: Variables >>
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ConTeXt-font-list (quote ((2 "{\\bf " "}") (3 "{\\sc " "}") (5 "{\\em " "}") (9 "{\\it " "}") (18 "{\\rm " "}") (19 "{\\sl " "}") (20 "{\\tt " "}") (4 "" "" t) (8 "\\high{" "}") (12 "\\low{" "}") (13 "\\hilo{" "}{}") (6 "{\\english " "}"))))
 '(TeX-auto-save t)
 '(TeX-command-list (quote (("TeX" "%(PDF)%(tex) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil (plain-tex-mode texinfo-mode ams-tex-mode) :help "Run plain TeX") ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX") ("Makeinfo" "makeinfo %t" TeX-run-compile nil (texinfo-mode) :help "Run Makeinfo with Info output") ("Makeinfo HTML" "makeinfo --html %t" TeX-run-compile nil (texinfo-mode) :help "Run Makeinfo with HTML output") ("AmSTeX" "%(PDF)amstex %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil (ams-tex-mode) :help "Run AMSTeX") ("ConTeXt" "context --once %(execopts)%t" TeX-run-TeX nil (context-mode) :help "Run ConTeXt once") ("ConTeXt Full" "context %(execopts)%t" TeX-run-TeX nil (context-mode) :help "Run ConTeXt until completion") ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX") ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer") ("Print" "%p" TeX-run-command t t :help "Print the file") ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command) ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file") ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file") ("Check" "lacheck %s" TeX-run-compile nil (latex-mode) :help "Check LaTeX file for correctness") ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document") ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files") ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files") ("Other" "" TeX-run-command t t :help "Run an arbitrary command"))))
 '(TeX-electric-sub-and-superscript t)
 '(TeX-expand-list (quote (("%p" TeX-printer-query) ("%q" (lambda nil (TeX-printer-query t))) ("%V" (lambda nil (TeX-source-correlate-start-server-maybe) (TeX-view-command-raw))) ("%vv" (lambda nil (TeX-source-correlate-start-server-maybe) (TeX-output-style-check TeX-output-view-style))) ("%v" (lambda nil (TeX-source-correlate-start-server-maybe) (TeX-style-check TeX-view-style))) ("%r" (lambda nil (TeX-style-check TeX-print-style))) ("%l" (lambda nil (TeX-style-check LaTeX-command-style))) ("%(PDF)" (lambda nil (if (and (eq TeX-engine (quote default)) (or TeX-PDF-mode TeX-DVI-via-PDFTeX)) "pdf" ""))) ("%(PDFout)" (lambda nil (cond ((and (eq TeX-engine (quote xetex)) (not TeX-PDF-mode)) " -no-pdf") ((and (eq TeX-engine (quote luatex)) (not TeX-PDF-mode)) " --output-format=dvi") ((and (eq TeX-engine (quote default)) (not TeX-PDF-mode) TeX-DVI-via-PDFTeX) " \"\\pdfoutput=0 \"") (t "")))) ("%(mode)" (lambda nil (if TeX-interactive-mode "" " -interaction=nonstopmode"))) ("%(o?)" (lambda nil (if (eq TeX-engine (quote omega)) "o" ""))) ("%(tex)" (lambda nil (eval (nth 2 (assq TeX-engine (TeX-engine-alist)))))) ("%(latex)" (lambda nil (eval (nth 3 (assq TeX-engine (TeX-engine-alist)))))) ("%(execopts)" my:ConTeXt-expand-options) ("%S" TeX-source-correlate-expand-options) ("%dS" TeX-source-specials-view-expand-options) ("%cS" TeX-source-specials-view-expand-client) ("%(outpage)" (lambda nil (if TeX-source-correlate-output-page-function (funcall TeX-source-correlate-output-page-function) "1"))) ("%s" file nil t) ("%t" file t t) ("%`" (lambda nil (setq TeX-command-pos t TeX-command-text ""))) (" \"\\" (lambda nil (if (eq TeX-command-pos t) (setq TeX-command-pos pos pos (+ 3 pos)) (setq pos (1+ pos))))) ("\"" (lambda nil (if (numberp TeX-command-pos) (setq TeX-command-text (concat TeX-command-text (substring command TeX-command-pos (1+ pos))) command (concat (substring command 0 TeX-command-pos) (substring command (1+ pos))) pos TeX-command-pos TeX-command-pos t) (setq pos (1+ pos))))) ("%'" (lambda nil (prog1 (if (stringp TeX-command-text) (progn (setq pos (+ (length TeX-command-text) 9) TeX-command-pos (and (string-match " " (funcall file t t)) "\"")) (concat TeX-command-text " \"\\input\"")) (setq TeX-command-pos nil) "") (setq TeX-command-text nil)))) ("%n" TeX-current-line) ("%d" file "dvi" t) ("%f" file "ps" t) ("%o" (lambda nil (funcall file (TeX-output-extension) t))) ("%b" TeX-current-file-name-master-relative) ("%m" preview-create-subdirectory))))
 '(TeX-output-view-style (quote (("^dvi$" ("^landscape$" "^pstricks$\\|^pst-\\|^psfrag$") "%(o?)dvips -t landscape %d -o && gv %f") ("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "%(o?)dvips %d -o && gv %f") ("^dvi$" ("^\\(?:a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4\\)$" "^landscape$") "%(o?)xdvi %dS -paper a4r -s 0 %d") ("^dvi$" "^\\(?:a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4\\)$" "%(o?)xdvi %dS -paper a4 %d") ("^dvi$" ("^\\(?:a5\\(?:comb\\|paper\\)\\)$" "^landscape$") "%(o?)xdvi %dS -paper a5r -s 0 %d") ("^dvi$" "^\\(?:a5\\(?:comb\\|paper\\)\\)$" "%(o?)xdvi %dS -paper a5 %d") ("^dvi$" "^b5paper$" "%(o?)xdvi %dS -paper b5 %d") ("^dvi$" "^letterpaper$" "%(o?)xdvi %dS -paper us %d") ("^dvi$" "^legalpaper$" "%(o?)xdvi %dS -paper legal %d") ("^dvi$" "^executivepaper$" "%(o?)xdvi %dS -paper 7.25x10.5in %d") ("^dvi$" "." "%(o?)xdvi %dS %d") ("^pdf$" "." "pdfopen -viewer xpdf %s %o %(outpage)") ("^html?$" "." "netscape %o"))))
 '(TeX-parse-self t)
 '(TeX-PDF-mode t)
 '(TeX-save-query nil)
 '(TeX-view-program-selection (cond ((eq system-type (quote windows-nt)) (quote (((output-dvi style-pstricks) "dvips and start") (output-dvi "Yap") (output-pdf "start") (output-html "start")))) (t (quote (((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf "xpdf") (output-html "xdg-open"))))))
 '(ansi-color-for-comint-mode t)
 '(auto-image-file-mode t)
 '(autopair-autowrap t)
 '(autopair-global-mode 1)
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "~/.backups"))))
 '(blink-cursor-mode nil)
 '(bs-alternative-configuration "all-intern-last")
 '(bs-default-configuration "files-and-scratch")
 '(bs-default-sort-name "by mode")
 '(calendar-date-style (quote iso))
 '(calendar-latitude 43.6)
 '(calendar-longitude 1.433333)
 '(calendar-mark-holidays-flag t)
 '(calendar-view-holidays-initially-flag nil)
 '(calendar-week-start-day 1)
 '(cdlatex-math-modify-prefix 176)
 '(color-theme-is-cumulative nil)
 '(column-number-mode t)
 '(comint-prompt-read-only t)
 '(compilation-scroll-output t)
 '(completion-ignored-extensions (append (quote (".bak" "~" "#" ".o" ".obj" ".lib" ".elc" ".exe" ".com" ".zo" ".arj" ".lza" ".lha" ".arc" ".zoo" ".log" ".synctex")) completion-ignored-extensions))
 '(cua-enable-cua-keys nil)
 '(cua-mode t nil (cua-base))
 '(current-language-environment "UTF-8")
 '(custom-buffer-done-kill t)
 '(custom-group-tag-faces nil)
 '(default-input-method nil)
 '(delete-old-versions t)
 '(delete-selection-mode t)
 '(directory-free-space-args "-Pkh")
 '(display-time-format "[ %H:%M -- %d/%m ]")
 '(display-time-mode t)
 '(european-calendar-style t)
 '(ffap-newfile-prompt t)
 '(fic-background-color nil)
 '(fic-foreground-color "red4")
 '(fill-column 72)
 '(fill-nobreak-predicate (quote (fill-french-nobreak-p)))
 '(font-latex-deactivated-keyword-classes nil)
 '(font-latex-fontify-script t)
 '(font-latex-fontify-sectioning 1.1)
 '(font-latex-is-Emacs20 t)
 '(font-latex-match-bold-command-keywords (quote (("usemodule" "[") ("starttext" "") ("stoptext" ""))))
 '(font-latex-match-bold-declaration-keywords (quote (("bold" ""))))
 '(font-latex-match-function-keywords (quote (("startenumerate" "") ("stopenumerate" "") ("framed" "[{") ("doifmode" "{{") ("doifmodeelse" "{{{") ("doifnotmode" "{{") ("doiftext" "{{") ("doiftextelse" "{{{") ("definepagebreak" "[[") ("definefontfeature" "[[[") ("startsectionblockenvironment" "[") ("stopsectionblockenvironment" "") ("startsetups" "") ("stopsetups" "") ("setups" "[{") ("setlayer" "[[{") ("setlayerframed" "[[[{") ("definelayer" "[[") ("defineselector" "[[") ("setupselector" "[[") ("definepagebreack" "[[") ("placeheadtext" "[") ("placerawheadtext" "[") ("placeheadnumber" "[") ("placerawheadnumber" "[") ("definestructureconversionset" "[[[") ("setupuserpagenumber" "[") ("paperwidth" "") ("paperheight" "") ("placetable" "[[{") ("usetypescript" "[") ("definetypeface" "[[[[[[") ("starttypescript" "[") ("stoptypescript" "") ("blank" "[") ("setupitemize" "[[") ("completecontent" "[") ("placecontent" "[") ("placebookmarks" "[[") ("placecombinedlist" "[[") ("placefigure" "[[{") ("externalfigure" "[[") ("placefootnotes" "[") ("placeformula" "[") ("placelegend" "{{") ("placelist" "[[") ("placelistoffloats" "") ("placelistofsorts" "") ("placelistofsynonyms" "") ("placelocalfootnotes" "[") ("placelogos" "[") ("placeongrid" "[{") ("placeontopofeachother" "{{") ("placereferencelist" "[") ("placeregister" "[[") ("placerule" "[") ("placesidebyside" "{{") ("placesubformula" "[") ("placetextvariable" "[") ("startalignment" "[[") ("startbackground" "[[") ("startbuffer" "[[") ("startcolor" "[[") ("startcolumns" "[[") ("startcombination" "[[") ("startcomment" "[[") ("startcomponent" "[[") ("startdescription" "[[") ("startdocument" "[[") ("startenumeration" "[[") ("startenvironment" "[[") ("startfact" "[[") ("startfigure" "[[") ("startfloattext" "[[") ("startformula" "[[") ("startframedtext" "[[") ("starthiding" "[[") ("startinteractionmenu" "[[") ("startitemize" "[") ("startlegend" "[[") ("startline" "[[") ("startlinecorrection" "[[") ("startlinenumbering" "[[") ("startlines" "[[") ("startlocal" "[[") ("startlocalenvironment" "[[") ("startlocalfootnotes" "[[") ("startmakeup" "[[") ("startmarginblock" "[[") ("startmarginrule" "[[") ("startnamemakeup" "[[") ("startnarrower" "[[") ("startopposite" "[[") ("startoverlay" "[[") ("startoverview" "[[") ("startpacked" "[[") ("startparagraph" "[[") ("startpositioning" "[[") ("startpostponing" "[[") ("startproduct" "[[") ("startprofile" "[[") ("startproject" "[[") ("startquotation" "[[") ("startregister" "[[") ("startsymbolset" "[[") ("startsynchronization" "[[") ("starttable" "[[") ("starttables" "[[") ("starttabulate" "[[") ("starttextrule" "[[") ("starttyping" "[[") ("startunpacked" "[[") ("startversion" "[[") ("stopalignment" "") ("stopbackground" "") ("stopbuffer" "") ("stopcolor" "") ("stopcolumns" "") ("stopcombination" "") ("stopcomment" "") ("stopcomponent" "") ("stopdescription" "") ("stopdocument" "") ("stopenumeration" "") ("stopenvironment" "") ("stopfact" "") ("stopfigure" "") ("stopfloattext" "") ("stopformula" "") ("stopframedtext" "") ("stophiding" "") ("stopinteractionmenu" "") ("stopitemize" "[") ("stoplegend" "") ("stopline" "") ("stoplinecorrection" "") ("stoplinenumbering" "") ("stoplines" "") ("stoplocal" "") ("stoplocalenvironment" "") ("stoplocalfootnotes" "") ("stopmakeup" "") ("stopmarginblock" "") ("stopmarginrule" "") ("stopnamemakeup" "") ("stopnarrower" "") ("stopopposite" "") ("stopoverlay" "") ("stopoverview" "") ("stoppacked" "") ("stopparagraph" "") ("stoppositioning" "") ("stoppostponing" "") ("stopproduct" "") ("stopprofile" "") ("stopproject" "") ("stopquotation" "") ("stopregister" "") ("stopsymbolset" "") ("stopsynchronization" "") ("stoptable" "") ("stoptables" "") ("stoptabulate" "") ("stoptextrule" "") ("stoptyping" "") ("stopunpacked" "") ("stopversion" "") ("define" "[[") ("defineblank" "[[") ("defineblock" "[[") ("definebodyfont" "[[") ("definebodyfontenvironment" "[[") ("definebuffer" "[[") ("definecolor" "[[") ("definecolorgroup" "[[") ("definecombinedlist" "[[[") ("defineconversion" "[[") ("definedescription" "[[") ("defineenumeration" "[[[") ("definefield" "[[") ("definefieldstack" "[[") ("definefiguresymbol" "[[") ("definefloat" "[[") ("definefont" "[[") ("defineframed" "[[") ("defineframedtext" "[[") ("definehead" "[[") ("defineindenting" "[[") ("defineinteractionmenu" "[[") ("defineinteractionmenu (2)" "[[") ("definelabel" "[[") ("definelist" "[[[") ("definelogo" "[[") ("definemakeup" "[[") ("definemarking" "[[") ("defineoutput" "[[") ("defineoverlay" "[[") ("definepalet" "[[") ("definepapersize" "[[") ("defineparagraphs" "[[") ("defineprofile" "[[") ("defineprogram" "[[") ("definerawfont" "[[") ("definereference" "[[") ("definereferenceformat" "[[") ("definereferencelist" "[[") ("defineregister" "[[") ("definerule" "[[") ("definesection" "[[") ("definesectionblock" "[[") ("definesorting" "[[") ("definestartstop" "[[") ("definesubfield" "[[") ("definesymbol" "[[") ("definesynonyms" "[[[") ("definetabletemplate" "[[") ("definetabulate" "[[") ("definetext" "[[") ("definetextposition" "[[") ("definetextvariable" "[[") ("definetype" "[[") ("definetyping" "[[") ("defineversion" "[[") ("setuppapersize" "[[") ("setuplayout" "[") ("setupenumerations" "[[") ("setupalign" "[[") ("setuparranging" "[[") ("setupbackground" "[[") ("setupbackgrounds" "[[") ("setupblackrules" "[[") ("setupblank" "[[") ("setupblock" "[[") ("setupbodyfont" "[[") ("setupbodyfontenvironment" "[[") ("setupbottom" "[[") ("setupbottomtexts" "[[[") ("setupbuffer" "[[") ("setupbuttons" "[[") ("setupcapitals" "[[") ("setupcaption" "[[") ("setupcaptions" "[[") ("setupclipping" "[[") ("setupcolor" "[[") ("setupcolors" "[[") ("setupcolumns" "[[") ("setupcombinations" "[[") ("setupcombinedlist" "[[") ("setupcomment" "[[") ("setupdescriptions" "[[") ("setupenumerations" "[[") ("setupexternalfigures" "[[") ("setupfield" "[[") ("setupfields" "[[") ("setupfillinlines" "[[") ("setupfillinrules" "[[") ("setupfloat" "[[") ("setupfloats" "[[") ("setupfloatsplitting" "[[") ("setupfooter" "[[") ("setupfootertexts" "[[[[") ("setupfootnotedefinition" "[[") ("setupfootnotes" "[[") ("setupforms" "[[") ("setupformulae" "[[") ("setupframed" "[[") ("setupframedtexts" "[[") ("setuphead" "[[") ("setupheader" "[[") ("setupheadertexts" "[[[[") ("setupheadnumber" "[[") ("setupheads" "[[") ("setupheadtext" "[[") ("setuphyphenmark" "[[") ("setupindentations" "[[") ("setupindenting" "[[") ("setupinmargin" "[[") ("setupinteraction" "[[") ("setupinteractionbar" "[[") ("setupinteractionscreen" "[[") ("setupinterlinespace" "[[") ("setupitemgroup" "[[") ("setupitems" "[[") ("setuplabeltext" "[[") ("setuplanguage" "[[") ("setuplayout" "[[") ("setuplegend" "[[") ("setuplinenumbering" "[[") ("setuplines" "[[") ("setuplinewidth" "[[") ("setuplist" "[[") ("setupmakeup" "[[") ("setupmarginblocks" "[[") ("setupmarginrules" "[[") ("setupmarking" "[[") ("setupnarrower" "[[") ("setupnumbering" "[[") ("setupoppositeplacing" "[[") ("setupoutput" "[[") ("setuppagenumber" "[[") ("setuppagenumbering" "[[") ("setuppagetransitions" "[[") ("setuppalet" "[[") ("setuppaper" "[[") ("setuppapersize" "[[") ("setupparagraphnumbering" "[[") ("setupparagraphs" "[[") ("setuppositioning" "[[") ("setupprofiles" "[[") ("setupprograms" "[[") ("setuppublications" "[[") ("setupquote" "[[") ("setupreferencelist" "[[") ("setupreferencing" "[[") ("setupregister" "[[") ("setuprotate" "[[") ("setuprule" "[[") ("setupscreens" "[[") ("setupsection" "[[") ("setupsectionblock" "[[") ("setupsorting" "[[") ("setupspacing" "[[") ("setupstrut" "[[") ("setupsubpagenumber" "[[") ("setupsymbolset" "[[") ("setupsynchronization" "[[") ("setupsynchronizationbar" "[[") ("setupsynonyms" "[[") ("setupsystem" "[[") ("setuptab" "[[") ("setuptables" "[[") ("setuptabulate" "[[") ("setuptext" "[[") ("setuptextposition" "[[") ("setuptextrules" "[[") ("setuptexttexts" "[[") ("setuptextvariable" "[[") ("setupthinrules" "[[") ("setuptolerance" "[[") ("setuptop" "[[") ("setuptoptexts" "[[[") ("setuptype" "[[") ("setuptyping" "[[") ("setupunderbar" "[[") ("setupurl" "[[") ("setupversions" "[[") ("setupwhitespace" "[[") ("showsetups" "[[") ("Use" "{[{"))))
 '(font-latex-match-italic-command-keywords nil)
 '(font-latex-match-italic-declaration-keywords (quote (("italic" "") ("slanted" "") ("english" ""))))
 '(font-latex-match-math-command-keywords (quote (("math" "{") ("mathematics" "{"))))
 '(font-latex-match-reference-keywords (quote (("NN" "[") ("NC" "[") ("AR" "[") ("FR" "[") ("MR" "[") ("LR" "[") ("HL" "[") ("NR" "[") ("SR" "[") ("DL" "[") ("DC" "[") ("about" "[|{{[") ("at" "[|{{[") ("atpage" "[") ("in" "[|{{[") ("inchp" "[") ("insec" "[") ("infig" "[") ("intab" "[") ("ineq" "[") ("ref" "[[") ("reference" "[["))))
 '(font-latex-match-sectioning-0-keywords (quote (("startpart" "[[") ("stoppart" ""))))
 '(font-latex-match-sectioning-1-keywords (quote (("title" "[{") ("startchapter" "[[") ("stopchapter"))))
 '(font-latex-match-sectioning-2-keywords (quote (("subject" "[{") ("startsection" "[[") ("stopsection"))))
 '(font-latex-match-sectioning-3-keywords (quote (("subsubject" "[{") ("startsubsection" "[[") ("stopsubsection"))))
 '(font-latex-match-sectioning-4-keywords (quote (("subsubsubject" "[{") ("startsubsubsection" "[[") ("stopsubsubsection"))))
 '(font-latex-match-sectioning-5-keywords (quote (("subsubsubsubject" "[{") ("startsubsubsubsection" "[[") ("stopsubsubsubsection"))))
 '(font-latex-match-slide-title-keywords nil)
 '(font-latex-match-textual-keywords nil)
 '(font-latex-match-type-command-keywords nil)
 '(font-latex-match-type-declaration-keywords nil)
 '(font-latex-match-variable-keywords nil)
 '(font-latex-match-warning-keywords (quote (("break" "") ("unprotect" "") ("protect" "") ("todo" "{"))))
 '(font-lock-display-type (quote color))
 '(font-lock-global-modes t)
 '(fringe-mode 0 nil (fringe))
 '(global-auto-revert-mode t)
 '(global-visual-line-mode nil)
 '(grep-command "grep -nH -e ")
 '(grep-find-command "find . -type f -print0 | xargs -0 -e grep -nH -e ")
 '(grep-find-template "find . <X> -type f <F> -print0 | xargs -0 -e grep <C> -nH -e <R>")
 '(grep-highlight-matches t)
 '(grep-scroll-output t)
 '(grep-template "grep <C> -nH -e <R> <F>")
 '(grep-use-null-device nil)
 '(help-window-select t)
 '(holiday-local-holidays (quote ((holiday-fixed 7 14 "Bastille Day"))))
 '(holiday-other-holidays (quote ((holiday-fixed 5 8 "WWII") (holiday-fixed 11 11 "WWI"))))
 '(ibuffer-compressed-file-name-regexp "\\.\\(arj\\|bgz\\|bz2\\|gz\\|lzh\\|taz\\|tbz\\|tgz\\|zip\\|z\\)$")
 '(ibuffer-default-shrink-to-minimum-size nil)
 '(ibuffer-default-sorting-mode (quote major-mode))
 '(ibuffer-old-time 24)
 '(ibuffer-saved-filters (quote (("gnus" ((or (mode . message-mode) (mode . mail-mode) (mode . gnus-group-mode) (mode . gnus-summary-mode) (mode . gnus-article-mode)))) ("programming" ((or (mode . emacs-lisp-mode) (mode . cperl-mode) (mode . c-mode) (mode . java-mode) (mode . idl-mode) (mode . lisp-mode) (mode . lua-mode) (mode . f90) (mode . fortran)))) ("TeX/LaTeX/ConTeXt" ((or (mode . tex) (mode . latex) (mode . TeX) (mode . LaTeX) (mode . context)))))))
 '(ibuffer-title-face (quote ibuffer-header-face))
 '(image-dired-main-image-directory "~/pictures/")
 '(indent-tabs-mode nil)
 '(inhibit-local-menu-bar-menus t)
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startup-screen t)
 '(ispell-program-name "aspell")
 '(iswitchb-buffer-ignore (quote ("^\\( \\|*\\)")))
 '(iswitchb-max-to-show 6)
 '(iswitchb-mode t)
 '(iswitchb-use-virtual-buffers t nil (recentf))
 '(kill-read-only-ok t)
 '(kill-whole-line t)
 '(menu-bar-mode nil)
 '(msb-files-by-directory nil)
 '(msb-menu-cond (quote (((and (boundp (quote server-buffer-clients)) server-buffer-clients (quote multi)) 3030 "Clients (%d)") ((and msb-display-invisible-buffers-p (msb-invisible-buffer-p) (quote multi)) 3090 "Invisible buffers (%d)") ((eq major-mode (quote dired-mode)) 2010 "Dired (%d)" msb-dired-item-handler msb-sort-by-directory) ((eq major-mode (quote Man-mode)) 4090 "Manuals (%d)") ((eq major-mode (quote w3-mode)) 4020 "WWW (%d)") ((or (memq major-mode (quote (rmail-mode rmail-edit-mode vm-summary-mode vm-mode mail-mode))) (memq major-mode (quote (mh-letter-mode mh-show-mode mh-folder-mode))) (memq major-mode (quote (gnus-summary-mode message-mode gnus-group-mode gnus-article-mode score-mode gnus-browse-killed-mode)))) 4010 "Mail (%d)") ((not buffer-file-name) 4099 "Buffers (%d)") ((quote no-multi) 1099 "Files (%d)"))))
 '(msb-mode t)
 '(paren-match-face (quote show-paren-match))
 '(paren-mismatch-face (quote show-paren-mismatch))
 '(parse-sexp-ignore-comments t)
 '(partial-completion-mode t)
 '(pr-gs-switches (quote ("-q -dNOPAUSE -I/usr/share/ghostscript/8.64")))
 '(preview-auto-cache-preamble t)
 '(preview-scale-function 1.4)
 '(proced-auto-update-flag t)
 '(proced-tree-flag t)
 '(read-buffer-function (quote iswitchb-read-buffer))
 '(read-file-name-completion-ignore-case t)
 '(recentf-arrange-rules (quote (("Elisp (%d)" ".\\.\\(el\\|emacs\\)\\'") ("Java (%d)" ".\\.java\\'") ("C/C++ (%d)" ".\\.c\\(pp\\)?\\'") ("Lua (%d)" ".\\.lua\\'") ("Archives (%d)" ".\\.\\(tar\\.\\(gz\\|bz2\\)\\|tgz\\|tbz\\)\\'") ("TeX/LaTeX/ConTeXt (%d)" ".\\.\\(\\(la\\|b\\)?tex\\|ltx\\)\\'"))))
 '(recentf-filter-changer-alist (quote ((recentf-arrange-by-rule . "Grouped by Custom Rules") (recentf-arrange-by-dir . "Grouped by Directory") (recentf-arrange-by-mode . "Grouped by Mode"))))
 '(recentf-max-saved-items 50)
 '(recentf-menu-append-commands-flag nil)
 '(recentf-menu-filter (quote recentf-filter-changer))
 '(recentf-menu-open-all-flag t)
 '(recentf-mode t)
 '(require-final-newline t)
 '(rlogin-program "ssh")
 '(rlogin-remote-user (user-login-name))
 '(safe-local-eval-forms (quote ((add-hook (quote write-file-functions) (quote time-stamp)))))
 '(safe-local-variable-values (quote ((eval folding-mode t) (eval rainbow-mode t) (eval auto-fill-mode t) (eval auto-fill-mode nil) (reftex-mode . t) (TeX-source-correlate-method . source-specials) (TeX-source-correlate-method-active . source-specials))))
 '(scalable-fonts-allowed t)
 '(scroll-bar-mode nil)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
 '(tab-always-indent (quote complete))
 '(tab-width 4)
 '(temp-buffer-max-height 15)
 '(temp-buffer-resize-mode t)
 '(tm/use-open-next-line nil)
 '(tool-bar-mode nil)
 '(track-eol nil)
 '(tramp-backup-directory-alist backup-directory-alist)
 '(tramp-default-user (user-login-name))
 '(truncate-lines t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(version-control t)
 '(view-inhibit-help-message t)
 '(view-read-only t)
 '(visual-line-fringe-indicators (quote (left-curly-arrow right-curly-arrow)))
 '(windmove-wrap-around t)
 '(window-min-height 5)
 '(word-wrap t)
 '(yank-pop-change-selection t))

;; Needs some fixing: should be automatic
(modify-face 'font-lock-fic-face fic-foreground-color
             fic-background-color nil t nil nil nil nil)
;>>
;<< Customs: Faces >>
(make-face 'ibuffer-header-face)
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default (
            ;; (((type x) (class color) (min-colors 88) (background dark)) (:inherit nil :stipple nil :background "Grey5" :foreground "Grey75" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))
            ;; (((type x) (class color) (min-colors 88) (background light)) (:inherit nil :stipple nil :background "#EEEEDD" :foreground "#262626" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
            (((type x) (class color) (min-colors 88) (background dark)) (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))
            (((type x) (class color) (min-colors 88) (background light)) (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(bold    (
            (((supports :weight bold)) (:weight bold))))
 '(bold-italic (
                (((supports :weight bold :slant italic)) (:slant italic :weight bold))
                (t (:inherit underline))))
 '(border  (
            (t nil)))
 '(button  (
            (((type x) (class color) (min-colors 88)) (:foreground "light blue"))
            (((supports :underline t)) (:underline t))))
 '(completions-first-difference (
            (((type x) (class color) (min-colors 88)) (:inherit bold :background "red4" :foreground "yellow"))))
 '(cursor  (
            (((type x) (class color) (min-colors 88) (background dark)) (:background "#55A096" :foreground "white"))))
 '(custom-button (
            (((type x) (class color grayscale)) (:background "light grey" :foreground "gray20" :box (:line-width 1 :color "dim gray")))))
 '(custom-button-mouse (
            (((type x) (class color grayscale)) (:background "dim gray" :foreground "white" :box (:line-width 1 :color "dim gray")))
            (t (:inverse-video t))))
 '(custom-button-pressed (
            (((type x) (class color grayscale)) (:inherit custom-button :foreground "white"))
            (t (:inverse-video t))))
 '(custom-button-pressed-unraised (
            (default (:inherit custom-button-unraised))
            (((class color) (background light)) (:foreground "magenta4"))))
 '(custom-button-unraised (
            (default (:inherit underline))))
 '(custom-comment (
            (((type x) (class grayscale color)) (:background "gray85"))))
 '(custom-comment-tag (
            (((type x) (class color) (min-colors 88)) (:foreground "blue4"))))
 '(custom-documentation (
            (t (:inherit default))))                ; (t (:inherit italic)) is also nice
 '(custom-face-tag (
            (default (:inherit custom-variable-tag))))
 '(custom-group-tag (
            (default (:weight bold))
            (((type x) (class color) (min-colors 88) (background dark)) (:foreground "dark orange"))
            (((type x) (class color) (min-colors 88) (background light)) (:foreground "dark red"))))
 '(custom-link (
            (default (:inherit link))))
 '(custom-state (
            (((type x) (class color) (min-colors 88) (background dark)) (:foreground "lawn green"))
            (((type x) (class color) (min-colors 88) (background light)) (:foreground "sea green"))))
 '(custom-variable-button (
            (((type x) (class color) (min-colors 88) (background dark)) (:underline t :weight bold))))
 '(custom-variable-tag (
            (default (:inherit bold))
            (((type x) (class color) (min-colors 88)) (:foreground "DodgerBlue3"))))
 '(custom-visibility (
            (default (:inherit link))))
 '(dired-directory (
            (default (:inherit font-lock-function-name-face))))
 '(dired-flagged (
            (default (:inherit font-lock-warning-face))))
 '(dired-header (
            (default (:inherit bold))))
 '(dired-ignored (
            (default (:inherit shadow))))
 '(dired-mark (
            (default (:inherit font-lock-constant-face))))
 '(dired-marked (
            (default (:inherit font-lock-constant-face))))
 '(dired-perm-write (
            (default (:inherit font-lock-warning-face))))
 '(dired-symlink (
            (default (:inherit font-lock-keyword-face :slant italic))))
 '(dired-warning (
            (default (:inherit font-lock-warning-face))))
 '(escape-glyph (
            (((type x) (class color) (min-colors 88)) (:foreground "brown1"))))
 '(fixed-pitch (
            (((supports :height 0.9)) (:height 0.9))
            (t (:family "Monospace"))))
 '(font-latex-bold-face (
            (default (:inherit bold))))
 '(font-latex-doctex-documentation-face (
            (((type x) (class color) (min-colors 88)) (:background "#333"))))
 '(font-latex-doctex-preprocessor-face (
            (((type x) (class color) (min-colors 88)) (:inherit (font-latex-doctex-documentation-face font-lock-preprocessor-face)))))
 '(font-latex-italic-face (
            (default (:inherit italic))))
 '(font-latex-math-face (
            (default (:inherit italic))
            (((type x) (class color) (min-colors 88)) (:foreground "#89E14B")))) ; "dark khaki" or "#89E14B"
 '(font-latex-sectioning-5-face (
            (default (:inherit (bold font-lock-type-face variable-pitch)))))
 '(font-latex-slide-title-face (
            (default (:inherit font-latex-sectioning-2-face))))
 '(font-latex-string-face (
            (default (:inherit font-lock-string-face))))
 '(font-latex-verbatim-face (
            (default (:inherit fixed-pitch))))
 '(font-latex-warning-face (
            (default (:inherit font-lock-warning-face))))
 '(font-lock-builtin-face (
            (((type x) (class color) (min-colors 88) (background dark)) (:foreground "#8AC6F2"))
            (((type x) (class color) (min-colors 88) (background light)) (:foreground "#8B008B"))))
 '(font-lock-comment-delimiter-face (
            (default (:inherit font-lock-comment-face))))
 '(font-lock-comment-face (
            (((type x) (class color) (min-colors 88) (background dark)) (:foreground "gray40"))
            (((type x) (class color) (min-colors 88) (background light)) (:foreground "gray60"))))
 '(font-lock-constant-face (
            (((type x) (class color) (min-colors 88) (background dark)) (:foreground "#00688B"))
            (((type x) (class color) (min-colors 88) (background light)) (:foreground "#B452CD"))))
 '(font-lock-doc-face (
            (default (:inherit italic))
            (((type x) (class color) (min-colors 88) (background dark)) (:foreground "#96DEFA"))
            (((type x) (class color) (min-colors 88) (background light)) (:foreground "VioletRed4"))))
 '(font-lock-function-name-face (
            (((type x) (class color) (min-colors 88) (background dark)) (:foreground "goldenrod")); or "dark orange"
            (((type x) (class color) (min-colors 88) (background light)) (:foreground "#00688B"))
            (t (:inherit bold))))
 '(font-lock-keyword-face (
            (((type x) (class color) (min-colors 88) (background dark)) (:foreground "steel blue"))
            (((type x) (class color) (min-colors 88) (background light)) (:foreground "purple"))))
 '(font-lock-preprocessor-face (
            (default (:inherit (font-lock-builtin-face bold)))))
 '(font-lock-regexp-grouping-backslash (
            (default (:inherit bold))
            (((type x) (class color) (min-colors 88)) (:underline "red2"))))
 '(font-lock-regexp-grouping-construct (
            (default (:inherit font-lock-regexp-grouping-backslash))))
 '(font-lock-string-face (
            (default (:inherit italic))
            (((type x) (class color) (min-colors 88) (background dark)) (:foreground "#89E14B"))
            (((type x) (class color) (min-colors 88) (background light)) (:foreground "#CD5555"))))
 '(font-lock-type-face (
            (((type x) (class color) (min-colors 88) (background dark)) (:foreground "yellow3"))
            (((type x) (class color) (min-colors 88) (background light)) (:foreground "green4"))))
 '(font-lock-variable-name-face (
            (((type x) (class color) (min-colors 88) (background dark)) (:foreground "#ADED80"))
            (((type x) (class color) (min-colors 88) (background light)) (:foreground "#00688B"))))
 '(font-lock-warning-face (
            (default (:weight bold))
            (((type x) (class color) (min-colors 88)) (:foreground "red4"))))
 '(fringe (
            (((type x) (class color) (min-colors 88)) (:background "gray75"))))
 '(header-line (
            (default (:inherit (mode-line bold)))))
 '(highlight (
            (((type x) (class color) (min-colors 88) (background dark)) (:background "#18374F"))
            (((type x) (class color) (min-colors 88) (background light)) (:background "DarkSeaGreen2"))))
 '(highlight-current-line-face (
            (t (:inverse-video t))))
 '(ibuffer-header-face (
            (default (:inherit (font-lock-type-face bold)))))
 '(isearch (
            (default (:inherit bold))
            (((type x) (class color) (background dark)) (:background "#AF81F4" :foreground "#E2DAEF"))
            (((type x) (class color) (background light)) (:background "DeepSkyBlue4" :foreground "LightSkyBlue1"))))
 '(italic  (
            (((supports :slant italic)) (:slant italic))
            (t (:inherit underline))))
 '(lazy-highlight (
            (((type x) (class color) (min-colors 88) (background dark)) (:background "PaleTurquoise4"))
            (((type x) (class color) (min-colors 88) (background light)) (:background "pale turquoise"))))
 '(link (
            (((type x) (class color) (background dark)) (:background "ivory3" :foreground "DodgerBlue4" :box (:line-width 1 :color "DodgerBlue4") :underline nil))
            (((type x) (class color) (background light)) (:background "ivory2" :foreground "DodgerBlue3" :box (:line-width 1 :color "DodgerBlue3") :underline nil))))
 '(link-visited (
            (default (:inherit link))
            (((type x) (class color) (background dark)) (:foreground "VioletRed2" :box (:line-width 1 :color "VioletRed2")))
            (((type x) (class color) (background light)) (:foreground "magenta4" :box (:line-width 1 :color "magenta4")))))
 '(match (
            (((class color) (min-colors 88) (background dark)) (:background "light blue" :foreground "gray5"))
            (((class color) (min-colors 88) (background light)) (:background "yellow2"))
            (t (:background "grey"))))
 '(menu (
            (((type x-toolkit)) nil)))
 '(minibuffer-prompt (
            (default (:inherit bold))
            (((type x) (class color) (min-colors 88) (background dark)) (:foreground "SteelBlue3"))
            (((type x) (class color) (min-colors 88) (background light)) (:foreground "SteelBlue4"))))
 '(mode-line (
            (((type x) (class color) (min-colors 88) (background dark)) (:background "gray5" :foreground "gray75"  :inverse-video nil :box (:line-width -1 :color "gray30")))
            (((type x) (class color) (min-colors 88) (background light)) (:background "gray70" :foreground "gray20" :inverse-video nil :box (:line-width -1 :color "gray45")))
            (t (:invers-video t))))
 '(mode-line-buffer-id (
            (default (:inherit bold))))
 '(mode-line-emphasis (
            (default (:inherit bold))))
 '(mode-line-highlight (
            (default (:inverse-video t))))
 '(mode-line-inactive (
            (default (:inherit mode-line))
            (((type x) (class color) (min-colors 88) (background dark)) (:background "gray5" :foreground "gray20" :box (:line-width -1 :color "gray10")))
            (((type x) (class color) (min-colors 88) (background light)) (:background "gray40" :foreground "gray80" :box (:line-width -1 :color "gray50")))))
 '(mouse (
            (t nil)))
 '(mumamo-background-chunk-major (
            (((type x) (class color) (min-colors 88) (background dark)) (:background "gray5"))
            (((type x) (class color) (min-colors 88) (background light)) (:background "gray70"))))
 '(mumamo-background-chunk-submode1 (
            (((type x) (class color) (min-colors 88) (background dark)) (:background "gray5"))
            (((type x) (class color) (min-colors 88) (background light)) (:background "gray70"))))
 '(mumamo-background-chunk-submode2 (
            (((type x) (class color) (min-colors 88) (background dark)) (:background "gray5"))
            (((type x) (class color) (min-colors 88) (background light)) (:background "gray70"))))
 '(mumamo-background-chunk-submode3 (
            (((type x) (class color) (min-colors 88) (background dark)) (:background "gray5"))
            (((type x) (class color) (min-colors 88) (background light)) (:background "gray70"))))
 '(mumamo-background-chunk-submode4 (
            (((type x) (class color) (min-colors 88) (background dark)) (:background "gray5"))
            (((type x) (class color) (min-colors 88) (background light)) (:background "gray70"))))
 '(nobreak-space (
            (((type x) (class color) (min-colors 88)) (:inherit escape-glyph :underline t))
            (t (:inverse-video t))))
 '(paren-face-match (
            (default (:inherit show-paren-match))))
 '(paren-face-mismatch (
            (default (:inherit show-paren-mismatch))))
 '(paren-face-no-match (
            (default (:inherit completions-first-difference))))
 '(proced-mark (
            (default (:inherit dired-mark))))
 '(proced-marked (
            (default (:inherit dired-marked))))
 '(proced-sort-header (
            (default (:inherit dired-header))))
 '(region (
            (((type x) (class color) (min-colors 88) (background dark)) (:background "gray20"))
            (((type x) (class color) (min-colors 88) (background light)) (:background "LightGoldenrod2"))))
 '(scroll-bar (
            (default nil)))
 '(secondary-selection (
            (default (:inherit region :inverse-video t))))
 '(show-paren-match (                   ; consider isearch or match for consistency
            ;; (((type x) (class color) (background dark)) (:background "#439EA9" :foreground "#FFFFFF")) ; also nice
            (((type x) (class color) (min-colors 88)) (:background "LightBlue4" :foreground "gray5"))))
 '(show-paren-mismatch (
            (((type x) (class color) (min-colors 88)) (:background "pink" :foreground "PaleVioletRed4"))))
 '(tool-bar (
            (default nil)))
 '(tooltip (
            (default (:inherit variable-pitch))
            (((type x) (class color) (min-colors 88)) (:background "light yellow" :foreground "black"))))
 '(trailing-whitespace (
            (((type x) (class color) (min-colors 88)) (:background "red1"))
            (t (:inverse-video t))))
 '(underline (
            (((supports :underline t)) (:underline t))))
 '(variable-pitch (
            (((supports :family "Sans Serif")) (:family "Sans Serif"))))
 '(vertical-border (
            (((type x) (class color) (min-colors 88)) (:foreground "black"))))
 '(widget-field (
            (default (:inherit italic))
            (((type x) (class color) (min-colors 88) (background dark)) (:background "yellow3" :foreground "black"))
            (((class color grayscale)) (:background "gray85" :foreground "black"))))
'(widget-single-line-field (
            (default (:inherit widget-field)))))
;>>


; ==== Personnal settings ========================================== ;

(defalias 'yes-or-no-p 'y-or-n-p)
(if (functionp 'paren-activate) (paren-activate)) ; may not exist
(defvar explicit-shell-file-name "/bin/bash")
(setq-default filladapt-mode t filladapt-mode-line-string nil)
; (global-undo-tree-mode) ; may require some getting used to
(winner-mode t)

;<< Keybindings >>
(my:include-content "acme/custom-keybindings.el")
(my:define-keys 'global
  ; theme related keybindings
  '("C-c t"   nil)
  '("C-c t d" my:dark-theme)
  '("C-c t l" my:light-theme)
  ; personnal, more or less easy to type keybindings
  '("C-c c"   nil)
  '("C-c c m" menu-bar-mode)
  '("C-c c o" overwrite-mode)
  '("C-c c i" overwrite-mode)
  '("M-g"     goto-line))
(windmove-default-keybindings 'control)
;>>
;<< Themes >>
(defun my:dark-theme (&optional frame)
  "Changes the theme of the frame `frame' to a dark one. If not
specified or nil, apply to current frame."
  (interactive)
  (modify-frame-parameters nil '((background-mode . dark)
                                 (foreground-color . "gray75")
                                 (background-color . "gray5"))))

(defun my:light-theme (&optional frame)
  "Changes the theme of the frame `frame' to a dark one. If not
specified or nil, apply to current frame."
  (interactive)
  (modify-frame-parameters frame '((background-mode . light)
                                   (foreground-color . "#262626")
                                   (background-color . "#EEEEDD"))))
;>>
;<< Compilations >>
(defvar cur nil)
(defvar w nil)
(defvar h nil)
(defun my:compile (&optional arg)
  "Run compile and resize the compile window"
  (interactive)
  (progn
    (if (and arg (stringp arg))
        (compile arg)
      (call-interactively 'compile))
    (setq cur (selected-window))
    (setq w (get-buffer-window "*compilation*"))
    (select-window w)
    (setq h (window-height w))
    (shrink-window (- h 10))
    (select-window cur)))
(defun my-hooks:compilation-mode ()
  "Make sure that the compile window is splitting vertically"
  (if (not (get-buffer-window "*compilation*"))
      (split-window-vertically)))
(defun my:compile-check-delete (buf str)
  (interactive)
  (if (string= str "finished\n")
      (delete-window (get-buffer-window "*compilation*"))))
(add-hook 'compilation-mode-hook 'my-hooks:compilation-mode)
(add-to-list 'compilation-finish-functions 'my:compile-check-delete)
;>>
;<< Delimiters >>
(setq skeleton-pair-filter-function
      '(lambda ()
         (cond
          ((eq last-command-char ?\")
           (or (looking-at   (regexp-quote (string last-command-char)))
               (looking-back (regexp-quote (string last-command-char)))
               (looking-back "[[:graph:]]")))
          (t
           (looking-at (regexp-quote (string last-command-char)))))))
(ignore-errors (tm/initialize))

(when (load "delim-kill" t t)
  (defvar my:delim-kill-pair-alist
    '((?\( _ ?\)) (?[  _ ?])
      (?{  _ ?})  (?<  _ ?>)
      (?«  _ ?»)
      ; single ones
      (?\" _ ?\") (?\' _ ?\')))

  (defun my:delim-kill (start save)
    "Kill text between delimiters based on a table of delimiter pairs."
    (interactive "cDelimiter: \nP")
    (let ((stop (car (cddr (assoc start my:delim-kill-pair-alist)))))
      (if stop
          (delim-kill start stop (point) save))))

  (defun my:delim-mark (&optional start include)
    "Mark text between delimiters based on a table of delimiter pairs."
    (interactive "cDelimiter: \nP")
    (setq this-command 'delim-mark-it)
    (if (not (eq last-command this-command))
        (setq delim-mark-include nil)
      (backward-char)
      (setq delim-mark-include (not delim-mark-include)))
    (cond
     (start
      (setq delim-mark-start start)
      (setq delim-mark-stop (car (cddr (assoc start my:delim-kill-pair-alist))))
      (delim-mark delim-mark-start delim-mark-stop (point) delim-mark-include))
     ((and (eq last-command this-command) delim-mark-start)
      (delim-mark delim-mark-start delim-mark-stop (point) delim-mark-include))
     (t
      (setq delim-mark-start nil)
      (setq delim-mark-stop nil)
      (while (not (assoc (char-after) my:delim-kill-pair-alist))
        (condition-case nil
            (backward-char)
          (error nil)))
      (setq start (char-after))
      (setq stop (car (cddr (assoc (char-after) my:delim-kill-pair-alist))))
      (delim-mark start stop (point) delim-mark-include))))

  (defun delim-mark (from-char to-char orig-point include)
  "Mark the text between two characters, preserving balance.

Marks the text between the first occurence of FROM before point
and the first occurence of TO after point, including FROM and TO
if specified."
  (interactive "cFrom: \ncTo: \nd\nP")
    (let* ((from (delim-find-char-balanced-backward from-char to-char))
           (to (delim-find-char-balanced-forward  from-char to-char)))
      (if (and from to)
          (progn
            (delim-mark-it from to include))
        (message "Not found!"))))

  (defun delim-mark-it (from to include)
    (message "%s" (buffer-substring from to))
    (let ((from (if include from (1+ from)))
          (to (if include to (1- to))))
      (goto-char to)
      (push-mark-command nil)
      (goto-char from)))

  (my:define-keys 'global
    '("M-k" my:delim-kill)
    '("C-c d" my:delim-kill)
    '("C-c m" nil)
    '("C-c m \(" (lambda nil (interactive) (setq this-command 'delim-mark-it) (my:delim-mark ?\()))
    '("C-c m {" (lambda nil (interactive) (setq this-command 'delim-mark-it) (my:delim-mark ?{)))
    '("C-c m [" (lambda nil (interactive) (setq this-command 'delim-mark-it) (my:delim-mark ?[)))
    '("C-c m <" (lambda nil (interactive) (setq this-command 'delim-mark-it) (my:delim-mark ?<)))
    '("C-c m \"" (lambda nil (interactive) (setq this-command 'delim-mark-it) (my:delim-mark ?\")))
    '("C-c m '" (lambda nil (interactive) (setq this-command 'delim-mark-it) (my:delim-mark ?\')))
    '("C-c m m" my:delim-mark nil))) ; try to be smart
;>>
;<< Modes >>
;-- ConTeXt --
(put 'ConTeXt-modes-list 'safe-local-variable 'sequencep)
(put 'ConTeXt-verbose 'safe-local-variable 'booleanp)
(put 'ConTeXt-use-beta 'safe-local-variable 'booleanp)
(put 'ConTeXt-use-makefile 'safe-local-variable 'booleanp)
(put 'TeX-PDF-mode 'safe-local-variable 'booleanp)
(put 'TeX-master 'safe-local-variable 'stringp)
(add-to-list 'auto-mode-alist '("\\.mkiv\\'" . context-mode))
(add-to-list 'auto-mode-alist '("\\.mkii\\'" . context-mode))
(autoload 'context-mode "custom-context")
;>>


;; Local Variables:
;; hide-local-variable-section: t
;; eval:(set-regexp-face "^\s*;+ ?=+\\( [ [:graph:]]+\\)?\\( =+ ?;*\\)?$" 'VioletRed4-bold-italic)
;; eval:(set-regexp-face "^\s*;+{* \\*+ [ [:graph:]]+ \\*+ ?}*$" 'Green4-bold-italic)
;; eval:(set-regexp-face "^\s*;+ ?\\[+ [ [:graph:]]+ \\]+$" 'h00688b-bold)
;; eval:(set-regexp-face "^\s*;+ ?<+ [ [:graph:]]+ >+$" 'h00688b-bold)
;; eval:(folding-mode t)
;; eval:(rainbow-mode t)
;; End:
