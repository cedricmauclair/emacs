; Time-stamp: <2010-11-25 14:52:55 cmauclai>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Copyright (c) 2010, Cédric Mauclair.
;;
;; Licenced under GPL v3.0 and after.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;<< —— server    —————————————————————————————————————————————————— >>

(load "server" t t)

(when (and (functionp 'server-running-p)
   (not (server-running-p)))
  (server-start))

;>> server (end) —————————————————————————————————————————————————— >>
;<< —— load-path —————————————————————————————————————————————————— >>

(defconst emacs-root
  (expand-file-name "emacs/" "~")
  "The root directory of your personnal emacs tree.")

(defconst lisp-root
  (concat emacs-root "lisp/")
  "Root directory of your lisp tree.")

(defconst more-lisp-dirs
  (list "auctex" "etexshow")
  "List of directories to add to the load path, relative to
    `lisp-root' or absolute.")


(add-to-list 'load-path lisp-root)

(dolist (dir more-lisp-dirs)
  (add-to-list 'load-path
               (if (file-name-absolute-p dir) dir
                 (concat lisp-root dir))))

;>> load-path (end) ——————————————————————————————————————————————— >>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;<< —— before-save-hook & write-file-hooks ———————————————————————— >>

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'write-file-hooks 'time-stamp)

;>> before-save-hook & write-file-hooks (end) ————————————————————— >>
;<< —— enable/disable some commands        ———————————————————————— >>

(put 'dired-find-alternate-file 'disabled nil)

;>> enable/disable some commands (end) ———————————————————————————— >>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;<< —— custom-set-variable ———————————————————————————————————————— >>

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
 '(dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*")
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
 '(font-latex-match-function-keywords (quote (("startenumerate" "") ("stopenumerate" "") ("framed" "[{") ("doifmode" "{{") ("doifmodeelse" "{{{") ("doifnotmode" "{{") ("doiftext" "{{") ("doiftextelse" "{{{") ("definepagebreak" "[[") ("definefontfeature" "[[[") ("startsectionblockenvironment" "[") ("stopsectionblockenvironment" "") ("startsetups" "") ("stopsetups" "") ("setups" "[{") ("setlayer" "[[{") ("setlayerframed" "[[[{") ("definelayer" "[[") ("defineselector" "[[") ("setupselector" "[[") ("definepagebreack" "[[") ("placeheadtext" "[") ("placerawheadtext" "[") ("placeheadnumber" "[") ("placerawheadnumber" "[") ("definestructureconversionset" "[[[") ("setupuserpagenumber" "[") ("paperwidth" "") ("paperheight" "") ("placetable" "[[{") ("usetypescript" "[") ("definetypeface" "[[[[[[") ("starttypescript" "[") ("stoptypescript" "") ("blank" "[") ("setupitemize" "[[") ("completecontent" "[") ("placecontent" "[") ("placebookmarks" "[[") ("placecombinedlist" "[[") ("placefigure" "[[{") ("externalfigure" "[[") ("placefootnotes" "[") ("placeformula" "[") ("placelegend" "{{") ("placelist" "[[") ("placelistoffloats" "") ("placelistofsorts" "") ("placelistofsynonyms" "") ("placelocalfootnotes" "[") ("placelogos" "[") ("placeongrid" "[{") ("placeontopofeachother" "{{") ("placereferencelist" "[") ("placeregister" "[[") ("placerule" "[") ("placesidebyside" "{{") ("placesubformula" "[") ("placetextvariable" "[") ("startalignment" "[[") ("startbackground" "[[") ("startbuffer" "[[") ("startcolor" "[[") ("startcolumns" "[[") ("startcombination" "[[") ("startcomment" "[[") ("startcomponent" "[[") ("startdescription" "[[") ("startdocument" "[[") ("startenumeration" "[[") ("startenvironment" "[[") ("startfact" "[[") ("startfigure" "[[") ("startfloattext" "[[") ("startformula" "[[") ("startframedtext" "[[") ("starthiding" "[[") ("startinteractionmenu" "[[") ("startitemize" "[") ("starttextitemize" "[") ("stoptextitemize" "[") ("startlegend" "[[") ("startline" "[[") ("startlinecorrection" "[[") ("startlinenumbering" "[[") ("startlines" "[[") ("startlocal" "[[") ("startlocalenvironment" "[[") ("startlocalfootnotes" "[[") ("startmakeup" "[[") ("startmarginblock" "[[") ("startmarginrule" "[[") ("startnamemakeup" "[[") ("startnarrower" "[[") ("startopposite" "[[") ("startoverlay" "[[") ("startoverview" "[[") ("startpacked" "[[") ("startparagraph" "[[") ("startpositioning" "[[") ("startpostponing" "[[") ("startproduct" "[[") ("startprofile" "[[") ("startproject" "[[") ("startquotation" "[[") ("startregister" "[[") ("startsymbolset" "[[") ("startsynchronization" "[[") ("starttable" "[[") ("starttables" "[[") ("starttabulate" "[[") ("starttextrule" "[[") ("starttyping" "[[") ("startunpacked" "[[") ("startversion" "[[") ("stopalignment" "") ("stopbackground" "") ("stopbuffer" "") ("stopcolor" "") ("stopcolumns" "") ("stopcombination" "") ("stopcomment" "") ("stopcomponent" "") ("stopdescription" "") ("stopdocument" "") ("stopenumeration" "") ("stopenvironment" "") ("stopfact" "") ("stopfigure" "") ("stopfloattext" "") ("stopformula" "") ("stopframedtext" "") ("stophiding" "") ("stopinteractionmenu" "") ("stopitemize" "[") ("stoplegend" "") ("stopline" "") ("stoplinecorrection" "") ("stoplinenumbering" "") ("stoplines" "") ("stoplocal" "") ("stoplocalenvironment" "") ("stoplocalfootnotes" "") ("stopmakeup" "") ("stopmarginblock" "") ("stopmarginrule" "") ("stopnamemakeup" "") ("stopnarrower" "") ("stopopposite" "") ("stopoverlay" "") ("stopoverview" "") ("stoppacked" "") ("stopparagraph" "") ("stoppositioning" "") ("stoppostponing" "") ("stopproduct" "") ("stopprofile" "") ("stopproject" "") ("stopquotation" "") ("stopregister" "") ("stopsymbolset" "") ("stopsynchronization" "") ("stoptable" "") ("stoptables" "") ("stoptabulate" "") ("stoptextrule" "") ("stoptyping" "") ("stopunpacked" "") ("stopversion" "") ("define" "[[") ("defineblank" "[[") ("defineblock" "[[") ("definebodyfont" "[[") ("definebodyfontenvironment" "[[") ("definebuffer" "[[") ("definecolor" "[[") ("definecolorgroup" "[[") ("definecombinedlist" "[[[") ("defineconversion" "[[") ("definedescription" "[[") ("defineenumeration" "[[[") ("definefield" "[[") ("definefieldstack" "[[") ("definefiguresymbol" "[[") ("definefloat" "[[") ("definefont" "[[") ("defineframed" "[[") ("defineframedtext" "[[") ("definehead" "[[") ("defineindenting" "[[") ("defineinteractionmenu" "[[") ("defineinteractionmenu (2)" "[[") ("definelabel" "[[") ("definelist" "[[[") ("definelogo" "[[") ("definemakeup" "[[") ("definemarking" "[[") ("defineoutput" "[[") ("defineoverlay" "[[") ("definepalet" "[[") ("definepapersize" "[[") ("defineparagraphs" "[[") ("defineprofile" "[[") ("defineprogram" "[[") ("definerawfont" "[[") ("definereference" "[[") ("definereferenceformat" "[[") ("definereferencelist" "[[") ("defineregister" "[[") ("definerule" "[[") ("definesection" "[[") ("definesectionblock" "[[") ("definesorting" "[[") ("definestartstop" "[[") ("definesubfield" "[[") ("definesymbol" "[[") ("definesynonyms" "[[[") ("definetabletemplate" "[[") ("definetabulate" "[[") ("definetext" "[[") ("definetextposition" "[[") ("definetextvariable" "[[") ("definetype" "[[") ("definetyping" "[[") ("defineversion" "[[") ("setuppapersize" "[[") ("setuplayout" "[") ("setupenumerations" "[[") ("setupalign" "[[") ("setuparranging" "[[") ("setupbackground" "[[") ("setupbackgrounds" "[[") ("setupblackrules" "[[") ("setupblank" "[[") ("setupblock" "[[") ("setupbodyfont" "[[") ("setupbodyfontenvironment" "[[") ("setupbottom" "[[") ("setupbottomtexts" "[[[") ("setupbuffer" "[[") ("setupbuttons" "[[") ("setupcapitals" "[[") ("setupcaption" "[[") ("setupcaptions" "[[") ("setupclipping" "[[") ("setupcolor" "[[") ("setupcolors" "[[") ("setupcolumns" "[[") ("setupcombinations" "[[") ("setupcombinedlist" "[[") ("setupcomment" "[[") ("setupdescriptions" "[[") ("setupenumerations" "[[") ("setupexternalfigures" "[[") ("setupfield" "[[") ("setupfields" "[[") ("setupfillinlines" "[[") ("setupfillinrules" "[[") ("setupfloat" "[[") ("setupfloats" "[[") ("setupfloatsplitting" "[[") ("setupfooter" "[[") ("setupfootertexts" "[[[[") ("setupfootnotedefinition" "[[") ("setupfootnotes" "[[") ("setupforms" "[[") ("setupformulae" "[[") ("setupframed" "[[") ("setupframedtexts" "[[") ("setuphead" "[[") ("setupheader" "[[") ("setupheadertexts" "[[[[") ("setupheadnumber" "[[") ("setupheads" "[[") ("setupheadtext" "[[") ("setuphyphenmark" "[[") ("setupindentations" "[[") ("setupindenting" "[[") ("setupinmargin" "[[") ("setupinteraction" "[[") ("setupinteractionbar" "[[") ("setupinteractionscreen" "[[") ("setupinterlinespace" "[[") ("setupitemgroup" "[[") ("setupitems" "[[") ("setuplabeltext" "[[") ("setuplanguage" "[[") ("setuplayout" "[[") ("setuplegend" "[[") ("setuplinenumbering" "[[") ("setuplines" "[[") ("setuplinewidth" "[[") ("setuplist" "[[") ("setupmakeup" "[[") ("setupmarginblocks" "[[") ("setupmarginrules" "[[") ("setupmarking" "[[") ("setupnarrower" "[[") ("setupnumbering" "[[") ("setupoppositeplacing" "[[") ("setupoutput" "[[") ("setuppagenumber" "[[") ("setuppagenumbering" "[[") ("setuppagetransitions" "[[") ("setuppalet" "[[") ("setuppaper" "[[") ("setuppapersize" "[[") ("setupparagraphnumbering" "[[") ("setupparagraphs" "[[") ("setuppositioning" "[[") ("setupprofiles" "[[") ("setupprograms" "[[") ("setuppublications" "[[") ("setupquote" "[[") ("setupreferencelist" "[[") ("setupreferencing" "[[") ("setupregister" "[[") ("setuprotate" "[[") ("setuprule" "[[") ("setupscreens" "[[") ("setupsection" "[[") ("setupsectionblock" "[[") ("setupsorting" "[[") ("setupspacing" "[[") ("setupstrut" "[[") ("setupsubpagenumber" "[[") ("setupsymbolset" "[[") ("setupsynchronization" "[[") ("setupsynchronizationbar" "[[") ("setupsynonyms" "[[") ("setupsystem" "[[") ("setuptab" "[[") ("setuptables" "[[") ("setuptabulate" "[[") ("setuptext" "[[") ("setuptextposition" "[[") ("setuptextrules" "[[") ("setuptexttexts" "[[") ("setuptextvariable" "[[") ("setupthinrules" "[[") ("setuptolerance" "[[") ("setuptop" "[[") ("setuptoptexts" "[[[") ("setuptype" "[[") ("setuptyping" "[[") ("setupunderbar" "[[") ("setupurl" "[[") ("setupversions" "[[") ("setupwhitespace" "[[") ("showsetups" "[[") ("Use" "{[{"))))
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
 '(safe-local-variable-values (quote ((TeX-source-correlate-method . source-specials) (TeX-source-correlate-method-active . source-specials))))
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

;>> custom-set-variable (end) ————————————————————————————————————— >>
;<< —— custom-set-faces    ———————————————————————————————————————— >>

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
   (((type x) (class color) (min-colors 88) (background dark)) (:foreground "#89E14B"))
   (((type x) (class color) (min-colors 88) (background light)) (:foreground "green4"))))
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

;>> custom-set-faces —————————————————————————————————————————————— >>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;<< —— (re)set some variables ————————————————————————————————————— >>

(defalias 'yes-or-no-p 'y-or-n-p)
(defcustom explicit-shell-file-name "/bin/bash" ""
  :type 'string
  :group 'acme)
(setq-default filladapt-mode t filladapt-mode-line-string nil)

;>> (re)set some variables (end) —————————————————————————————————— >>
;<< —— enable some goodies    ————————————————————————————————————— >>

(when (require 'mic-paren nil t) (paren-activate))
(when (require 'winner nil t)    (winner-mode t))

;>> enable some goodies (end) ————————————————————————————————————— >>
;<< —— load some packages     ————————————————————————————————————— >>

(defcustom packages-to-load
  '("dired-x" "drag-stuff" "fic-mode" "filladapt" "gnuplot"
    "gnuplot-gui" "rainbow-mode" "tex-site" "textmate")
  "List of packages to load automatically."
  :type '(repeat string)
  :group 'acme)

(dolist (package packages-to-load)
  (require (intern package) nil t))

;>> load some packages (end) —————————————————————————————————————— >>

;<< —— info-mode       ———————————————————————————————————————————— >>

(add-hook
 'Info-mode-hook
 (lambda nil
   (local-set-key (kbd "<backtab>") nil)
   (local-set-key (kbd "M-s")       nil)))

;>> info-mode (end) ——————————————————————————————————————————————— >>
;<< —— help-mode       ———————————————————————————————————————————— >>

(add-hook
 'help-mode-hook
 (lambda nil
   (local-set-key (kbd "<backtab>") nil)
   (local-set-key (kbd "M-p")       'help-go-back)
   (local-set-key (kbd "M-n")       'help-go-forward)))

;>> help-mode (end) ——————————————————————————————————————————————— >>
;<< —— dired-mode      ———————————————————————————————————————————— >>

(add-hook 'dired-mode-hook 'dired-omit-mode)

;>> dired-mode (end) —————————————————————————————————————————————— >>
;<< —— folding-mode    ———————————————————————————————————————————— >>

(when (require 'folding nil t)
  (defcustom add-folding-mode-to
    '(("emacs-lisp" ";<<"  ";>>")
      ("lua"        "--<<" "-->>")
      ("latex"      "%<<"  "%>>"))
    "List of modes where to activate `folding-mode' automatically."
    :type '(repeat (repeat string))
    :group 'acme)

  (dolist (mode add-folding-mode-to)
    (let ((beg (nth 1 mode))
          (end (nth 2 mode))
          (hook (intern (concat (nth 0 mode) "-mode-hook")))
          (mode (intern (concat (nth 0 mode) "-mode"))))
      (add-hook hook 'folding-mode)
      (folding-add-to-marks-list mode beg end)))

  (add-hook
   'folding-mode-hook
   (lambda nil
     (local-set-key (kbd "M-g")     'folding-goto-line)
     (local-set-key (kbd "C-c f")    nil) ; make it a prefix
     (local-set-key (kbd "M-RET")   'folding-toggle-show-hide)
     (local-set-key (kbd "M-ESC")   'folding-toggle-show-hide)
     (local-set-key (kbd "C-c f s") 'folding-show-current-subtree)
     (local-set-key (kbd "C-c f h") 'folding-hide-current-subtree)
     (local-set-key (kbd "C-c f o") 'folding-open-buffer)
     (local-set-key (kbd "C-c f c") 'folding-whole-buffer))))

;>> folding-mode (end) ———————————————————————————————————————————— >>
;<< —— emacs-lisp-mode ———————————————————————————————————————————— >>

(add-hook
 'emacs-lisp-mode-hook
 (lambda nil
   (local-set-key (kbd "<M-f8>") 'my:byte-compile-this-file)))

;>> emacs-lisp-mode (end) ————————————————————————————————————————— >>
;<< —— lua-mode        ———————————————————————————————————————————— >>

(when (require 'lua-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.mlua\\'" . lua-mode))

  (add-hook
   'lua-mode-hook
   (lambda nil
     (local-set-key (kbd " ") (lambda nil (interactive) (insert " ")))
     (local-set-key (kbd "C-c C-r") 'lua-restart-with-whole-file))))

;>> lua-mode (end) ———————————————————————————————————————————————— >>

;<< —— latex-mode      ———————————————————————————————————————————— >>

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
(require 'cdlatex nil t)

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
                           :foreground "OliveDrab"))

(add-hook 'bibtex-mode-hook 'BibTeX-auto-store)
(add-hook 'LaTeX-mode-hook 'my:LaTeX-mode-hook)

(put 'TeX-master 'safe-local-variable 'stringp)

;>> latex-mode ———————————————————————————————————————————————————— >>
;<< —— context-mode    ———————————————————————————————————————————— >>

(defun my:tex-unbreakable-space ()
  "Insère '~' pour inserer une espace insécable sous (La|Con)TeX(|t)."
  (interactive)
  (insert "~"))

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
  `(defun ,(intern (concat "my:enclose-in-" name)) (beg end)
     (interactive "r")
     (if (not (region-active-p))
         (progn (setq beg (point))
                (setq end (point))))
     (goto-char end)
     (insert ,strend)
     (goto-char beg)
     (insert ,strbeg)))

(defvar etexshow-xml-files-alist nil)
(when (require 'etexshow nil t)
  (setq etexshow-xml-files-alist
        '(("~/emacs/lisp/etexshow/cont-en.xml" . "~/emacs/lisp/etexshow/cont-en.cache")
          ("~/emacs/lisp/etexshow/mycommands.xml" . "~/emacs/lisp/etexshow/mycommands.cache")))
  (setq etexshow-comment-file "~/emacs/lisp/etexshow/cont-en-comments.xml" )
  (add-hook 'etexshow-mode-hook
            '(lambda () (local-set-key (kbd "<f7>") 'etexshow-quit))))

(my-context:insert "doubt"   "\\doute{" "}")
(my-context:insert "english" "{\\english " "}")
(my-context:insert "quote"   "« " " »")
(my-context:insert "inline"  "|<|" "|>|")

(defun my:context-view-memoir ()
  (interactive)
  (shell-command "pdfopen -viewer xpdf ~/memoir/phd-thesis-memoir.pdf"))

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
                  '("C-c C-v" my:context-view-memoir)
                                        ; insert strings in the buffer
                  '("C-c i"   nil)
                  '("C-c i d" my:enclose-in-doubt)
                  '("C-c i e" my:enclose-in-english)
                  '("C-c i q" my:enclose-in-quote)
                  '("C-c i i" my:enclose-in-inline)
                  '(" "       my:tex-unbreakable-space)
                                        ; some help for the commands
                  '("<f7>"    etexshow-cmd)
                  '("<S-f7>"  etexshow)
                                        ; other
                  '("M-q"     LaTeX-fill-paragraph))

  (setq TeX-engine 'luatex)
  (setq TeX-command-default "ConTeXt Full")
  (auto-fill-mode t)
  (folding-mode t)
  (setq fill-column 80))

(defun ConTeXt-switch-makefile-AUCTeX ()
  (interactive)
  (if (not ConTeXt-use-makefile)
      (local-set-key (kbd "C-c C-c")
                     (lambda ()
                       (interactive) (save-buffer) (my:compile "make -k")))
    (local-set-key (kbd "C-c C-c") 'TeX-command-master))
  (setq ConTeXt-use-makefile (not ConTeXt-use-makefile)))

(put 'ConTeXt-modes-list 'safe-local-variable 'listp)
(put 'ConTeXt-verbose 'safe-local-variable 'booleanp)
(put 'ConTeXt-use-beta 'safe-local-variable 'booleanp)

(folding-add-to-marks-list 'context-mode "%<<" "%>>")
(add-hook 'ConTeXt-mode-hook 'my-hooks:context-mode)

;>> context-mode —————————————————————————————————————————————————— >>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;<< —— usefull functions     —————————————————————————————————————— >>
;<< ———— utility functions ———————————————————————————————————————— >>

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

;>> utility functions (end) ——————————————————————————————————————— >>
;<< ———— colors  functions ———————————————————————————————————————— >>

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

;>> colors functions (end) ———————————————————————————————————————— >>
;<< ———— keybdgs functions   —————————————————————————————————————— >>

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

;>> helper functions (end) ———————————————————————————————————————— >>
;<< ———— buffers functions ———————————————————————————————————————— >>

(defalias 'list-buffers 'electric-buffer-list)

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

;>> manipulating buffers (end) ———————————————————————————————————— >>
;<< ———— editing functions ———————————————————————————————————————— >>

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

;>> editing functions (end) ——————————————————————————————————————— >>
;<< ———— macros  functions ———————————————————————————————————————— >>
(defvar macro-key-record-key nil
  "When a macro-key is recorded this is the key vector, null otherwise.")
(defvar macro-key-record-keymap nil
  "When a macro-key is recorded this is the keymap it should be defined in.")
(defvar macro-key-record-name nil
  "When a macro-key is recorded, this is the key name.")
(defvar macro-key-record-mode nil
  "When a macro-key is recorded, this is 'global, 'local, or 'append.")
(defvar macro-key-saved-binding nil
  "When a macro-key is being recorded, this holds its previous binding.")
(defvar macro-key-saved-last-kbd-macro nil
  "When a macro-key is being recorded, this saves `last-kbd-macro's value.")
(defvar macro-key-last-key+mode nil
  "Last (key . mode) that we attempted to make a macro key.")

(defun macro-key-start-recording (key mode)
  ;; mode is 'global, 'local, or 'append
  (let* ((keyname (key-description key))
         (keymap  (cond ((eq mode 'local) (current-local-map))
                        ((eq mode 'global) (current-global-map))
                        (t nil)))
         (binding (if keymap (lookup-key keymap key) (key-binding key))))
    (unless (or (not binding)
                (arrayp binding) ; already a macro
                (eq binding 'self-recording-macro-key)
                (and (eq this-command last-command)
                     (equal (cons key mode) macro-key-last-key+mode)))
      (setq macro-key-last-key+mode (cons key mode))
      (error "`%s' is bound to `%s', repeat to rebind." keyname binding))
    (when (and (eq mode 'append) (not (arrayp binding)))
      (error "`%s' is not bound to a macro." keyname))
    (unless keymap
      (setq keymap
            (cond ((lookup-key (current-local-map)  key) (current-local-map))
                  ((lookup-key (current-global-map) key) (current-global-map))
                  (t (error "Could not find the binding of `%s'," keyname)))))
    (setq macro-key-last-key+mode        nil
          macro-key-record-key           key
          macro-key-record-name          keyname
          macro-key-record-keymap        keymap
          macro-key-record-mode          mode
          macro-key-saved-binding        binding
          macro-key-saved-last-kbd-macro last-kbd-macro
          last-kbd-macro                 (if (eq mode 'append) binding nil))
    (define-key keymap key 'end-kbd-macro)
    (add-hook 'post-command-hook 'macro-key-post-command)
    (if (eq mode 'append) (start-kbd-macro t t) (start-kbd-macro nil))
    (message "%s macro key `%s'..."
             (cond ((eq mode 'local)  "Recording local")
                   ((eq mode 'global) "Recording global")
                   ((eq mode 'append) "Appending to"))
             keyname)))

(defun macro-key-post-command ()
  (unless defining-kbd-macro (macro-key-done-recording)))

(defun macro-key-done-recording ()
  (remove-hook 'post-command-hook 'macro-key-post-command)
  (let ((errorp (not (and last-kbd-macro macro-key-record-key)))
        (name macro-key-record-name))
    ;; either bind to new macro, or reset to saved value
    (define-key macro-key-record-keymap macro-key-record-key
      (or last-kbd-macro macro-key-saved-binding))
    ;; restore state
    (setq macro-key-record-key nil
          last-kbd-macro       macro-key-saved-last-kbd-macro)
    (if errorp
      (progn (sit-for 1)
             (message "Macro %s aborted!" name)
             (beep)
             (sleep-for 0.2))
      (message
       (cond ((eq macro-key-record-mode 'local) "Local macro key `%s' recorded.")
             ((eq macro-key-record-mode 'global) "Macro key `%s' recorded.")
             ((eq macro-key-record-mode 'append) "Appended to `%s' macro key."))
       name))))

(defun macro-key-get-key+macro (for-what)
  (let* ((key  (my:get-key (format "Keyboard macro to %s: " for-what)))
         (lval (local-key-binding key))
         (gval (and (not lval) (global-key-binding key))))
    (unless (arrayp (or lval gval))
      (error "`%s' is not bound to a keyboard macro" (key-description key)))
    (cons key
          (if lval (cons 'local-set-key lval) (cons 'global-set-key gval)))))

(autoload 'edmacro-format-keys "edmacro")

(defun macro-key-save-macro (key mac)
  (let ((buf (get-buffer-create "*Macro*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert ";; Grab this definition and put it somewhere like \"~/.emacs\"\n")
      (insert (format "(global-set-key (kbd %S) (read-kbd-macro %S))"
                      (edmacro-format-keys key)
                      (concat "\n " (format-kbd-macro mac)))))
    (display-buffer buf)))

(defun macro-key-menu ()
  (let* ((op (my:get-key "[a]ppend [e]dit [d]ebug [s]ave "))
         (op (cond ((equal [?a] op) 'append)
                   ((equal [?e] op) 'edit)
                   ((equal [?d] op) 'debug)
                   ((equal [?s] op) 'save)
                   (t (error "Unknown choice"))))
         (x   (macro-key-get-key+macro op))
         (key (car x))
         (x   (cdr x))
         (set (car x))
         (mac (cdr x)))
    (require 'kmacro)
    (require 'edmacro)
    (cond ((equal op 'append) (macro-key-start-recording key 'append))
          ((equal op 'save) (macro-key-save-macro key mac))
          ((equal op 'edit)
           (setq last-kbd-macro mac)
           (edit-kbd-macro 'call-last-kbd-macro current-prefix-arg
                           `(lambda () (,set ,key last-kbd-macro))))
          ((equal op 'debug)
           (let ((last-kbd-macro mac))
             (call-interactively 'kmacro-step-edit-macro)
             (funcall set key last-kbd-macro)))
          (t (error "Unknown choice")))))

(defun self-recording-macro-key ()
  "Start a macro key recording for the key that invoked this.
Stop recording if this key is being recorded.
The macro key that gets recorded is always global."
  (interactive)
  (macro-key-start-recording (this-single-command-keys) current-prefix-arg))

(defun macro-key ()
  "Main entry point for recording and managing macro keys."
  (interactive)
  (cond (defining-kbd-macro (end-kbd-macro))
        (executing-kbd-macro (error "Cannot use `macro-key' from macros."))
        (t (let* ((localp current-prefix-arg)
                  (key (my:get-key (format "%s to record (repeat for options): "
                                            (if localp "Local key" "Key")))))
             (if (eq this-command (key-binding key))
               (macro-key-menu)
               (macro-key-start-recording key (if localp 'local 'global)))))))

(add-hook 'emacs-startup-hook ; add a mode-line indicator for recorded key
          (lambda ()
            (let ((indicator
                   '(macro-key-record-name (" Def:" macro-key-record-name)
                                           " Def"))
                  (x (assq 'defining-kbd-macro minor-mode-alist)))
              (setcar (cdr x) indicator)))
          t)

;>> macros functions (end) ———————————————————————————————————————— >>
;<< ———— markers functions ———————————————————————————————————————— >>

(defvar my:markers (make-hash-table :test 'equal))

(defun my:marker-get-key (prompt)
  (let ((key (my:get-key prompt t))) (and key (key-description key))))

(defun my:marker-set-frameconf ()
  (let ((key nil))
    (while (not key)
      (setq key (my:marker-get-key "Key to save marker+windows+frames: ")))
    (message "Setting marker key `%s' to this position+windows+frames." key)
    (puthash key (list (current-frame-configuration)) my:markers)
    key))

(defun my:marker-set-winconf ()
  (let ((key (my:marker-get-key
              "Key to save marker+windows (again to save frames): ")))
    (if (not key)
        (setq key (my:marker-set-frameconf))
      (progn (puthash key nil my:markers)
             (message "Setting marker key `%s' to this position+windows." key)))
    (puthash key
             (nconc (gethash key my:markers '())
                    (list (current-window-configuration)))
             my:markers)
    key))

(defun my:marker-set-marker ()
  (let ((key (my:marker-get-key
              "Key to save marker (again to save windows): ")))
    (if (not key)
        (setq key (my:marker-set-winconf))
      (progn (puthash key nil my:markers)
             (message "Setting marker key `%s' to this position." key)))
    (puthash key
             (nconc (gethash key my:markers '()) (list (point-marker)))
             my:markers)))

(defun my:marker-copy (beg end)
  (interactive "r")
  (let ((key (my:marker-get-key "Key to save region to: ")))
    (message "Setting marker key `%s' to this text." key)
    (puthash key (buffer-substring beg end) my:markers)))

(defun my:marker ()
  "Jump to a marker, possibly restoring window and/or frame configuration;
or set such a marker."
  (interactive)
  (let ((key (my:marker-get-key
              "Marker to jump to (repeat to set a marker): ")))
    (if (not key)
        (my:marker-set-marker)
      (let ((data (gethash key my:markers)) (more nil))
        (unless data (error "No `%s' marker set." key))
        (if (stringp data)
            (insert data)
          (dolist (d data)
            (cond ((markerp d)
                   (or (marker-buffer d)
                       (error "The `%s' marker's buffer no longer exists." key))
                   (switch-to-buffer (marker-buffer d))
                   (push-mark)
                   (goto-char d))
                  ((window-configuration-p d)
                   (set-window-configuration d)
                   (unless more (setq more " (restored windows)")))
                  ((frame-configuration-p d)
                   (set-frame-configuration d)
                   (setq more " (restored windows/frames)"))
                  (t (error "Bad data in `%s' marker: %S." key d)))) ())
        (message "Jumped to `%s'%s." key (or more ""))))))

;>> markers functions (end) ——————————————————————————————————————— >>
;>> usefull functions (end) ——————————————————————————————————————— >>
;<< —— general keybindings   —————————————————————————————————————— >>

(my:define-keys 'global
  ;; ** Movements keys **
  '("<next>"  my:scroll-up)     ; scroll in place
  '("C-v"     my:scroll-up)     ; scroll in place
  '("<prior>" my:scroll-down)   ; scroll in place
  '("M-v"     my:scroll-down)   ; scroll in place
  '("M-p"     previous-line)      ; tired to go from "M-f/b" to "C-p"
  '("M-n"     next-line)          ; tired to go from "M-f/b" to "C-n" (my:next-line ?)
  '("M-g"     goto-line)

  ;; ** Marks/kills **
  '("M-@"   cua-set-mark)    ; was `mark-word', moved to "M-+"
  '("M-+"   mark-word)       ; moved from "M-@"
  '("M-W"   kill-region)     ; tired to go from "M-f/b" to "C-k"
  '("C-S-w" kill-ring-save)  ; tired to go from "C-p/n/b/f" to "M-w"

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
  '("M-Z" zap-to-char)

  ;; ** Mouse **
  '("<S-down-mouse-1>" nil)
  '("<S-mouse-1>"      mouse-save-then-kill)
  '("<mouse-3>"        nil)
  '("<down-mouse-3>"   mouse-major-mode-menu)
  '("<S-mouse-3>"      nil)
  '("<S-down-mouse-3>" mouse-set-font)

  ;; ** Function keys **
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
  '("<S-f12>"  widen))

(my:define-keys
  ;; ** Help stuff **
  help-map
  '("C-q"                       my:emacs-quickref)

  ;; ** isearch **
  'global
  '([(meta ?s)]                 isearch-forward-regexp)
  '([(meta ?r)]                 isearch-backward-regexp)

  read-expression-map
  ;; tab completes a symbol when editing a minibuffer lisp expression
  '([(tab)]                     lisp-complete-symbol)

  isearch-mode-map
  ;; ** Yanking **
  '([(control ?w)]              isearch-yank-word)
  '([(control ?f)]              isearch-yank-char)
  '([(control ?k)]              isearch-yank-line)
  '([(control ?y)]              isearch-yank-kill)
  '([(control ?t)]              isearch-toggle-case-fold)
  ;; fix backspace & delete to behave the same in isearch mode
  '([(?\C-?)]                   isearch-delete-char)
  '([(delete)]                  isearch-delete-char)
  '("C-M-r"                     recursive-edit)) ; was `isearch-repeat-backward', moved to "M-r"

;>> general keybindings ——————————————————————————————————————————— >>
;<< —— personnal keybindings —————————————————————————————————————— >>

; personnal keybindings
(my:define-keys 'global
  '("C-c t"   nil) ; make it a prefix
  '("C-c t m" menu-bar-mode)
  '("C-c t i" overwrite-mode)
  '("C-c t d" my:dark-theme)
  '("C-c t l" my:light-theme))

(global-set-key
 (kbd "<backtab>") (read-kbd-macro "C-x C-\\340 RET"))

(windmove-default-keybindings    'control)

;>> personnal keybindings (end) ——————————————————————————————————— >>
;<< —— themes                —————————————————————————————————————— >>

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

;>> themes (end) —————————————————————————————————————————————————— >>
;<< —— compilation buffer    —————————————————————————————————————— >>

(defvar cur nil) (defvar w nil) (defvar h nil)

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

;>> compilation buffer (end) —————————————————————————————————————— >>
;<< —— delimiters            —————————————————————————————————————— >>

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

;>> delimiters (end) —————————————————————————————————————————————— >>
