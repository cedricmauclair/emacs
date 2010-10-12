;;; verbiste.el --- word conjugation

;;; Commentary:

;; there is another verbiste.el out there, this one gives a prettier
;; output

;;; THANKS:

;;; BUGS:

;;; INSTALLATION:

;;; Code:

(defvar verbiste-buffer "*verbiste*"
  "Name of the buffer where conjugated verbs will be displayed")

;;;###autoload
(defun verbiste ()
  "Display conjugation of verb at point"
  (interactive)
  (let* ((verb (read-string "Conjugaison du verbe : "
                            (current-word)
                            nil
                            (current-word)))
         (split (verbiste-split verb))
         string)
    (if split
        (verbiste-display split)
      (setq string (shell-command-to-string
                    (concat "french-deconjugator " verb)))
      (if (string= string "\n")
          (message "Pas de conjugaison pour %s." verb)
        (setq split (verbiste-split (car (split-string string ","))))
        (verbiste-display split)))))

(defun verbiste-split (word)
  "Return a list of conjugations for WORD"
  (let (string ret line)
    (with-temp-buffer
      (call-process "french-conjugator" nil t nil word)
      (goto-char (point-min))
      (while (not (eobp))
        (setq line (buffer-substring
                    (line-beginning-position) (line-end-position)))
        (if (not (or (string= line "")
                     (string= (substring line 0 1) "-")))
            (setq string (append string (list line)))
          (when (not (bobp))
            (setq ret (append ret (list string)))
            (setq string nil)))
        (forward-line 1)))
    ret))

(defun verbiste-display (split)
  "Display SPLIT into a buffer"
  (let ((buf (get-buffer-create verbiste-buffer))
        verb)
    (with-current-buffer buf
      (kill-region (point-min) (point-max))
      (verbiste-first-banner)
      (verbiste-list-tense 0 6 split verb)
      (verbiste-second-banner)
      (verbiste-list-tense 6 11 split verb)
      (display-buffer buf t))))

(defun verbiste-list-tense (vls-start vls-end split verb)
  "List all tenses from VLS-START to VLS-END."
  (let ((tense 0)
        (declination 0))
    (while (< declination 6)
      (setq tense vls-start)
      (while (< tense vls-end)
        (setq verb (nth declination (nth tense split)))
        (if verb
            (insert (verbiste-pad-string verb))
          (insert "            "))
        (setq tense (1+ tense)))
      (insert "\n")
      (setq declination (1+ declination)))))


(defun verbiste-first-banner ()
  "Display the first banner."
  (verbiste-banner
   (concat
    "infinitif   present     imparfait   "
    "futur       passé       conditionnel\n"
    "                                    "
    "                        présent\n")))

(defun verbiste-second-banner ()
  "Display the second banner"
  (verbiste-banner
   (concat
    "\n"
    "subjonctif  subjonctif  imperatif   "
    "participe   participe\n"
    "présent     imparfait   présent     "
    "présent     passé\n")))

(defun verbiste-banner (string)
  "Common function for displaying banner, set face to bold"
  (insert (propertize string 'face 'bold)))

(defun verbiste-pad-string (string)
  "Pad a string for nice column display."
  (let ((len (length string))
        (padding 12))
    (if (< len padding)
        (concat string (make-string (- padding len) ?\s))
      (substring string (- len padding)))))

;; Local Variables:
;; compile-command: "make"
;; End:

;; Copyright (C) 2010 Ivan Kanis
;; Author: Ivan Kanis
;;
;; This program is free software ; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation ; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY ; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
