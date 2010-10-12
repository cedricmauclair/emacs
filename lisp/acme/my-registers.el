;; Time-stamp: <2010-04-08 13:47:11 cmauclai>


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


;; Local Variables:
;; eval:(set-regexp-face "^\s*;;\\([{}]*\\)?\\( =+ .* =+\\)?$" 'VioletRed4-bold-italic)
;; eval:(set-regexp-face "^\s*;;\\([{}]*\\)?\\( \\*+ .* \\*+\\)?$" 'Green4-bold-italic)
;; eval:(set-regexp-face "^\s*;;\\([{}]*\\)?\\( \\[+ .* \\]+\\)?$" 'h00688b-bold-italic)
;; End:
