;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(let* ((el-get-dir        (expand-file-name "~/.emacs.d/el-get/"))
       (dummy             (unless (file-directory-p el-get-dir)
                            (make-directory el-get-dir t)))
       (package           "el-get")
       (bname             "*el-get bootstrap*")
       (pdir              (concat (file-name-as-directory el-get-dir) package))
       (git               (or (executable-find "git") (error "Unable to find `git'")))
       (url               "git://github.com/dimitri/el-get.git")
       (el-get-sources    `((:name ,package :type "git" :url ,url :features el-get :compile "el-get.el")))
       (default-directory el-get-dir)
       (process-connection-type nil) ; pipe, no pty (--no-progress)
       (status            (call-process git nil bname t "--no-pager" "clone" "-v" url package)))
  (set-window-buffer (selected-window) bname)
  (when (eq 0 status)
    (load (concat (file-name-as-directory pdir) package ".el"))
    ;; (require 'bytecomp)
    (el-get-init "el-get")
    (with-current-buffer bname
      (goto-char (point-max))
      (insert "\nCongrats, el-get is installed and ready to serve!"))))
nil
