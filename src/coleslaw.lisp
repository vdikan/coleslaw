(in-package :coleslaw)

(defvar *last-revision* nil
  "The git revision prior to the last push. For use with GET-UPDATED-FILES.")

(defun main (repo-dir &key oldrev (deploy t))
  "Load the user's config file, compile the blog in REPO-DIR into STAGING-DIR,
 and optionally deploy the blog to DEPLOY-DIR.
  OLDREV -- the git revision prior to the last push.
  DEPLOY -- when non-nil, perform the deploy. (default: t)"
  (load-config repo-dir)
  (setf *last-revision* oldrev)
  (load-content)
  (compile-theme (theme *config*))
  (let ((dir (staging-dir *config*)))
    (compile-blog dir)
    (when deploy
      (deploy dir))))

(defun load-content ()
  "Load all content stored in the blog's repo."
  (do-subclasses (ctype content)
    (discover ctype))
  (update-content-metadata)
  (do-subclasses (itype index)
    (discover itype)))

(defun compile-blog (staging)
  "Compile the blog to a STAGING directory as specified in .coleslawrc."
  (ensure-directories-exist staging)
  (with-current-directory staging
    (let ((theme-dir (find-theme (theme *config*))))
      (dolist (dir (list (merge-pathnames "css" theme-dir)
                         (merge-pathnames "img" theme-dir)
                         (merge-pathnames "js" theme-dir)
                         (repo-path "static")))
        (when (probe-file dir)
          (run-program "rsync --delete -raz ~a ." dir)))
      (cl-fad:walk-directory            ; sync files from `root-static`
       (repo-path "root-static")        ; to the root of staging dir
       #'(lambda (file)
           (run-program "rsync --delete -raz ~a ." file))
       :if-does-not-exist :ignore))
    (do-subclasses (ctype content)
      (publish ctype))
    (do-subclasses (itype index)
      (publish itype))
    (update-symlink (format nil "index.~A" (page-ext *config*))
                    (format nil "1.~A" (page-ext *config*)))))

(defgeneric deploy (staging)
  (:documentation "Deploy the STAGING build to the directory specified in the config.")
  (:method (staging)
    "By default, do nothing"
    (declare)))

(defun update-symlink (path target)
  "Update the symlink at PATH to point to TARGET."
  (run-program "ln -sfn ~a ~a" target path))

(defun preview (path &optional (content-type 'post))
  "Render the content at PATH under user's configured repo and save it to
~/tmp.html. Load the user's config and theme if necessary."
  (let ((current-working-directory (cl-fad:pathname-directory-pathname path)))
    (unless *config*
      (load-config (namestring current-working-directory))
      (compile-theme (theme *config*)))
    (let* ((file (rel-path (repo-dir *config*) path))
           (content (construct content-type (read-content file))))
      (write-file "tmp.html" (render-page content)))))

(defun render-page (content &optional theme-fn &rest render-args)
  "Render the given CONTENT to HTML using THEME-FN if supplied.
Additional args to render CONTENT can be passed via RENDER-ARGS."
  (funcall (or theme-fn (theme-fn 'base))
           (list :config *config*
                 :content content
                 :raw (apply 'render content render-args)
                 :pubdate (format-rfc1123-timestring nil (local-time:now))
                 :injections (find-injections content))))
