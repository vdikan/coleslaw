(in-package :coleslaw)

(defclass post (content)
  ((title  :initarg :title  :reader title-of)
   (author :initarg :author :reader author-of)
   (excerpt :initarg :excerpt :reader excerpt-of)
   (format :initarg :format :reader post-format)
   (ogimage :initarg :ogimage :reader ogimage-of))
  (:default-initargs :author nil :excerpt nil :ogimage nil))

(defmethod initialize-instance :after ((object post) &key)
  (with-slots (url title author excerpt format ogimage text) object
    (let (post-content)
      (setf url (compute-url object (slugify title))
            format (make-keyword (string-upcase format))
            post-content (render-text text format)
            excerpt (or excerpt
                        (first (split (excerpt-sep *config*)
                                      post-content
                                      :limit 2)))
            text post-content
            author (or author (author *config*))
            ;; ogimage (or ogimage (ogimage *config*))
            ogimage ogimage))))

(defmethod render ((object post) &key prev next)
  (funcall (theme-fn 'post) (list :config *config*
                                  :post object
                                  :prev prev
                                  :next next)))

(defmethod publish ((doc-type (eql (find-class 'post))))
  (loop for (next post prev) on (append '(nil) (by-date (find-all 'post)))
     while post do (write-document post nil :prev prev :next next)))
