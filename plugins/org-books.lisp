;; (defpackage :coleslaw-org-books
;;   (:use :cl)
;;   (:export #:enable)
;;   (:import-from :coleslaw
;;                 #:*config*
;;                 #:assert-field
;;                 #:index
;;                 #:content
;;                 #:find-all
;;                 #:by-date
;;                 #:render
;;                 #:publish
;;                 #:theme-fn
;;                 #:render-text
;;                 #:add-document
;;                 #:write-document))

;; (in-package :coleslaw-org-books)

;;FIXME: For plugin-like package definition to work I need
;; to create instances with `coleslaw::url`-like fields.
;; Same is done in `static-pages` (initialize-instance).
;; That should fix `sitemap` too.
(in-package :coleslaw)


;; (defclass book (content) ())


;;;SHELF
(defclass base-shelf ()
  ((genre :initarg :genre :reader shelf-genre)))


(defclass shelf (index base-shelf) ())


(defmethod discover ((doc-type (eql (find-class 'shelf))))
  (let ((content (by-date (find-all 'post))))
    (dolist (genre '(all compsci social natural fiction))
      (let ((shelf (make-instance 'shelf
                                  :genre genre
                                  :content content
                                  :slug (string-downcase
                                         (format nil "~a" genre))
                                  :name (format nil "shelf-~a" genre)
                                  :title (format nil "Book Shelf: ~a" genre))))
        (add-document shelf)))))


(defmethod render ((object shelf) &key prev next)
  (declare (ignore next prev))
  (funcall (theme-fn 'shelf)
           (list :config *config*
                 :shelf object)))


(defmethod publish ((doc-type (eql (find-class 'shelf))))
  (dolist (shelf (find-all 'shelf))
    (write-document shelf)))


(defun enable ())
