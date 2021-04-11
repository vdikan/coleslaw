(ql:quickload :coleslaw)
(ql:quickload :cl-org-mode)
(ql:quickload :trivia)

(defpackage :coleslaw-org-books
  (:use :cl)
  (:export #:enable)
  (:import-from :coleslaw
                #:*config*
                #:assert-field
                #:index
                #:content
                #:find-all
                #:by-date
                #:render
                #:publish
                #:theme-fn
                #:render-text
                #:add-document
                #:write-document))

(in-package :coleslaw-org-books)

;;FIXME: For plugin-like package definition to work I need
;; to create instances with `coleslaw::url`-like fields.
;; Same is done in `static-pages` (initialize-instance).
;; That should fix `sitemap` too.
;; (in-package :coleslaw)


;; (defclass book (content) ())


(defvar *org-src* #p"~/Grimoire/org/brain/Books.org")


(defvar shelf-categories
  '(("Computer Science" compsci)
    ("Social Science"   social)
    ("Natural Science"  natural)
    ("Fiction"          fiction)))


(defun shelf-for-category (name)
  (second (assoc name shelf-categories :test #'string-equal)))


(defun parse-org-src ()
  (cl-org-mode-raw:org-raw-parse *org-src*))


(defun get-revised ()
  (nth 3 (third
          (rest (parse-org-src)))))


(defun get-categories ()
  (trivia:match (get-revised)
    ((cons :entry
           (cons '(:STARS 2 :TITLE "Revised")
                 (cons x y))) y)))


(defun get-category-name (category)
  (trivia:match category
    ((list* :entry
            (list :stars _ :title name) _)
     name)))


(defun parse-entry (entry shelf-symbol)
  (trivia:match entry
    ((list :ENTRY (list :STARS _ :TITLE title)
           (list :SECTION
                 (list :PROPERTY-DRAWER "PROPERTIES" :CONTENTS
                       (list (list :PROPERTY "AUTHOR" :VALUE author)
                             (list :PROPERTY "ADDED" :VALUE added-date)
                             (list :PROPERTY "GOODREADS" :VALUE goodreads)
                             _))
                 review-text))
     (list :title title
           :author author
           :added-date added-date
           :goodreads goodreads
           :review-text review-text
           :shelf-symbol shelf-symbol))))


(loop for category in (get-categories)
      do (let* ((category-name (get-category-name category))
                (shelf-symbol (shelf-for-category category-name)))
           (loop for entry in (cdddr category)
                 do (print (parse-entry entry shelf-symbol)))))


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
