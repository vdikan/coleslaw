;;NOTE: The new INDEX-objects, like org-books "shelf" or "sitemap",
;; are not hooked up from plugins for some reasons,
;; so I place them in /src

(in-package :coleslaw)


(defvar shelf-categories
  '(("Computer Science" compsci)
    ("Social Science"   social)
    ("Natural Science"  natural)
    ("Fiction"          fiction))
  "Shelf categories against their db symbols.")


(defun shelf-for-category (name)
  (second (assoc name shelf-categories :test #'string-equal)))


(defun shelf-for-symbol (sym)
  (caar (remove-if-not (lambda (x) (eq sym (second x)))
                       shelf-categories)))


(defun shelf-symbols ()
  (loop :for shelf-cat :in shelf-categories :collect (second shelf-cat)))


(defun parse-org-src ()
  (cl-org-mode-raw:org-raw-parse (org-books-src *config*)))


(defun get-revised ()
  (third (rest (parse-org-src))))


(defun get-categories ()
  (trivia:match (get-revised)
    ((cons :entry
           (cons (list* :STARS _ :TITLE _)
                 (cons _ y))) y)))


(defun get-category-name (category)
  (trivia:match category
    ((list* :entry
            (list :stars _ :title name) _)
     name)))


(defun cleanup-date (date)
  (ppcre:regex-replace-all "[\\s\\[\\]]+" date ""))


(defun cleanup-text (text)
  (string-trim " " (ppcre:regex-replace-all "\\n" text " ")))


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
           :date (cleanup-date added-date)
           :goodreads goodreads
           :text (cleanup-text review-text)
           :shelfsym shelf-symbol
           :shelfstr (string-downcase
                      (format nil "~a" shelf-symbol))))))


(defun get-all-books ()
  (loop for category in (get-categories)
        appending (let* ((category-name (get-category-name category))
                         (shelf-symbol (shelf-for-category category-name)))
                    (loop for entry in (cdddr category)
                          collect (parse-entry entry shelf-symbol)))))


(defclass base-shelf ()
  ((booklist :initarg :books :reader booklist)))


(defclass shelf (index base-shelf) ())


(defmethod discover ((doc-type (eql (find-class 'shelf))))
  (let ((books (sort (get-all-books) #'string>
                     :key (lambda (x) (getf x :date)))))
    (let ((root-shelf
            (make-instance 'shelf
                           :books books
                           :slug "all"
                           :name "shelf-all"
                           :title "All the Library Hall Records")))
      (add-document root-shelf))
    (dolist (sym (shelf-symbols))
      (let* ((books
               (remove-if-not (lambda (x) (eq sym (getf x :shelfsym)))
                              books))
             (shelf (make-instance 'shelf
                                   :books books
                                   :slug (string-downcase
                                          (format nil "~a" sym))
                                   :name (string-downcase
                                          (format nil "shelf-~a" sym))
                                   :title (format nil "Book Shelf: ~a"
                                                  (shelf-for-symbol sym)))))
        (add-document shelf)))))


(defmethod render ((object shelf) &key prev next)
  (declare (ignore next prev))
  (funcall (theme-fn 'shelf)
           (list :config *config*
                 :shelf object)))


(defmethod publish ((doc-type (eql (find-class 'shelf))))
  (dolist (shelf (find-all 'shelf))
    (write-document shelf)))
