(defpackage :coleslaw-highlightjs
  (:use :cl)
  (:export #:enable)
  (:import-from :coleslaw #:add-injection
                          #:content
                          #:index
                          #:tag-p
                          #:index-content))

(in-package :coleslaw-highlightjs)

(defvar *highlightjs-header* "~%
<script src=\"~A/js/highlight.pack.js\"></script>
<script src=\"~A/js/highlightjs-line-numbers.min.js\"></script>

<script>hljs.initHighlightingOnLoad();</script>
<script>hljs.initLineNumbersOnLoad();</script>
")

(defgeneric highlightjs-p (document)
  (:documentation "Test if DOCUMENT requires contains any highlighted source code content.")
  (:method ((content content))
    (tag-p "code" content))
  (:method ((index index))
    (and (slot-boundp index 'content)
         (some #'highlightjs-p (index-content index)))))

(defun enable (&key force base-url)
  (flet ((inject-p (x)
           (when (or force (highlightjs-p x))
             (format nil *highlightjs-header* base-url base-url))))
    (add-injection #'inject-p :head)))
