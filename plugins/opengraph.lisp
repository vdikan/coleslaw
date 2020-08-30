(defpackage :coleslaw-opengraph
  (:use :cl)
  (:export #:enable)
  (:import-from :coleslaw #:add-injection
                          #:ogimage-of
                          #:title-of
                          #:page-url
                          #:post))

(in-package :coleslaw-opengraph)

(defvar *og-metas*
  "~%
<meta property=\"og:title\" content=\"~a\" />
<meta property=\"og:url\" content=\"~a/~a\" />
<meta property=\"og:image\" content=\"~a/~a\" />
<meta property=\"og:image:secure_url\" content=\"~a/~a\" />
<meta property=\"og:image:alt\" content=\"~a\" />")

(defun enable (&key og-base-url og-default-image)
  (flet ((inject-p (x)
           (when (typep x 'post)
             (format nil *og-metas*
                     (title-of x)
                     og-base-url (page-url x)
                     og-base-url (or (ogimage-of x) og-default-image)
                     og-base-url (or (ogimage-of x) og-default-image)
                     (format nil "Image for: ~a" (title-of x))))))
    (add-injection #'inject-p :head)))
