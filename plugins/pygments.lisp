(defpackage #:coleslaw-pygments
  (:use #:cl)
  (:export #:enable))

(in-package #:coleslaw-pygments)

(defun enable ()
  (setf 3bmd-code-blocks:*renderer* :pygments)
  (setf 3bmd-code-blocks:*code-blocks-pre-class* ".highlight")
  (setf 3bmd-code-blocks:*code-blocks-span-class* ".highlight"))
