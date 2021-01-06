;;;; ./src/example/example.lisp

(defpackage #:mnas-package/example (:use #:cl))

(in-package :mnas-package/example)

(defparameter *a* '(0 1 2 3 4 5 6)
  "Documentation for function *a* ==================================================================")

(defparameter *b* '(0 1 2 3 4 5 6)
  "Documentation for function *b*")

(defun f-foo ()
  "Documentation for function f-foo ================================================================"
  t)

(defun (setf f-foo) (val lst n)
  "Documentation for function (setf f-foo) =========================================================" 
  (setf (nth n lst) (* val val))
  lst)

(defun f-bar ()
  "Documentation for function f-bar ================================================================"
  (f-foo))

(defun f-baz ()
  "Documentation for function f-baz ================================================================"
    (f-bar))

(defun fs-foo ()
    "Documentation for function fs-foo"  
    t)

(defun fs-bar ()
    "Documentation for function fs-bar"
    (f-foo))

(defun fs-baz ()
    "Documentation for function fs-baz"
    (fs-bar))

(defclass <a> () ()
    (:documentation
     "Documentation for class <a> =================================================================="))

(defclass <b> (<a>) ()
    (:documentation
     "Documentation for class <b> ================================================================================"))

(defclass <c> (<b>) ()
    (:documentation
     "Documentation for class <c> ================================================================================"))

(defclass <a-s> () ()
    (:documentation
     "Documentation for class <a-s>"))

(defclass <b-s> (<a>) ()
    (:documentation
     "Documentation for class <b-s>"))

(defclass <c-s> (<b>) ()
    (:documentation
     "Documentation for class <c-s>"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric m-foo (x y z)
    (:documentation
     "Documentation for defgeneric m-foo ================================================================================"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod m-foo ((x <a>) (y <b>) (z <c>))
    "Documentation for defmethod m-foo (x <a>) (y <b>) (z <c>)  =================================================="
    t)

(defmethod m-foo :after ((x <a>) (y <b>) (z <c>))
    "Documentation for defmethod m-foo :after (x <a>) (y <b>) (z <c>) =================================================="
    t)

(defmethod m-foo :before ((x <a>) (y <b>) (z <c>))
    "Documentation for defmethod m-foo :before (x <a>) (y <b>) (z <c>) =================================================="
    t)

(defmethod m-foo :around ((x <a>) (y <b>) (z <c>))
    "Documentation for defmethod m-foo :before (x <a>) (y <b>) (z <c>) =================================================="
    t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod m-foo ((x <a>) (y <b>) z)
    "Documentation for defmethod m-foo (x <a>) (y <b>) (z <c>) =================================================="
    t)

(defmethod m-foo :after ((x <a>) (y <b>) z)
    "Documentation for defmethod m-foo :after (x <a>) (y <b>) (z <c>) =================================================="
    t)

(defmethod m-foo :before ((x <a>) (y <b>) z)
    "Documentation for defmethod m-foo :before (x <a>) (y <b>) (z <c>) =================================================="
    t)

(defmethod m-foo :around ((x <a>) (y <b>) z)
    "Documentation for defmethod m-foo :before (x <a>) (y <b>) (z <c>) =================================================="
    t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric ms-foo (x y z)
    (:documentation
     "Documentation for defgeneric ms-foo"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod ms-foo ((x <a>) (y <b>) (z <c>))
    "Documentation for defmethod ms-foo (x <a>) (y <b>) (z <c>)"
    t)

(defmethod ms-foo :after ((x <a>) (y <b>) (z <c>))
    "Documentation for defmethod ms-foo :after (x <a>) (y <b>) (z <c>)"
    t)

(defmethod ms-foo :before ((x <a>) (y <b>) (z <c>))
    "Documentation for defmethod ms-foo :before (x <a>) (y <b>) (z <c>)"
    t)

(defmethod ms-foo :around ((x <a>) (y <b>) (z <c>))
    "Documentation for defmethod ms-foo :before (x <a>) (y <b>) (z <c>)"
    t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod ms-foo ((x <a>) (y <b>) z)
    "Documentation for defmethod ms-foo (x <a>) (y <b>) z"
    t)

(defmethod ms-foo :after ((x <a>) (y <b>) z)
    "Documentation for defmethod ms-foo :after (x <a>) (y <b>) <c>"
    t)

(defmethod ms-foo :before ((x <a>) (y <b>) z)
    "Documentation for defmethod ms-foo :before (x <a>) (y <b>) <c>"
    t)

(defmethod ms-foo :around ((x <a>) (y <b>) z)
    "Documentation for defmethod ms-foo :before (x <a>) (y <b>) z"
    t)  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
