;;;; ./src/example/example.lisp

(defpackage #:mnas-package/example
  (:use #:cl)
  (:export *a-exp* *b-exp*)
  (:export foo bar baz)
  (:export baz-short)
  (:export <c>)
  (:export mak-a)
  (:export m-foo)
  (:documentation
   "Documentation example for package @b(mnas-package/example)
================================================================================")
  )

(in-package :mnas-package/example)

(defparameter *a-exp* '(0 1 2 3 4 5 6)
  "Documentation example for variable @b(*a-exp*)
================================================================================")

(defparameter *b-exp* '(0 1 2 3 4 5 6)
  "Documentation example for variable @b(*b-exp*)
================================================================================")

(defparameter *c* '(0 1 2 3 4 5 6)
  "Documentation example for variable @b(*c*)")

(defun foo ()
  "Documentation example for function @b(foo)
================================================================================"
  t)

(defun (setf foo) (val lst n)
  "Documentation example for function @b((setf foo))
================================================================================" 
  (setf (nth n lst) (* val val))
  lst)

(defun bar ()
  "Documentation example for function bar
================================================================================"
  (foo))

(defun baz ()
  "Documentation example for function baz 
================================================================================"
  (bar))

(defun foo-short ()
  "Documentation example for function foo-short"  
  t)

(defun bar-short ()
  "Documentation example for function bar-short"
  (foo))

(defun baz-short ()
  "Documentation example for function baz-short"
  (bar-short))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro mak-a (a b)
  "Documentation example for macro mak-a ==================================================================="
  `(list ,a ,b))

(defmacro mak-a-short (a b)
  "Documentation example for macro mak-a"
  `(list ,a ,b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <a> ()
  ((a :accessor <a>-a :initarg a :initform nil))
  (:documentation
   "Documentation example for class <a> ===================================================================="))

(defmethod print-object ((a <a>) s) (format s "#<a>(a=~S)" (<a>-a a)))

(defclass <b> (<a>)
  ((b :accessor <b>-b :initarg b :initform nil))
  (:documentation
   "Documentation example for class <b> ===================================================================="))

(defmethod print-object ((b <b>) s) (format s "#<b>(b=~S)" (<b>-b b))
  (call-next-method))

(defclass <c> (<b>) ()
  (:documentation
   "Documentation example for class <c> ===================================================================="))

(defclass <a-short> () ()
  (:documentation
   "Documentation example for class <a-short>"))

(defclass <b-short> (<a-short>) ()
  (:documentation
   "Documentation example for class <b-s>"))

(defclass <c-short> (<b-short>) ()
  (:documentation
   "Documentation example for class <c-s>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; defgeneric

(defgeneric (setf m-foo) (val obj)
  (:documentation
   "Documentation example for defgeneric @b((setf m-foo))
================================================================================"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; defmethods

(defmethod (setf m-foo) (val (a <a>))
  "Documentation example for defgeneric @b((setf m-foo) (val (a <a>)))
================================================================================"
  (setf (<a>-a a) val)
  a)

(defmethod (setf m-foo) ((val number) (a <a>))
    "Documentation example for defgeneric @b((setf m-foo) ((val number) (a <a>)))
================================================================================"
  (setf (<a>-a a) (1+ val))
  a)

(defmethod (setf m-foo) (val (b <b>))
    "Documentation example for defgeneric @b((setf m-foo) (val (b <b>)))
================================================================================"
  (setf (<b>-b b) val)
  b)

(defmethod (setf m-foo) ((val number) (b <b>))
    "Documentation example for defgeneric @b((setf m-foo) ((val number) (b <b>)))
================================================================================"
  (setf (<b>-b b) (1+ val))
  b)

#|
(defparameter *<a>* (make-instance '<a>))
(defparameter *<b>* (make-instance '<b>))

(setf (m-foo *<b>*) 10.25)
(setf (m-foo *<a>*) 210.25)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric m-foo (x y z)
  (:documentation
   "Documentation example for defgeneric @b(m-foo)
================================================================================"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod m-foo ((x <a>) (y <b>) (z <c>))
  "Documentation example for defmethod m-foo (x <a>) (y <b>) (z <c>)  01 ==================================="
  t)

(defmethod m-foo :after ((x <a>) (y <b>) (z <c>))
  "Documentation example for defmethod m-foo :after (x <a>) (y <b>) (z <c>) 02 ============================="
  t)

(defmethod m-foo :before ((x <a>) (y <b>) (z <c>))
  "Documentation example for defmethod m-foo :before (x <a>) (y <b>) (z <c>) 03 ============================"
  t)

(defmethod m-foo :around ((x <a>) (y <b>) (z <c>))
  "Documentation example for defmethod m-foo :before (x <a>) (y <b>) (z <c>) 04 ============================"
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod m-foo ((x <a>) (y <b>) z)
  "Documentation example for defmethod m-foo (x <a>) (y <b>) (z <c>) 05 ===================================="
  t)

(defmethod m-foo :after ((x <a>) (y <b>) z)
  "Documentation example for defmethod m-foo :after (x <a>) (y <b>) (z <c>) 06 ============================="
  t)

(defmethod m-foo :before ((x <a>) (y <b>) z)
  "Documentation example for defmethod m-foo :before (x <a>) (y <b>) (z <c>) 07 ============================"
  t)

(defmethod m-foo :around ((x <a>) (y <b>) z)
  "Documentation example for defmethod m-foo :before (x <a>) (y <b>) (z <c>) 08 ============================"
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric m-foo-short (x y z)
  (:documentation
   "Documentation example for defgeneric m-foo-short"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod m-foo-short ((x <a>) (y <b>) (z <c>))
  "Documentation example for defmethod m-foo-short (x <a>) (y <b>) (z <c>) 01"
  t)

(defmethod m-foo-short :after ((x <a>) (y <b>) (z <c>))
  "Documentation example for defmethod m-foo-short :after (x <a>) (y <b>) (z <c>) 02"
  t)

(defmethod m-foo-short :before ((x <a>) (y <b>) (z <c>))
  "Documentation example for defmethod m-foo-short :before (x <a>) (y <b>) (z <c>) 03"
  t)

(defmethod m-foo-short :around ((x <a>) (y <b>) (z <c>))
  "Documentation example for defmethod m-foo-short :before (x <a>) (y <b>) (z <c>) 04"
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod m-foo-short ((x <a>) (y <b>) z)
  "Documentation example for defmethod m-foo-short (x <a>) (y <b>) z 05"
  t)

(defmethod m-foo-short :after ((x <a>) (y <b>) z)
  "Documentation example for defmethod m-foo-short :after (x <a>) (y <b>) <c> 06"
  t)

(defmethod m-foo-short :before ((x <a>) (y <b>) z)
  "Documentation example for defmethod m-foo-short :before (x <a>) (y <b>) <c> 07"
  t)

(defmethod m-foo-short :around ((x <a>) (y <b>) z)
  "Documentation example for defmethod m-foo-short :before (x <a>) (y <b>) z 08"
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




