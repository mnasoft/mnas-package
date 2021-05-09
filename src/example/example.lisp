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
  ((a))
  (:documentation
   "Documentation example for class <a> ===================================================================="))

(defclass <b> (<a>) ()
  (:documentation
   "Documentation example for class <b> ===================================================================="))

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

(defgeneric (setf m-foo) (val obj)
  (:documentation
   "Documentation example for defgeneric @b((setf m-foo))
================================================================================"))

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




