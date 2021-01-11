;;;; ./src/example/example.lisp

(defpackage #:mnas-package/example
  (:use #:cl)
  (:export *a*
           baz
           baz-short
           <c>
           ))

(in-package :mnas-package/example)

(defparameter *a* '(0 1 2 3 4 5 6)
  "Documentation for function *a* ==================================================================")

(defparameter *b* '(0 1 2 3 4 5 6)
  "Documentation for function *b* ==================================================================")

(defparameter *c* '(0 1 2 3 4 5 6)
  "Documentation for function *c*")

(defun foo ()
  "Documentation for function foo =================================================================="
  t)

(defun (setf foo) (val lst n)
  "Documentation for function (setf foo) ===========================================================" 
  (setf (nth n lst) (* val val))
  lst)

(defun (setf setf-foo) (val lst n)
  "Documentation for function (setf setf-foo) =====================================================" 
  (setf (nth n lst) (* val val))
  lst)

(defun bar ()
  "Documentation for function bar =================================================================="
  (foo))

(defun baz ()
  "Documentation for function baz =================================================================="
  (bar))

(defun foo-short ()
  "Documentation for function foo-short"  
  t)

(defun bar-short ()
  "Documentation for function bar-short"
  (foo))

(defun baz-short ()
  "Documentation for function baz-short"
  (bar-short))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro mak-a (a b)
  "Documentation for macro mak-a ==================================================================="
  `(list ,a ,b))

(defmacro mak-a-short (a b)
  "Documentation for macro mak-a"
  `(list ,a ,b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <a> () ()
  (:documentation
   "Documentation for class <a> ===================================================================="))

(defclass <b> (<a>) ()
  (:documentation
   "Documentation for class <b> ===================================================================="))

(defclass <c> (<b>) ()
  (:documentation
   "Documentation for class <c> ===================================================================="))

(defclass <a-short> () ()
  (:documentation
   "Documentation for class <a-short>"))

(defclass <b-short> (<a-short>) ()
  (:documentation
   "Documentation for class <b-s>"))

(defclass <c-short> (<b-short>) ()
  (:documentation
   "Documentation for class <c-s>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric m-foo (x y z)
  (:documentation
   "Documentation for defgeneric m-foo ============================================================="))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod m-foo ((x <a>) (y <b>) (z <c>))
  "Documentation for defmethod m-foo (x <a>) (y <b>) (z <c>)  ======================================"
  t)

(defmethod m-foo :after ((x <a>) (y <b>) (z <c>))
  "Documentation for defmethod m-foo :after (x <a>) (y <b>) (z <c>) ================================"
  t)

(defmethod m-foo :before ((x <a>) (y <b>) (z <c>))
  "Documentation for defmethod m-foo :before (x <a>) (y <b>) (z <c>) ==============================="
  t)

(defmethod m-foo :around ((x <a>) (y <b>) (z <c>))
  "Documentation for defmethod m-foo :before (x <a>) (y <b>) (z <c>) ==============================="
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod m-foo ((x <a>) (y <b>) z)
  "Documentation for defmethod m-foo (x <a>) (y <b>) (z <c>) ======================================="
  t)

(defmethod m-foo :after ((x <a>) (y <b>) z)
  "Documentation for defmethod m-foo :after (x <a>) (y <b>) (z <c>) ================================"
  t)

(defmethod m-foo :before ((x <a>) (y <b>) z)
  "Documentation for defmethod m-foo :before (x <a>) (y <b>) (z <c>) ================================"
  t)

(defmethod m-foo :around ((x <a>) (y <b>) z)
  "Documentation for defmethod m-foo :before (x <a>) (y <b>) (z <c>) ================================"
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric m-foo-short (x y z)
  (:documentation
   "Documentation for defgeneric m-foo-short"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod m-foo-short ((x <a>) (y <b>) (z <c>))
  "Documentation for defmethod m-foo-short (x <a>) (y <b>) (z <c>)"
  t)

(defmethod m-foo-short :after ((x <a>) (y <b>) (z <c>))
  "Documentation for defmethod m-foo-short :after (x <a>) (y <b>) (z <c>)"
  t)

(defmethod m-foo-short :before ((x <a>) (y <b>) (z <c>))
  "Documentation for defmethod m-foo-short :before (x <a>) (y <b>) (z <c>)"
  t)

(defmethod m-foo-short :around ((x <a>) (y <b>) (z <c>))
  "Documentation for defmethod m-foo-short :before (x <a>) (y <b>) (z <c>)"
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod m-foo-short ((x <a>) (y <b>) z)
  "Documentation for defmethod m-foo-short (x <a>) (y <b>) z"
  t)

(defmethod m-foo-short :after ((x <a>) (y <b>) z)
  "Documentation for defmethod m-foo-short :after (x <a>) (y <b>) <c>"
  t)

(defmethod m-foo-short :before ((x <a>) (y <b>) z)
  "Documentation for defmethod m-foo-short :before (x <a>) (y <b>) <c>"
  t)

(defmethod m-foo-short :around ((x <a>) (y <b>) z)
  "Documentation for defmethod m-foo-short :before (x <a>) (y <b>) z"
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




