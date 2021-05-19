;;;; ./src/example/example.lisp

(defpackage #:mnas-package/example
  (:use #:cl)
  ;; Vriables
  (:export *v-c-exp* *v-a-exp* *v-b-exp*)
  (:intern *v-c-int* *v-a-int* *v-b-int*)
  ;; Functionss
  (:export  f-c-exp   f-a-exp   f-b-exp )
  (:intern  f-c-int   f-a-int   f-b-int )
  ;; Classes
  (:export <c-с-exp> <c-a-exp> <c-b-exp>)
  (:intern <c-с-int> <c-a-int> <c-b-int>)
  ;; Macro
  (:export  k-c-exp   k-a-exp   k-b-exp )
  (:intern  k-c-int   k-a-int   k-b-int )
  ;; Generics, methods
  (:export  m-c-exp   m-a-exp   m-b-exp )
  (:intern  m-c-int   m-a-int   m-b-int )
  (:documentation
   "Documentation example for package @b(mnas-package/example)

 Соглашения по именованию.

 Имена должны начинаться с группы символов:
@begin(list)
 @item(\"v\" - для глобальных переменных;)
 @item(\"f\" - для функций;)
 @item(\"k\" - для макросов;)
 @item(\"m\" - для обобщенных функций и соответствующих им методов;)
 @item(\"с\" - классов;)
 @item(\"s\" - структур.)
@end(list)

 Имена должны иметь признак, указываемый предпоследним:
@begin(list)
 @item(\"exp\" - для экспортируемых символов;)
 @item(\"int\" - для внутрених символов.)
@end(list)

 Имена должны заканчиваться на:
@begin(list)
 @item(\"ds\" - для элементов, с короткими строками документации;)
 @item(\"dl\" - для элементов, с длинными строками документации.)
@end(list)
"
  )

(in-package :mnas-package/example)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Varibles
(defparameter *v-c-exp* '(0 1 2 3 4 5 6)
    "Documentation example for variable @b(*v-c-exp*)
================================================================================")

(defparameter *v-a-exp* '(0 1 2 3 4 5 6)
  "Documentation example for variable @b(*v-a-exp*)
================================================================================")

(defparameter *v-b-exp* '(0 1 2 3 4 5 6)
    "Documentation example for variable @b(*v-b-exp*)
================================================================================")

(defparameter *v-c-int* '(0 1 2 3 4 5 6)
  "Documentation example for variable @b(*v-c-int*)
================================================================================")

(defparameter *v-a-int* '(0 1 2 3 4 5 6)
  "Documentation example for variable @b(*v-a-int*)")

(defparameter *v-b-int* '(0 1 2 3 4 5 6)
  "Documentation example for variable @b(*v-a-int*)")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Functions
(defun f-c-exp ()
  "Documentation example for function @b(f-c-exp)
================================================================================"
  t)

(defun f-a-exp ()
  "Documentation example for function @b(f-a-exp)
================================================================================"
  t)

(defun f-b-exp ()
  "Documentation example for function @b(f-b-exp)
================================================================================"
  t)

(defun f-c-int ()
  "Documentation example for function @b(f-c-exp)
================================================================================"
  t)

(defun f-a-int ()
  "Documentation example for function @b(f-a-exp)
================================================================================"
  t)

(defun f-b-int ()
  "Documentation example for function @b(f-b-exp)
================================================================================"
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setf-Functions

(defun (setf f-b-exp) (val lst n)
  "Documentation example for function @b((setf f-b-exp))
================================================================================" 
  (setf (nth n lst) (* val val))
  lst)

(defun f-a-exp ()
  "Documentation example for function f-a-exp
================================================================================"
  (f-b-exp))

(defun f-c-exp ()
  "Documentation example for function f-c-exp 
================================================================================"
  (f-a-exp))

(defun f-b-exp ()
  "Documentation example for function f-b-exp"  
  t)

(defun f-a-exp ()
  "Documentation example for function f-a-exp"
  (f-b-exp))

(defun f-c-exp ()
  "Documentation example for function f-c-exp"
  (f-a-exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro k-a (a b)
  "Documentation example for macro k-a ==================================================================="
  `(list ,a ,b))

(defmacro k-a (a b)
  "Documentation example for macro k-a"
  `(list ,a ,b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <c-a-int> ()
  ((a :accessor <c-a-int>-a :initarg a :initform nil))
  (:documentation
   "Documentation example for class <c-a-int> ===================================================================="))

(defmethod print-object ((a <c-a-int>) s) (format s "#<c-a-int>(a=~S)" (<c-a-int>-a a)))

(defclass <c-b-int> (<c-a-int>)
  ((b :accessor <c-b-int>-b :initarg b :initform nil))
  (:documentation
   "Documentation example for class <c-b-int> ===================================================================="))

(defmethod print-object ((b <c-b-int>) s) (format s "#<c-b-int>(b=~S)" (<c-b-int>-b b))
  (call-next-method))

(defclass <c-с-exp> (<c-b-int>) ()
  (:documentation
   "Documentation example for class <c-с-exp> ===================================================================="))

(defclass <a> () ()
  (:documentation
   "Documentation example for class <a>"))

(defclass <b> (<a>) ()
  (:documentation
   "Documentation example for class <b-s>"))

(defclass <c> (<b>) ()
  (:documentation
   "Documentation example for class <c-s>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; defgeneric

(defgeneric (setf m-b-exp) (val obj)
  (:documentation
   "Documentation example for defgeneric @b((setf m-b-exp))
================================================================================"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; defmethods

(defmethod (setf m-b-exp) (val (a <c-a-int>))
  "Documentation example for defgeneric @b((setf m-b-exp) (val (a <c-a-int>)))
================================================================================"
  (setf (<c-a-int>-a a) val)
  a)

(defmethod (setf m-b-exp) ((val number) (a <c-a-int>))
    "Documentation example for defgeneric @b((setf m-b-exp) ((val number) (a <c-a-int>)))
================================================================================"
  (setf (<c-a-int>-a a) (1+ val))
  a)

(defmethod (setf m-b-exp) (val (b <c-b-int>))
    "Documentation example for defgeneric @b((setf m-b-exp) (val (b <c-b-int>)))
================================================================================"
  (setf (<c-b-int>-b b) val)
  b)

(defmethod (setf m-b-exp) ((val number) (b <c-b-int>))
    "Documentation example for defgeneric @b((setf m-b-exp) ((val number) (b <c-b-int>)))
================================================================================"
  (setf (<c-b-int>-b b) (1+ val))
  b)

#|
(defparameter *<c-a-int>* (make-instance '<c-a-int>))
(defparameter *<c-b-int>* (make-instance '<c-b-int>))

(setf (m-b-exp *<c-b-int>*) 10.25)
(setf (m-b-exp *<c-a-int>*) 210.25)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric m-b-exp (x y z)
  (:documentation
   "Documentation example for defgeneric @b(m-b-exp)
================================================================================"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod m-b-exp ((x <c-a-int>) (y <c-b-int>) (z <c-с-exp>))
  "Documentation example for defmethod m-b-exp (x <c-a-int>) (y <c-b-int>) (z <c-с-exp>)  01 ==================================="
  t)

(defmethod m-b-exp :after ((x <c-a-int>) (y <c-b-int>) (z <c-с-exp>))
  "Documentation example for defmethod m-b-exp :after (x <c-a-int>) (y <c-b-int>) (z <c-с-exp>) 02 ============================="
  t)

(defmethod m-b-exp :before ((x <c-a-int>) (y <c-b-int>) (z <c-с-exp>))
  "Documentation example for defmethod m-b-exp :before (x <c-a-int>) (y <c-b-int>) (z <c-с-exp>) 03 ============================"
  t)

(defmethod m-b-exp :around ((x <c-a-int>) (y <c-b-int>) (z <c-с-exp>))
  "Documentation example for defmethod m-b-exp :before (x <c-a-int>) (y <c-b-int>) (z <c-с-exp>) 04 ============================"
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod m-b-exp ((x <c-a-int>) (y <c-b-int>) z)
  "Documentation example for defmethod m-b-exp (x <c-a-int>) (y <c-b-int>) (z <c-с-exp>) 05 ===================================="
  t)

(defmethod m-b-exp :after ((x <c-a-int>) (y <c-b-int>) z)
  "Documentation example for defmethod m-b-exp :after (x <c-a-int>) (y <c-b-int>) (z <c-с-exp>) 06 ============================="
  t)

(defmethod m-b-exp :before ((x <c-a-int>) (y <c-b-int>) z)
  "Documentation example for defmethod m-b-exp :before (x <c-a-int>) (y <c-b-int>) (z <c-с-exp>) 07 ============================"
  t)

(defmethod m-b-exp :around ((x <c-a-int>) (y <c-b-int>) z)
  "Documentation example for defmethod m-b-exp :before (x <c-a-int>) (y <c-b-int>) (z <c-с-exp>) 08 ============================"
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric m-b-exp (x y z)
  (:documentation
   "Documentation example for defgeneric m-b-exp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod m-b-exp ((x <c-a-int>) (y <c-b-int>) (z <c-с-exp>))
  "Documentation example for defmethod m-b-exp (x <c-a-int>) (y <c-b-int>) (z <c-с-exp>) 01"
  t)

(defmethod m-b-exp :after ((x <c-a-int>) (y <c-b-int>) (z <c-с-exp>))
  "Documentation example for defmethod m-b-exp :after (x <c-a-int>) (y <c-b-int>) (z <c-с-exp>) 02"
  t)

(defmethod m-b-exp :before ((x <c-a-int>) (y <c-b-int>) (z <c-с-exp>))
  "Documentation example for defmethod m-b-exp :before (x <c-a-int>) (y <c-b-int>) (z <c-с-exp>) 03"
  t)

(defmethod m-b-exp :around ((x <c-a-int>) (y <c-b-int>) (z <c-с-exp>))
  "Documentation example for defmethod m-b-exp :before (x <c-a-int>) (y <c-b-int>) (z <c-с-exp>) 04"
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod m-b-exp ((x <c-a-int>) (y <c-b-int>) z)
  "Documentation example for defmethod m-b-exp (x <c-a-int>) (y <c-b-int>) z 05"
  t)

(defmethod m-b-exp :after ((x <c-a-int>) (y <c-b-int>) z)
  "Documentation example for defmethod m-b-exp :after (x <c-a-int>) (y <c-b-int>) <c-с-exp> 06"
  t)

(defmethod m-b-exp :before ((x <c-a-int>) (y <c-b-int>) z)
  "Documentation example for defmethod m-b-exp :before (x <c-a-int>) (y <c-b-int>) <c-с-exp> 07"
  t)

(defmethod m-b-exp :around ((x <c-a-int>) (y <c-b-int>) z)
  "Documentation example for defmethod m-b-exp :before (x <c-a-int>) (y <c-b-int>) z 08"
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
