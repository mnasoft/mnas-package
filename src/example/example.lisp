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

 Имена должны иметь признак, указываемый последним:
@begin(list)
 @item(\"exp\" - для экспортируемых символов;)
 @item(\"int\" - для внутрених символов.)
@end(list)"
  ))

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

(defun (setf f-c-exp) (val lst n)
  "Documentation example for function @b((setf f-b-exp))
================================================================================" 
  (setf (nth n lst) (* val val))
  lst)

(defun (setf f-a-exp) (val lst n)
  "Documentation example for function @b((setf f-b-exp))
================================================================================" 
  (setf (nth n lst) (* val val))
  lst)

(defun (setf f-b-exp) (val lst n)
  "Documentation example for function @b((setf f-b-exp))
================================================================================" 
  (setf (nth n lst) (* val val))
  lst)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Makro
(defmacro k-c-exp (a b)
  "Documentation example for macro k-c-exp 
==================================================================="
  `(list ,a ,b))

(defmacro k-a-exp (a b)
  "Documentation example for macro k-a-exp
==================================================================="
  `(list ,a ,b))

(defmacro k-b-exp (a b)
  "Documentation example for macro k-b-exp
==================================================================="
  `(list ,a ,b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro k-c-int (a b)
  "Documentation example for macro k-a ==================================================================="
  `(list ,a ,b))

(defmacro k-a-int (a b)
  "Documentation example for macro k-a ==================================================================="
  `(list ,a ,b))

(defmacro k-b-int (a b)
  "Documentation example for macro k-a"
  `(list ,a ,b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Classes
(defclass <c-c-int> ()
  ((c :accessor <c-c-int>-c :initarg c :initform nil))
  (:documentation
   "Documentation example for class <c-c-int> 
===================================================================="))

(defmethod print-object ((c <c-c-int>) s) (format s "#<c-c-int>(a=~S)" (<c-c-int>-c c)))

(defclass <c-a-int> ()
  ((a :accessor <c-a-int>-a :initarg a :initform nil))
  (:documentation
   "Documentation example for class <c-a-int> 
===================================================================="))

(defmethod print-object ((a <c-a-int>) s) (format s "#<c-a-int>(a=~S)" (<c-a-int>-a a)))

(defclass <c-b-int> (<c-a-int>)
  ((b :accessor <c-b-int>-b :initarg b :initform nil))
  (:documentation
   "Documentation example for class <c-b-int> 
===================================================================="))

(defmethod print-object ((b <c-b-int>) s) (format s "#<c-b-int>(b=~S)" (<c-b-int>-b b))
  (call-next-method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <c-с-exp> (<c-c-int>) ()
  (:documentation
   "Documentation example for class <c-с-exp> 
===================================================================="))

(defclass <c-a-exp> (<c-a-int>) ()
  (:documentation
   "Documentation example for class <c-a-exp> 
===================================================================="))

(defclass <c-b-exp> (<c-b-int>) ()
  (:documentation
   "Documentation example for class <c-b-exp> 
===================================================================="))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Generics
(defgeneric m-c-exp (x y z)
  (:documentation
   "Documentation example for defgeneric @b(m-c-exp)
================================================================================"))

(defgeneric m-a-exp (x y z)
  (:documentation
   "Documentation example for defgeneric @b(m-a-exp)
================================================================================"))

(defgeneric m-b-exp (x y z)
  (:documentation
   "Documentation example for defgeneric @b(m-b-exp)
================================================================================"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setf-Generics

(defgeneric (setf m-c-exp) (val obj)
  (:documentation
   "Documentation example for defgeneric @b((setf m-c-exp))
================================================================================"))

(defgeneric (setf m-a-exp) (val obj)
  (:documentation
   "Documentation example for defgeneric @b((setf m-a-exp))
================================================================================"))

(defgeneric (setf m-b-exp) (val obj)
  (:documentation
   "Documentation example for defgeneric @b((setf m-b-exp))
================================================================================"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Methods

(defmethod m-c-exp ((x <c-a-int>) (y <c-b-int>) (z <c-с-exp>))
  "Documentation example for method @b(m-c-exp (x <c-a-int>) (y <c-b-int>) (z <c-с-exp>))
================================================================================"
  t)


(defmethod m-b-exp ((x <c-a-int>) (y <c-b-int>) (z <c-с-exp>))
  "Documentation example for method @b(m-b-exp (x <c-a-int>) (y <c-b-int>) (z <c-с-exp>))
================================================================================"
  t)

(defmethod m-a-exp ((x <c-a-int>) (y <c-b-int>) (z <c-с-exp>))
  "Documentation example for method @b(m-a-exp ((x <c-a-int>) (y <c-b-int>) (z <c-с-exp>)))
================================================================================"
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setf-Methods

(defmethod (setf m-c-exp) (val (a <c-a-int>))
  "Documentation example for defgeneric @b((setf m-c-exp) (val (a <c-a-int>)))
================================================================================"
  (setf (<c-a-int>-a a) val)
  a)

(defmethod (setf m-c-exp) ((val number) (a <c-a-int>))
    "Documentation example for defgeneric @b((setf m-c-exp) ((val number) (a <c-a-int>)))
================================================================================"
  (setf (<c-a-int>-a a) (1+ val))
  a)

(defmethod (setf m-c-exp) (val (b <c-b-int>))
    "Documentation example for defgeneric @b((setf m-c-exp) (val (b <c-b-int>)))
================================================================================"
  (setf (<c-b-int>-b b) val)
  b)

(defmethod (setf m-c-exp) ((val number) (b <c-b-int>))
    "Documentation example for defgeneric @b((setf m-c-exp) ((val number) (b <c-b-int>)))
================================================================================"
  (setf (<c-b-int>-b b) (1+ val))
  b)

;;;;;;;;;;;;;;;;;;;;

(defmethod (setf m-a-exp) (val (a <c-a-int>))
  "Documentation example for defgeneric @b((setf m-a-exp) (val (a <c-a-int>)))
================================================================================"
  (setf (<c-a-int>-a a) val)
  a)

(defmethod (setf m-a-exp) ((val number) (a <c-a-int>))
    "Documentation example for defgeneric @b((setf m-a-exp) ((val number) (a <c-a-int>)))
================================================================================"
  (setf (<c-a-int>-a a) (1+ val))
  a)

(defmethod (setf m-a-exp) (val (b <c-b-int>))
    "Documentation example for defgeneric @b((setf m-a-exp) (val (b <c-b-int>)))
================================================================================"
  (setf (<c-b-int>-b b) val)
  b)

(defmethod (setf m-a-exp) ((val number) (b <c-b-int>))
    "Documentation example for defgeneric @b((setf m-a-exp) ((val number) (b <c-b-int>)))
================================================================================"
  (setf (<c-b-int>-b b) (1+ val))
  b)
 
;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Methods whit modifiers

#+nil
(defmethod m-b-exp :after ((x <c-a-int>) (y <c-b-int>) (z <c-с-exp>))
  "Documentation example for defmethod m-b-exp :after (x <c-a-int>) (y <c-b-int>) (z <c-с-exp>)
================================================================================"
  t)

#+nil
(defmethod m-b-exp :before ((x <c-a-int>) (y <c-b-int>) (z <c-с-exp>))
  "Documentation example for defmethod m-b-exp :before (x <c-a-int>) (y <c-b-int>) (z <c-с-exp>)
================================================================================"
  t)

#+nil
(defmethod m-b-exp :around ((x <c-a-int>) (y <c-b-int>) (z <c-с-exp>))
  "Documentation example for defmethod m-b-exp :before (x <c-a-int>) (y <c-b-int>) (z <c-с-exp>)
================================================================================"
  t)



