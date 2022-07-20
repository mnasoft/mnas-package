;;;; ./src/docs/docs.lisp

(defpackage #:mnas-package/doc
  (:use #:cl ) ;; :mnas-package/pkg
  (:nicknames "MPKG/DOC")
  (:export include-macro
           find-slot)
  (:export make-classes
           make-slots
           make-macroses
           make-generics
           make-setf-generics           
           make-functions
           make-setf-functions           
           make-variables
           make-methods
           make-setf-methods)
  (:documentation "Пакет @b(mnas-package/doc) содержит функции предназначенные для:

@begin(list)
 @item(извлечения строк документации;)
 @item(генерирования кода документации.)
@end(list)
"))

(in-package :mnas-package/doc)

(defmacro make-doc (obj-name obj-type doc-string)
  `(setf (documentation ,obj-name ,obj-type)
         ,doc-string))

(defun find-slot (slot-name class)
  (find slot-name
        (sb-mop:class-direct-slots  (find-class class))
        :key #'sb-mop:slot-definition-name))

(defun include-macro ()
  (format t 
"(defmacro make-doc (obj-name obj-type doc-string)
  `(setf (documentation ,obj-name ,obj-type)
         ,doc-string))

(defun find-slot (slot-name class)
  (find slot-name
        (sb-mop:class-direct-slots  (find-class class))
        :key #'sb-mop:slot-definition-name))
~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-macroses (package &key (external t) (internal nil) (inherited nil))
  "(make-macroses :mnas-package/example)"
  (loop :for i :in (mnas-package/pkg:package-macroses package :external external :internal internal :inherited inherited)
        :do
           (format t "~%
(make-doc
  (macro-function '~S) t
  ~S)"
                   (mnas-package/obj:obj-name i)
                   (documentation i t))))


(sb-mop:class-direct-slots
 (second (mnas-package/pkg:package-classes :mnas-package/example)))





(make-doc
 (find-slot 'MNAS-PACKAGE/EXAMPLE::B 'mnas-package/example::<c-b-int>)
 t
 "SLOT COOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOL
Slot @b(b) for something."
 )

(documentation
  (find-slot 'MNAS-PACKAGE/EXAMPLE::B 'mnas-package/example::<c-b-int>)
 t)

;;;;;;;;;;;;;;;;;;;;

(defun make-slots (package &key (external t) (internal nil) (inherited nil))
  "(make-slots :mnas-package/example)"
  (loop :for class :in (mnas-package/pkg:package-classes package :external external :internal internal :inherited inherited) :do
    (loop :for slot :in (sb-mop:class-direct-slots class) :do
      (format t "~%
(make-doc
 (find-slot '~S '~S)
 t
 ~S)"

              (sb-mop:slot-definition-name slot)
              (mnas-package/obj:obj-name class)
              (documentation slot t)))))

;;;;;;;;;;;;;;;;;;;;

(defun make-classes (package &key (external t) (internal nil) (inherited nil))
  "(make-classes :mnas-package/example)"
  (loop :for i :in (mnas-package/pkg:package-classes package :external external :internal internal :inherited inherited)
        :do
           (format t "~%
(make-doc
  (find-class '~S) t
  ~S)"
                   (mnas-package/obj:obj-name i)
                   (documentation i t))))

(defun make-variables (package &key (external t) (internal nil) (inherited nil))
  "(make-variables :mnas-package/example)"
  (loop :for i :in (mnas-package/pkg:package-variables package :external external :internal internal :inherited inherited)
        :do
           (format t "~%
(make-doc
  '~S 'variable
  ~S)"
                   (mnas-package/obj:obj-name i)
                   (documentation i 'variable))))

(defun make-generics (package &key (external t) (internal nil) (inherited nil))
  "(make-generics :mnas-package/example)"
  (loop :for i :in (mnas-package/pkg:package-generics package :external external :internal internal :inherited inherited)
        :do
           (format t "~%
(make-doc
  #'~S 'function
  ~S)"
                   (mnas-package/obj:obj-name i)
                   (documentation i t))))

(defun make-setf-generics (package &key (external t) (internal nil) (inherited nil))
  "(make-setf-generics :mnas-package/example)"
  (loop :for i :in (mnas-package/pkg:package-setf-functions package :external external :internal internal :inherited inherited)
        :do
           (format t "~%
(make-doc
  #'(setf ~S)
  'function
  ~S)"
                   (mnas-package/obj:obj-name i)
                   (documentation i t))))

(defun make-functions (package &key (external t) (internal nil) (inherited nil))
  "(make-functions :mnas-package/example)"
  (loop :for i :in (mnas-package/pkg:package-functions package :external external :internal internal :inherited inherited)
        :do
           (format t "~%
(make-doc
  #'~S 'function
  ~S)"
                   (mnas-package/obj:obj-name i)
                   (documentation i t))))

(defun make-setf-functions (package &key (external t) (internal nil) (inherited nil))
  "(make-setf-functions :mnas-package/example)"
  (loop :for i :in (mnas-package/pkg:package-setf-functions package :external external :internal internal :inherited inherited)
        :do
           (format t "~%
(make-doc
  #'(setf ~S)
  'function
  ~S)"
                   (mnas-package/obj:obj-name i)
                   (documentation i t))))


(defun make-setf-methods (package &key (external t) (internal nil) (inherited nil))
  "(make-setf-methods :mnas-package/example)"
  (loop :for i :in (mnas-package/pkg:package-setf-methods package :external external :internal internal :inherited inherited)
        :do
           (format t "~%
(make-doc
  (find-method #'~S ~S '~S)
  t
  ~S)"
                   (sb-mop:generic-function-name (sb-mop:method-generic-function i))
                   (method-qualifiers i)
                   (mapcar #' class-name(sb-mop:method-specializers i))
                   (documentation i t))))

(defun make-methods (package &key (external t) (internal nil) (inherited nil))
  "(make-methods :mnas-package/example)"
  (loop :for i :in (mnas-package/pkg:package-methods package :external external :internal internal :inherited inherited)
        :do
           (format t "~%
(make-doc
  (find-method #'~S ~S '~S)
  t
  ~S)"
                   (sb-mop:generic-function-name (sb-mop:method-generic-function i))
                   (method-qualifiers i)
                   (mapcar #' class-name(sb-mop:method-specializers i))
                   (documentation i t))))

(defun make-all (package &key (external t) (internal nil) (inherited nil))
  "(make-all :mnas-package/example :internal t)"
  (include-macro)
  (map 'nil
       #'(lambda (fname)
           (funcall fname package :external external :internal internal :inherited inherited))
       `(,#'make-variables
         ,#'make-macroses
         ,#'make-functions ,#'make-setf-functions
         ,#'make-generics  ,#'make-setf-generics 
         ,#'make-classes 
         ,#'make-slots  
         ,#'make-methods  
         ,#'make-setf-methods))
  (format t "~%~%"))
