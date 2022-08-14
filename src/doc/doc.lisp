;;;; ./src/docs/docs.lisp

(defpackage #:mnas-package/doc
  (:use #:cl ) ;; :mnas-package/pkg
  (:nicknames "MPKG/DOC")
  (:export include-in-package
           include-macro
           find-slot)
  (:export make-packages
           make-classes
           make-slots
           make-macroses
           make-generics
           make-setf-generics           
           make-functions
           make-setf-functions           
           make-variables
           make-methods
           make-setf-methods)
  (:export make-all)
  (:documentation
   "Пакет @b(mnas-package/doc) содержит функции предназначенные для:

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

(defun include-macro (&key (stream t))
  (format stream 
"(defmacro make-doc (obj-name obj-type doc-string)
  `(setf (documentation ,obj-name ,obj-type)
         ,doc-string))

(defun find-slot (slot-name class)
  (find slot-name
        (sb-mop:class-direct-slots  (find-class class))
        :key #'sb-mop:slot-definition-name))
~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-macroses (package &key (stream t) (external t) (internal nil) (inherited nil))
  "(make-macroses :mnas-package/example)"
  (loop :for i :in (mnas-package/pkg:package-macroses package :external external :internal internal :inherited inherited)
        :do
           (format stream "~%
(make-doc
  (macro-function '~S) t
  ~S)"
                   (mnas-package/obj:obj-name i)
                   (documentation i t))))

;;;;;;;;;;;;;;;;;;;;

(defun make-slots (package &key (stream t) (external t) (internal nil) (inherited nil))
  "(make-slots :mnas-package/example)"
  (loop :for class :in (mnas-package/pkg:package-classes package :external external :internal internal :inherited inherited) :do
    (loop :for slot :in (sb-mop:class-direct-slots class) :do
      (when (stringp (documentation slot t))
        (format stream "~2%(make-doc~%  (find-slot '~S '~S)~% t~%  ~S)"
                (sb-mop:slot-definition-name slot)
                (mnas-package/obj:obj-name class)
                (documentation slot t))))))

;;;;;;;;;;;;;;;;;;;;

(defun include-in-package (package &key (stream t) &aux (pkg (find-package package)))
    (format stream "~%(in-package #:~A)~2%" (mnas-package/obj:obj-name pkg)))

(defun make-packages (package &key (stream t) &aux (pkg  (find-package package)))
  "(make-packages :mnas-package/example)"
  (when (stringp (documentation pkg t))
    (format stream "~2%(make-doc~%  (find-package '~A) t~%  ~S)"
            (mnas-package/obj:obj-name pkg)
            (documentation pkg t))))

(defun make-classes (package &key (stream t) (external t) (internal nil) (inherited nil))
  "(make-classes :mnas-package/example)"
  (loop :for i :in (mnas-package/pkg:package-classes package :external external :internal internal :inherited inherited) :do
    (when (stringp (documentation i t))
      (format stream "~2%(make-doc~%  (find-class '~S) t~%  ~S)"
              (mnas-package/obj:obj-name i)
              (documentation i t)))))

(defun make-variables (package &key (stream t) (external t) (internal nil) (inherited nil))
  "(make-variables :mnas-package/example)"
  (loop :for i :in (mnas-package/pkg:package-variables package :external external :internal internal :inherited inherited) :do
    (when (stringp (documentation i 'variable))
      (format stream "~2%(make-doc~%  '~S 'variable~%  ~S)"
              (mnas-package/obj:obj-name i)
              (documentation i 'variable)))))

(defun make-generics (package &key (stream t) (external t) (internal nil) (inherited nil))
  "(make-generics :mnas-package/example)"
  (loop :for i :in (mnas-package/pkg:package-generics package :external external :internal internal :inherited inherited) :do
    (when (stringp (documentation i t))
      (format stream "~2%(make-doc~%  #'~S 'function~%  ~S)"
              (mnas-package/obj:obj-name i)
              (documentation i t)))))

(defun make-setf-generics (package &key (stream t) (external t) (internal nil) (inherited nil))
  "(make-setf-generics :mnas-package/example)"
  (loop :for i :in (mnas-package/pkg:package-setf-generics package :external external :internal internal :inherited inherited)
        :do
           (when (stringp (documentation i t))
             (format stream "~2%(make-doc~%  #'(setf ~S)~%  'function~%  ~S)"
                     (mnas-package/obj:obj-name i)
                     (documentation i t)))))

(defun make-functions (package &key (stream t) (external t) (internal nil) (inherited nil))
  "(make-functions :mnas-package/example)"
  (loop :for i :in (mnas-package/pkg:package-functions package :external external :internal internal :inherited inherited) :do
    (when (stringp (documentation i t))
      (format stream "~2%(make-doc~%  #'~S 'function~%  ~S)"
              (mnas-package/obj:obj-name i)
              (documentation i t)))))

(defun make-setf-functions (package &key (stream t) (external t) (internal nil) (inherited nil))
  "(make-setf-functions :mnas-package/example)"
  (loop :for i :in (mnas-package/pkg:package-setf-functions package :external external :internal internal :inherited inherited) :do
    (when (stringp (documentation i t))
      (format stream "~2%(make-doc~%  #'(setf ~S)~%  'function~%  ~S)"
              (mnas-package/obj:obj-name i)
              (documentation i t)))))

(defun make-setf-methods (package &key (stream t) (external t) (internal nil) (inherited nil))
  "(make-setf-methods :mnas-package/example)"
  (loop :for i :in (mnas-package/pkg:package-setf-methods package :external external :internal internal :inherited inherited)
        :do
           (when (stringp (documentation i t))
             (format stream "~2%(make-doc~%  (find-method #'~S ~S '~S)~%  t~%  ~S)"
                     (sb-mop:generic-function-name (sb-mop:method-generic-function i))
                     (method-qualifiers i)
                     (mapcar #' class-name(sb-mop:method-specializers i))
                     (documentation i t)))))

(defun make-methods (package &key (stream t) (external t) (internal nil) (inherited nil))
  "(make-methods :mnas-package/example)"
  (loop :for i :in (mnas-package/pkg:package-methods package :external external :internal internal :inherited inherited) :do
    (when (stringp (documentation i t))
      (format stream "~2%(make-doc~%  (find-method #'~S ~S '~S)~%  t~%  ~S)"
              (sb-mop:generic-function-name (sb-mop:method-generic-function i))
              (method-qualifiers i)
              (mapcar #' class-name(sb-mop:method-specializers i))
              (documentation i t)))))

(defun make-all (package &key (stream t) (external t) (internal t) (inherited nil))
  "(make-all :mnas-package/example)"
  (include-in-package package :stream stream)
  (include-macro :stream stream)
  (make-packages package :stream stream)
  (map 'nil
       #'(lambda (fname)
           (funcall fname package :stream stream :external external :internal internal :inherited inherited))
       `(,#'make-variables
         ,#'make-macroses
         ,#'make-functions ,#'make-setf-functions
         ,#'make-generics  ,#'make-setf-generics 
         ,#'make-classes 
         ,#'make-slots  
         ,#'make-methods  
         ,#'make-setf-methods))
  (format stream "~2%"))
