;;;; ./src/obj/obj.lisp
 
(defpackage #:mnas-package/obj
  (:use #:cl )
  (:nicknames "MPKG/OBJ")
  (:export obj-package
           obj-package-string
           obj-name
           obj-name-string)
  (:documentation
   "Пакет :mnas-package/obj определяет обобщенные функции и методы,
полученные на их основе для получения:
@begin(list)
 @item(имени (символа), связанного с объектом;)
 @item(строки, представляующей имя (символ) объекта;) 
 @item(пакета, в котором определен объект;)
 @item(строки, представляующей пакет, в котором определен объект.)
@end(list)
"
   ))

(in-package #:mnas-package/obj)

(defgeneric obj-name (obj))

(defgeneric obj-name-string (obj))

(defgeneric obj-package (obj))

(defgeneric obj-package-string (obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod obj-name ((symbol symbol))
  "@b(Описание:) метод @b(obj-name) возвращает символ,
представляющий имя символа."
  (values symbol :symbol))

(defmethod obj-name ((function function))
  (let ((name (nth-value 2 (function-lambda-expression function))))
    (cond
      ((symbolp name) (values name :function))
      ((and (listp name)
            (= 2 (length name))
            (eq 'setf (first name)))
       (values (second name) :setf-function))
      ((and (listp name) (= 2 (length name)) (eq 'macro-function (first name)))
       (values (second name) :macro-function))
      ((and (listp name) (= 2 (length name)) (eq :macro (first name)))
       (values (second name) :macro))
      (t (error "Неожиданное значение аргумента name=~S" name)))))

(defmethod obj-name ((generic standard-generic-function))
#+nil
  " @b(Пример использования:)
@begin[lang=lisp](code)
;;;; Доработать (obj-name mnas-package/example:))
@end(code)"
    (let ((name (closer-mop:generic-function-name generic)))
    (cond
      ((symbolp name) (values name :generic))
      ((and (listp name)
            (= 2 (length name))
            (eq 'setf (first name)))
       (values (second name) :setf-generic))
      (t (error "Неожиданное значение аргумента name=~S" name)))))

(defmethod obj-name ((method method))
  (let ((name (closer-mop:generic-function-name
               (closer-mop:method-generic-function method))))
    (cond
      ((symbolp name) (values name :method))
      ((and (listp name)
            (= 2 (length name))
            (eq 'setf (first name)))
       (values (second name) :setf-method))
      (t (error "Неожиданное значение аргумента name=~S" name)))
    ))

(defmethod obj-name ((class class))
  (values (class-name class) :class))

(defmethod obj-name ((package package))
  (values (package-name package) :package))

(defmethod obj-name ((slot-definition closer-mop:slot-definition))
  (closer-mop:slot-definition-name slot-definition))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod obj-name-string (obj)
  (format nil "~s" (obj-name obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod obj-package ((symbol symbol))
  (symbol-package symbol))

(defmethod obj-package ((function function))
  (symbol-package (obj-name function)))

(defmethod obj-package ((generic standard-generic-function))
  (symbol-package (obj-name generic)))

(defmethod obj-package ((method method))
  (symbol-package (obj-name method)))

(defmethod obj-package ((class class))
  (symbol-package (obj-name class)))

(defmethod obj-package ((package package))
  package)

(defmethod obj-package ((slot-definition closer-mop:slot-definition))
  (symbol-package (obj-name slot-definition)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod obj-package-string (obj)
  (package-name (obj-package obj)))

