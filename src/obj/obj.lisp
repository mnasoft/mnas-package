;;;; ./src/obj/obj.lisp
 
(defpackage #:mnas-package/obj
  (:use #:cl )
  (:nicknames "MPKG/OBJ")
  (:export obj-package
           obj-package-string
           obj-name
           obj-name-string)
  (:documentation
   "Система mnas-package предназначена для извлечения информации из asdf-систем.

 Извлеченная информация представляется в виде графов.

 Система позволяет построить следующие графы:
@begin(list)
 @item(зависимостей систем @image[src=./system-graph-mnas-package.gv.png]())
 @item(вызовов функций     @image[src=./call-graph-mnas-package.gv.png]())
 @item(использования символов функциями @image[src=./symbol-graph-mnas-package.gv.png]())
 @item(наследования классов  @image[src=./class-graph-mnas-package.gv.png]())
@end(list)"
   ))

(in-package #:mnas-package/obj)

(defgeneric obj-name (obj)
  (:documentation "@b(Описание:) обобщенная функция @b(obj-name)
возвращает символ, представляющий имя объекта obj."))

(defgeneric obj-name-string (obj)
  (:documentation "@b(Описание:) обобщенная функция @b(obj-name-string)
возвращает символ, представляющий имя объекта obj."))

(defgeneric obj-package (obj)
  (:documentation "@b(Описание:) обобщенная функция @b(obj-package)
возвращает пакет, в котором определен объект obj."))

(defgeneric obj-package-string (obj)
  (:documentation "@b(Описание:) обобщенная функция @b(obj-package-string)
возвращает строку, представляющую имя пакета, в котором определен объект @b(obj)."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod obj-name ((symbol symbol))
  "@b(Описание:) метод @b(obj-name) возвращает символ,
представляющий имя функции."
  symbol)

(defmethod obj-name ((function function))
  "@b(Описание:) метод @b(obj-name) возвращает символ,
представляющий имя функции.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (obj-name (second (package-functions :mnas-package)))
@end(code)"
  (nth-value 2 (function-lambda-expression function)))

(defmethod obj-name ((generic standard-generic-function))
  "@b(Описание:) метод @b(obj-name) возвращает символ,
представляющий имя обобщенной функции.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (require :dxf)
 (obj-name (first (package-generics :dxf)))
@end(code)
"
  (sb-mop:generic-function-name generic))

(defmethod obj-name ((method method))
      "@b(Описание:) метод @b(obj-name) возвращает символ,
представляющий имя метода.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (require :dxf)
 (obj-name (second (sb-mop:generic-function-methods (first (package-generics :dxf)))))
@end(code)"
  (sb-mop:generic-function-name
   (sb-mop:method-generic-function method)))

(defmethod obj-name ((class class))
  "@b(Описание:) метод @b(obj-name) возвращает символ,
представляющий имя класса.

 @b(Пример использования:)
@begin[lang=lisp](code)
  (obj-name (first (package-classes :dxf :internal t)))
@end(code)"
  (class-name class))

(defmethod obj-name ((package package))
  "@b(Описание:) метод @b(obj-name) возвращает символ,
представляющий имя класса.

 @b(Пример использования:)
@begin[lang=lisp](code)
  (obj-name (find-package :dxf))
@end(code)"
  (package-name package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod obj-name-string (obj)
  "@b(Описание:) метод @b(obj-name) возвращает строку,
представляющую имя функции.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (progn
   (require :dxf)
   (obj-name-string (second (package-functions :mnas-package)))
   (obj-name-string (second (package-functions :dxf)))
   (obj-name (first (package-generics :dxf)))
   (obj-name (second (sb-mop:generic-function-methods (first (package-generics :dxf))))))
@end(code)"
  (format nil "~s" (obj-name obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod obj-package ((function function))
  "@b(Описание:) метод @b(obj-package) возвращает символ,
представляющий имя функции.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (obj-package (second (package-functions :mnas-package)))
@end(code)"
  (symbol-package (obj-name function)))

(defmethod obj-package ((generic standard-generic-function))
    "@b(Описание:) метод @b(obj-package) возвращает символ,
представляющий имя обобщенной функции.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (require :dxf)
 (obj-package (first (package-generics :dxf)))
@end(code)
"
  (symbol-package (obj-name generic)))

(defmethod obj-package ((method method))
      "@b(Описание:) метод @b(obj-package) возвращает символ,
представляющий имя метода.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (require :dxf)
 (obj-package (second (sb-mop:generic-function-methods (first (package-generics :dxf)))))
@end(code)"
  (symbol-package (obj-name method)))

(defmethod obj-package ((class class))
  "@b(Описание:) метод @b(obj-package) возвращает символ,
представляющий имя класса.

 @b(Пример использования:)
@begin[lang=lisp](code)
  (obj-package (first (package-classes :dxf :internal t)))
@end(code)"
  (symbol-package (obj-name class)))

(defmethod obj-package ((package package))
  "@b(Описание:) метод @b(obj-package) возвращает символ,
представляющий имя класса.

 @b(Пример использования:)
@begin[lang=lisp](code)
  (require :dxf)
  (obj-package (find-package :dxf))
@end(code)"
  package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod obj-package-string (obj)
    "@b(Описание:) метод @b(obj-package-string)
возвращает строку, представляющую имя пакета, в котором определен объект @b(obj).

 @b(Пример использования:)
@begin[lang=lisp](code)
  (require :dxf)
  (obj-package-string (find-package :dxf))
@end(code)"
  (package-name (obj-package obj)))

