;;;; ./mnas-package/src/obj/obj-doc.lisp
(in-package :mnas-package/obj)

(defmacro make-doc (obj-name obj-type doc-string)
  `(setf (documentation ,obj-name ,obj-type)
         ,doc-string))

(defun find-slot (slot-name class)
  (find slot-name
        (sb-mop:class-direct-slots  (find-class class))
        :key #'sb-mop:slot-definition-name))



(make-doc
  #'MNAS-PACKAGE/OBJ:OBJ-NAME-STRING 'function
  "@b(Описание:) обобщенная функция @b(obj-name-string)
возвращает строку, представляую имя объекта obj.")

(make-doc
  #'MNAS-PACKAGE/OBJ:OBJ-NAME 'function
  "@b(Описание:) обобщенная функция @b(obj-name)
возвращает символ, представляющий имя объекта obj.")

(make-doc
  #'MNAS-PACKAGE/OBJ:OBJ-PACKAGE-STRING 'function
  "@b(Описание:) обобщенная функция @b(obj-package-string)
возвращает строку, представляющую имя пакета, в котором определен объект @b(obj).")

(make-doc
  #'MNAS-PACKAGE/OBJ:OBJ-PACKAGE 'function
  "@b(Описание:) обобщенная функция @b(obj-package)
возвращает пакет, в котором определен объект obj.")

(make-doc
  (find-method #'MNAS-PACKAGE/OBJ:OBJ-PACKAGE NIL '(SYMBOL))
  t
  "@b(Описание:) метод @b(obj-name) возвращает символ,
представляющий имя символа.")

(make-doc
  (find-method #'MNAS-PACKAGE/OBJ:OBJ-PACKAGE NIL '(FUNCTION))
  t
  "@b(Описание:) метод @b(obj-package) возвращает символ,
представляющий имя функции.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (obj-package (second (package-functions :mnas-package)))
@end(code)")

(make-doc
  (find-method #'MNAS-PACKAGE/OBJ:OBJ-PACKAGE NIL '(STANDARD-GENERIC-FUNCTION))
  t
  "@b(Описание:) метод @b(obj-package) возвращает символ,
представляющий имя обобщенной функции.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (require :dxf)
 (obj-package (first (package-generics :dxf)))
@end(code)
")

(make-doc
  (find-method #'MNAS-PACKAGE/OBJ:OBJ-PACKAGE NIL '(METHOD))
  t
  "@b(Описание:) метод @b(obj-package) возвращает символ,
представляющий имя метода.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (require :dxf)
 (obj-package (second (closer-mop:generic-function-methods (first (package-generics :dxf)))))
@end(code)")

(make-doc
  (find-method #'MNAS-PACKAGE/OBJ:OBJ-PACKAGE NIL '(CLASS))
  t
  "@b(Описание:) метод @b(obj-package) возвращает символ,
представляющий имя класса.

 @b(Пример использования:)
@begin[lang=lisp](code)
  (obj-package (first (package-classes :dxf :internal t)))
@end(code)")

(make-doc
  (find-method #'MNAS-PACKAGE/OBJ:OBJ-PACKAGE NIL '(PACKAGE))
  t
  "@b(Описание:) метод @b(obj-package) возвращает символ,
представляющий имя класса.

 @b(Пример использования:)
@begin[lang=lisp](code)
  (require :dxf)
  (obj-package (find-package :dxf))
@end(code)")

(make-doc
  (find-method #'MNAS-PACKAGE/OBJ:OBJ-PACKAGE NIL '(SB-MOP:SLOT-DEFINITION))
  t
  "@b(Описание:) метод @b(obj-package) возвращает символ,
представляющий имя определение слота.
")

(make-doc
  (find-method #'MNAS-PACKAGE/OBJ:OBJ-PACKAGE-STRING NIL '(T))
  t
  "@b(Описание:) метод @b(obj-package-string)
возвращает строку, представляющую имя пакета, в котором определен объект @b(obj).

 @b(Пример использования:)
@begin[lang=lisp](code)
  (require :dxf)
  (obj-package-string (find-package :dxf))
@end(code)")

(make-doc
  (find-method #'MNAS-PACKAGE/OBJ:OBJ-NAME NIL '(SYMBOL))
  t
  "@b(Описание:) метод @b(obj-name) возвращает символ,
представляющий имя символа.")

(make-doc
  (find-method #'MNAS-PACKAGE/OBJ:OBJ-NAME NIL '(FUNCTION))
  t
  "@b(Описание:) метод @b(obj-name) возвращает символ,
представляющий имя функции.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (obj-name (second (package-functions :mnas-package)))
@end(code)")

(make-doc
  (find-method #'MNAS-PACKAGE/OBJ:OBJ-NAME NIL '(STANDARD-GENERIC-FUNCTION))
  t
  "@b(Описание:) метод @b(obj-name) возвращает символ,
представляющий имя обобщенной функции.

 Вторым значением возвращается:
@begin(list)
 @item(:generic - для обыкновенной обобщенной функции;)
 @item(:setf-generic - для setf обобщенной функции.)
@end(list)")

(make-doc
  (find-method #'MNAS-PACKAGE/OBJ:OBJ-NAME NIL '(METHOD))
  t
  "@b(Описание:) метод @b(obj-name) возвращает символ,
представляющий имя метода.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (require :dxf)
 (obj-name (second (closer-mop:generic-function-methods (first (package-generics :dxf)))))
@end(code)")

(make-doc
  (find-method #'MNAS-PACKAGE/OBJ:OBJ-NAME NIL '(CLASS))
  t
  "@b(Описание:) метод @b(obj-name) возвращает символ,
представляющий имя класса.

 @b(Пример использования:)
@begin[lang=lisp](code)
  (obj-name (first (package-classes :dxf :internal t)))
@end(code)")

(make-doc
  (find-method #'MNAS-PACKAGE/OBJ:OBJ-NAME NIL '(PACKAGE))
  t
  "@b(Описание:) метод @b(obj-name) возвращает символ,
представляющий имя класса.

 @b(Пример использования:)
@begin[lang=lisp](code)
  (obj-name (find-package :dxf))
@end(code)")

(make-doc
  (find-method #'MNAS-PACKAGE/OBJ:OBJ-NAME NIL '(SB-MOP:SLOT-DEFINITION))
  t
  "@b(Описание:) метод @b(obj-name) возвращает символ,
представляющий имя определеия слота.")

(make-doc
  (find-method #'MNAS-PACKAGE/OBJ:OBJ-NAME-STRING NIL '(T))
  t
  "@b(Описание:) метод @b(obj-name) возвращает строку,
представляющую имя функции.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (progn
   (require :dxf)
   (obj-name-string (second (package-functions :mnas-package)))
   (obj-name-string (second (package-functions :dxf)))
   (obj-name (first (package-generics :dxf)))
   (obj-name (second (closer-mop:generic-function-methods (first (package-generics :dxf))))))
@end(code)")
