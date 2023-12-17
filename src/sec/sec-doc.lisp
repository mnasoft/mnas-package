;;;; ./mnas-package/src/sec/sec-doc.lisp
(in-package :mnas-package/sec)

(defmacro make-doc (obj-name obj-type doc-string)
  `(setf (documentation ,obj-name ,obj-type)
         ,doc-string))

(defun find-slot (slot-name class)
  (find slot-name (closer-mop:class-direct-slots (find-class class))
        :key #'closer-mop:slot-definition-name))

(make-doc
  'MNAS-PACKAGE/SEC:*MIN-DOC-LENGTH* 'variable
  "@b(Описание:) переменная (*min-doc-length*) устанавливает длину
строки, свыше которой осуществляется вставка инструкций на
генерирование документации для соответствующего объекта.")

(make-doc
  (macro-function 'MNAS-PACKAGE/SEC:WITH-DOWNCASE) t
  "Not yet documented")

(make-doc
  (macro-function 'MNAS-PACKAGE/SEC:WITH-PACKAGE) t
  "Not yet documented")

(make-doc
  #'MNAS-PACKAGE/SEC:SECTION-SETF-GENERICS 'function
  "@b(Описание:) функция @b(section-setf-generics) выводит в поток stream
секцию с документацией в формате codex, содержащую обощенные setf-функции из пакета package-name.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (section-setf-generics :mnas-package/example :external t :internal t :sort nil) 
 (section-setf-generics :mnas-package/example :external t :internal t :sort t) 
@end(code)")

(make-doc
  #'MNAS-PACKAGE/SEC:SECTION-MACROSES 'function
  "@b(Описание:) функция @b(section-macroses) выводит в поток stream
секцию с документацией в формате codex, содержащую макросы из пакета
package-name.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (section-macroses :mnas-package/example :external t :internal t :sort t)
 (section-macroses :mnas-package/example :external t :internal t :sort t :min-doc-length 10) 
@end(code)")

(make-doc
  #'MNAS-PACKAGE/SEC:SECTION-FUNCTIONS 'function
  "@b(Описание:) функция @b(section-functions) выводит в поток stream
секцию с документацией в формате codex, содержащую функции из пакета
package-name.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (section-functions :math/stat :external t :internal t :sort t) 
@end(code)")

(make-doc
  #'MNAS-PACKAGE/SEC:SECTION-SYSTEM 'function
  "@b(Описание:) функция @b(section-system)

 @b(Пример использования:)
@begin[lang=lisp](code)

@end(code)")

(make-doc
  #'MNAS-PACKAGE/SEC:SECTION-VARIABLES 'function
  "@b(Описание:) функция @b(section-classes) выводит в поток @b(stream)
секцию с документацией в формате codex, содержащую переменные из пакета @b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (require :dxf)
 (section-classes :dxf :internal t)
 @end(code)")

(make-doc
  #'MNAS-PACKAGE/SEC:SECTION-METHODS 'function
  "@b(Описание:) функция @b(section-methods) выводит в поток @b(stream)
секцию с документацией в формате codex, содержащую методы из пакета @b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (section-methods :mnas-package/example :internal t)
 @end(code)")

(make-doc
  #'MNAS-PACKAGE/SEC:SECTION-SETF-FUNCTIONS 'function
  "@b(Описание:) функция @b(section-setf-functions) выводит в поток stream
секцию с документацией в формате codex, содержащую setf-функции из пакета package-name.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (section-setf-functions :mnas-package/example :external t :internal t :sort t) 
 (section-setf-functions :mnas-package/example :external t :internal t :sort nil) 
@end(code)")

(make-doc
  #'MNAS-PACKAGE/SEC:SECTION-CLASSES 'function
  "@b(Описание:) функция @b(section-classes) выводит в поток @b(stream)
секцию с документацией в формате codex, содержащую классы из пакета @b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (require :dxf)
 (section-classes :dxf :internal t)
 @end(code)
")

(make-doc
 #'MNAS-PACKAGE/SEC:SECTION-PACKAGE 'function
 "@b(Описание:) функция @b(section-package)

 @b(Пример использования:)
@begin[lang=lisp](code)
(section-package :mnas-package)
@end(code)")

(make-doc
  #'MNAS-PACKAGE/SEC:SECTION-SETF-METHODS 'function
  "@b(Описание:) функция @b(section-setf-methods) выводит в поток
@b(stream) секцию с документацией в формате codex, содержащую
setf-методы из пакета @b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (section-setf-methods :mnas-package/example :internal t :sort nil)
 @end(code)")

(make-doc
  #'MNAS-PACKAGE/SEC:SECTION-GENERICS 'function
  "@b(Описание:) функция @b(section-generics) выводит в поток stream
секцию с документацией в формате codex, содержащую обобщенные функции из пакета @b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (section-generics :mnas-package/example :internal t :sort t)
@end(code)")

(make-doc
  #'MNAS-PACKAGE/SEC:INSERT-CODEX-DOC 'function
  "@b(Описание:) обобщенная функция @b(insert-codex-doc)
выводит в поток @b(stream) код для вставки документации, относящейся к 
объекту @b(obj). Документация объекта выводится в поток только если
ее длина превышает @b(min-doc-length). 

 Возвращает: 
@begin(list)
 @item(@b(t) - если документация была выведена в поток;)
 @item(@b(nil) - если документация не была выведена в поток.)
@end(list)")

(make-doc
  (find-method #'MNAS-PACKAGE/SEC:INSERT-CODEX-DOC NIL '(SYMBOL))
  t
  " @b(Пример использования:) 
@begin[lang=lisp](code)
 (mapcar #'insert-codex-doc (mpkg/pkg:package-variables :mnas-package/example :internal t))
@end(code)")

(make-doc
  (find-method #'MNAS-PACKAGE/SEC:INSERT-CODEX-DOC NIL '(FUNCTION))
  t
  " @b(Пример использования:)
@begin[lang=lisp](code)

@end(code)")

(make-doc
  (find-method #'MNAS-PACKAGE/SEC:INSERT-CODEX-DOC NIL '(STANDARD-GENERIC-FUNCTION))
  t
  NIL)

(make-doc
  (find-method #'MNAS-PACKAGE/SEC:INSERT-CODEX-DOC NIL '(CLASS))
  t
  NIL)

(make-doc
  (find-method #'MNAS-PACKAGE/SEC:INSERT-CODEX-DOC NIL '(METHOD))
  t
  "(insert-codex-doc (find-package :mpkg))")

(make-doc
  (find-method #'MNAS-PACKAGE/SEC:INSERT-CODEX-DOC NIL '(PACKAGE))
  t
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (insert-codex-doc (find-package :mpkg))
@end(code) ")

