;;;; ./mnas-package/src/make/make-doc.lisp
(in-package :mnas-package/make)

(defmacro make-doc (obj-name obj-type doc-string)
  `(setf (documentation ,obj-name ,obj-type)
         ,doc-string))

(defun find-slot (slot-name class)
  (find slot-name (closer-mop:class-direct-slots (find-class class))
        :key #'closer-mop:slot-definition-name))

(make-doc
  #'MNAS-PACKAGE/MAKE:CALL-GRAPH 'function
  "@b(Описание:) функция @b(call-graph) возвращает граф вызовов пакета
 @b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (mnas-package:make-call-graph :mnas-package)
@end(code)")

(make-doc
  #'MNAS-PACKAGE/MAKE:GENERIC-GRAPH 'function
  "@b(Описание:) функция @b(generic-graph) возвращает граф параметров
 обобщенной функций. 

 Данный граф должен быть трехуровневым:
@begin(list)
 @item(первый уровень - обобщенная функция;)
 @item(второй - номер по порядку для обязательного параметра и его имя; )
 @item(третий - тип обязательного параметра.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)

@end(code)")

(make-doc
  #'MNAS-PACKAGE/MAKE:SYSTEM-GRAPH 'function
  "@b(Описание:) функция @b(system-graph) возвращает граф систем, от
которых зависит система @b(system).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (mnas-package/make:system-graph :mnas-package)
@end(code)")

(make-doc
  #'MNAS-PACKAGE/MAKE:CLASS-SLOT-GRAPH 'function
  "@b(Описание:) функция @b(class-slot-graph) создает граф слотов класса
с именем @b(class-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (require :temperature-fild)
 (mnas-graph:view-graph (class-slot-graph (find-class 'temperature-fild/sector:<sector>)))
@end(code)")

(make-doc
  #'MNAS-PACKAGE/MAKE:SYMBOL-GRAPH 'function
  "@b(Описание:) функция @b(symbol-graph) строит граф использования методпми и функциями 
внешних символов.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (symbol-graph :mnas-string)
@end(code)
")

(make-doc
  #'MNAS-PACKAGE/MAKE:CLASS-GRAPH 'function
  "@b(Описание:) функция @b(class-graph) создает граф наследования
 классов.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (class-graph :mnas-package )
@end(code)")
