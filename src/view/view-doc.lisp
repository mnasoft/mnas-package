;;;; ./mnas-package/src/view/view-doc.lisp

(in-package :mnas-package/view)

(defmacro make-doc (obj-name obj-type doc-string)
  `(setf (documentation ,obj-name ,obj-type)
         ,doc-string))

(defun find-slot (slot-name class)
  (find slot-name (closer-mop:class-direct-slots (find-class class))
        :key #'closer-mop:slot-definition-name))

(make-doc
  #'MNAS-PACKAGE/VIEW:CALL-GRAPH 'function
  "@b(Описание:) функция @b(call-graph) выполняет визуализацию графа
 вызовов пакета @b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (call-graph :mnas-package)
@end(code)")

(make-doc
  #'MNAS-PACKAGE/VIEW:GENERIC-GRAPH 'function
  "Not yet documented")

(make-doc
  #'MNAS-PACKAGE/VIEW:SYSTEM-GRAPH 'function
  "@b(Описание:) функция @b(system-graph) визуализирует граф систем, от
которых зависит система @b(system).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (mnas-package/view:system-graph :mnas-package :out-type \"png\" :viewer nil)
@end(code)")

(make-doc
  #'MNAS-PACKAGE/VIEW:CLASS-SLOT-GRAPH 'function
  "@b(Описание:) функция @b(class-slot-graph) - ...
")

(make-doc
  #'MNAS-PACKAGE/VIEW:SYMBOL-GRAPH 'function
  "@b(Описание:) функция @b(symbol-graph) отображает граф зависимостей глобальных символов.

 Позволяет ответить на вопрос: в какой функции используется тот или
 иной глобальный символ.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (view-symbol-graph :mnas-package)
@end(code)")

(make-doc
  #'MNAS-PACKAGE/VIEW:CLASS-GRAPH 'function
  "@b(Описание:) функция @b(class-graph) выводит визуальное представление
иерархии классов (графа наследования).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (mnas-package:mnas-package-demo-11)
@end(code)")
