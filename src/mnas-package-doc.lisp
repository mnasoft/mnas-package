;;;; ./mnas-package/src/mnas-package-doc.lisp
(in-package #:mnas-package)

(defmacro make-doc (obj-name obj-type doc-string)
  `(setf (documentation ,obj-name ,obj-type)
         ,doc-string))

(defun find-slot (slot-name class)
  (find slot-name
        (sb-mop:class-direct-slots  (find-class class))
        :key #'sb-mop:slot-definition-name))



(make-doc
  'MNAS-PACKAGE:*INTRANET-SERVER* 'variable
  "@b(Описание:) переменная @b(*intranet-server*) содержит путь к
  документации в локальной сети.")

(make-doc
  'MNAS-PACKAGE:*INTRANET-HOSTS* 'variable
  "@b(Описание:) параметр @b(*intranet-hosts*) содержит перечень
    хостов в доступом в корпоративную сеть.")

(make-doc
  'MNAS-PACKAGE:*INTERNET-HOSTS* 'variable
  "@b(Описание:) параметр @b(*internet-hosts*) содержит перечень
  хостов в доступом в интернет.")

(make-doc
  'MNAS-PACKAGE::+MAINFEST-LISP-TEMPLATE+ 'variable
  NIL)

(make-doc
  #'MNAS-PACKAGE:SUPER-CLASS-GRAPH 'function
  " @b(Описание:) метод @b(sub-class-graph) возвращает граф,
содержащий иерархию предков для класса @b(class).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (mnas-graph:view-graph (super-class-graph (find-class 'mnas-package/example:<c>)))
 (mnas-graph:view-graph (super-class-graph (find-class 'list)))
@end(code)
")

(make-doc
  #'MNAS-PACKAGE:RSYNC-DOC 'function
  "@b(Описание:) функция @b(rsync-doc) выполняет копирование
  документации на удаленный сервер.")

(make-doc
  #'MNAS-PACKAGE:MAKE-MAINFEST-LISP 'function
  NIL)

(make-doc
  #'MNAS-PACKAGE:MAKE-DOC-GENERICS 'function
  "@b(Описание:) функция @b(make-doc-methods) выводит в поток
@b(stream) раздел документации, подготовленной для вставки в scr-файл
системы документирования codex. Этот раздел содержит обобщенные
функции класса @b(class), имена которых начинаются с префикса
@b(prefix).

 @b(Пример использования:)
@begin[lang=lisp](code)
(make-doc-generics
 (find-package :mnas-package/example)
 (find-class 'mnas-package/example:<c-с-exp>)
 \"\")
@end(code)
->  @cl:with-package[name=\"MNAS-PACKAGE/EXAMPLE\"](
     @cl:doc(generic m-a-exp)
     @cl:doc(generic m-b-exp)
     @cl:doc(generic m-c-exp))
=> #<package \"MNAS-PACKAGE\">
")

(make-doc
  #'MNAS-PACKAGE:DOCUMENT 'function
  " @b(Описание:) функция @b(document) формирует scr-файл (сценарий
  системы codex), содержащий документацию о пакете @b(package-name) и
  системы системы @b(system-name). Если имя системы равно @b(nil),
  извлечение связанной с ней документации не выполняется.

  @b(Пример использования:)
@begin[lang=lisp](code)
 (mnas-package:document :mnas-package :mnas-package) => path_to_mnas-package_system/docs/mnas-package.scr
 (mnas-package:document :mnas-package/view nil) => path_to_mnas-package_system/docs/mnas-package-view.scr
@end(code)
")

(make-doc
  #'MNAS-PACKAGE:MAKE-CODEX-GRAPHS 'function
  "  @b(Описание:) функция @b(make-codex-graphs) создает в каталоге
./docs/build/mnas-package/html gv-файлы и png-файлы, содержащие графы,
отображающие завмсимости
@begin(list)
 @item(классов;)
 @item(систем;)
 @item(символов;)
 @item(вызовов.)
@end(list)
")

(make-doc
  #'MNAS-PACKAGE:FIND-SOURCES 'function
  NIL)

(make-doc
  #'MNAS-PACKAGE:SUB-CLASS-GRAPH 'function
  " @b(Описание:) метод @b(sub-class-graph) возвращает граф,
содержащий иерархию подклассов класса @b(class).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (mnas-graph:view-graph (sub-class-graph (find-class 'mnas-package/example::<a>)))
 (mnas-graph:view-graph (sub-class-graph (find-class 'list)))
@end(code)
")

(make-doc
  #'MNAS-PACKAGE:MAKE-HTML-PATH 'function
  "@b(Описание:) функция @b(make-html-path) в качестве побочного
   эффекта создает каталог, в который система codex выводит
   html-докуметы.")

(make-doc
  #'MNAS-PACKAGE:COPY-DOC->PUBLIC-HTML 'function
  "@b(Описание:) функция @b(copy-doc->public-html) выполняет
  копирование документации системы @b(system-name) в каталог
  ~/public_html/Common-Lisp-Programs.
")

(make-doc
  #'MNAS-PACKAGE:MAKE-DOC-METHODS 'function
  "@b(Описание:) функция @b(make-doc-methods) выводит в поток
@b(stream) раздел документации, подготовленной для вставки в 
scr-файл системы документирования codex. Этот раздел содержит 
методы класса @b(class), имена которых начинаются 
с префикса @b(prefix).

 @b(Пример использования:)
@begin[lang=lisp](code)
(make-doc-generics
 (find-package :mnas-package/example)
 (find-class 'mnas-package/example:<c-с-exp>)
 \"\")
-> @cl:with-package[name=\"MNAS-PACKAGE/EXAMPLE\"](
     @cl:doc(method m-a-exp (x <c-a-int>) (y <c-b-int>) (z <c-с-exp>))
     @cl:doc(method m-b-exp (x <c-a-int>) (y <c-b-int>) (z <c-с-exp>))
     @cl:doc(method m-c-exp (x <c-a-int>) (y <c-b-int>) (z <c-с-exp>)))
=> #<package \"MNAS-PACKAGE\">
@end(code) ")

(make-doc
  #'MNAS-PACKAGE::CODEX-HTML-PATHNAME 'function
  "@b(Описание:) функция @b(codex-html-pathname) возвращает строку,
содержащую расположение каталога ./docs/build/mnas-package/html системы 
@b(system-designator) на диске.

 (codex-html-pathname :mnas-package) 
 \"D:/PRG/msys32/home/namatv/quicklisp/local-projects/mnas/mnas-package/docs/build/mnas-package/html\"
 
")

(make-doc
  #'MNAS-PACKAGE::CODEX-DOCS-PATHNAME 'function
  "@b(Описание:) функция @b(codex-docs-pathname) возвращает строку,
содержащую расположение каталога ./docs системы @b(system-designator) на диске.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (codex-docs-pathname :mnas-package) 
 => \"D:/PRG/msys32/home/namatv/quicklisp/local-projects/mnas/mnas-package/docs\"
@end(code)
")

(make-doc
  #'MNAS-PACKAGE::MK-PATHNAME 'function
  NIL)

(make-doc
  #'MNAS-PACKAGE::FIND-ALL-GENERICS 'function
  "@b(Описание:) функция @b(find-all-generics) возвращает список
обобщенных функций, связанных с классом @b(class), начинающихся с 
префикса @b(prefix).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (require :temperature-fild/t-fild)
 (find-all-generics (find-class 'mtf/t-fild:<t-fild>) \"SPLOT\")
@end(code)
")

(make-doc
  #'MNAS-PACKAGE::FIND-ALL-METHODS 'function
  "(pprint (find-all-methods (find-class 'mtf:<sector>) \"SEC\"))")

(make-doc
  #'MNAS-PACKAGE::MAKE-CODEX-DOCUMENTATION 'function
  "@b(Описание:) функция @b(make-codex-documentation) выводит в поток @b(stream)
секции с документацией в формате codex, содержащие:
@begin(list)
 @item(переменные;)
 @item(функции;)
 @item(макросы;)
 @item(setf-функции;)
 @item(обобщенные функции;)
 @item(методы;)
 @item(классы.)
@end(list)
из пакета @b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-codex-documentation :mnas-package/example :internal t)
@end(code)
")

(make-doc
  #'MNAS-PACKAGE::REMOVE-MSYS-PREFIX 'function
  NIL)

(make-doc
  #'MNAS-PACKAGE::CODEX-BUILD-PATHNAME 'function
  "@b(Описание:) функция @b(codex-docs-pathname) возвращает строку,
содержащую расположение каталога ./docs системы @b(system-designator) на диске.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (codex-build-pathname :mnas-package) 
 => \"D:/PRG/msys32/home/namatv/quicklisp/local-projects/mnas/mnas-package/docs\"
@end(code)
")

(make-doc
  #'MNAS-PACKAGE::CODEX-HTML-PATHNAME/ 'function
  NIL)

