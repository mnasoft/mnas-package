;;;; ./mnas-package/src/mnas-package-doc.lisp

(in-package :mnas-package)

(defmacro make-doc (obj-name obj-type doc-string)
  `(setf (documentation ,obj-name ,obj-type)
         ,doc-string))

(defun find-slot (slot-name class)
  (find slot-name
        (closer-mop:class-direct-slots (find-class class))
        :key #'closer-mop:slot-definition-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-doc
  (find-package 'MNAS-PACKAGE) t
  "@b(Описание:) пакет @b(mnas-package) является основным в системе @b(mnas-package).

 Основными функциями в системе являются:
@begin(list)
 @item(document;)
 @item(make-codex-graphs;)
@end(list)

 Перечисленные ниже функции имеют схожий набор аргументов:
@begin(list)
 @item(document;)
 @item(make-codex-documentation;)
 @item(section-system;)
 @item(section-package;) 
 @item(section-variables;)
 @item(section-functions;) 
 @item(section-macroses;) 
 @item(section-setf-functions;)
 @item(section-generics;) 
 @item(section-setf-generics;)
 @item(section-methods;) 
 @item(section-classes.)
@end(list)

  @b(Аргументы:)
@begin(list)
 @item(package-name - пакет из которого извлекаются
       сущности (глобальными переменными, функциями, и т.д. и т.п.);)
 @item(stream - поток, в который выводятся информация о сущностях;)
 @item(external - если не nil - в поток выводятся информация о
       экспортируемых сущностях;)
 @item(internal - если не nil - в поток выводятся информация о
       внутренних сущностях;)
 @item(inherited - если не nil - в поток выводятся информация о
       заимствованных сущностях;)
 @item(sort - если не nil - сущности сортируются в алфавитном
 порядке;)
 @item(min-doc-length - минимальная длина строки документации,
       связанной с сущностью, при которой созается ссылка указаение на
       вставку документации.)
@end(list)
")

(make-doc
  'MNAS-PACKAGE:*INTRANET-HOSTS* 'variable
  "@b(Описание:) переменная @b(*intranet-hosts*) определяет имена
 хостов, на которых нет выхода в Интернет.")

(make-doc
  'MNAS-PACKAGE::+MAINFEST-LISP-TEMPLATE+ 'variable
  "@b(Описание:) переменая @b(+mainfest-lisp-template+) определяет
 шаблон для создания файла @i(docs/manifest.lisp).
")

(make-doc
  (macro-function 'MNAS-PACKAGE::MAKE-DOC) t
  NIL)

(make-doc
  #'MNAS-PACKAGE:COPY-DOC->PUBLIC-HTML 'function
  "@b(Описание:) функция @b(copy-doc->public-html) выполняет
  копирование документации системы @b(system-name) в каталог
  ~/public_html/Common-Lisp-Programs.")

(make-doc
  #'MNAS-PACKAGE:MAKE-CODEX-GRAPHS 'function
  " @b(Описание:) функция @b(make-codex-graphs) создает в каталоге
./docs/build/mnas-package/html gv-файлы и png-файлы, содержащие графы,
отображающие завмсимости
@begin(list)
 @item(классов;)
 @item(систем;)
 @item(символов;)
 @item(вызовов.)
@end(list)")

(make-doc
  #'MNAS-PACKAGE:MAKE-HTML-PATH 'function
  "@b(Описание:) функция @b(make-html-path) в качестве побочного
   эффекта создает каталог, в который система codex выводит
   html-докуметы.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-html-path :mnas-path)
@end(code)")

(make-doc
  #'MNAS-PACKAGE:SUPER-CLASS-GRAPH 'function
  " @b(Описание:) метод @b(sub-class-graph) возвращает граф,
содержащий иерархию предков для класса @b(class).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (mnas-graph:view-graph (super-class-graph (find-class 'mnas-package/example:<c>)))
 (mnas-graph:view-graph (super-class-graph (find-class 'list)))
@end(code)")

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
  " @b(Описание:) function @b(document) формирует scr-файл (сценарий
  системы codex), содержащий документацию о пакете @b(package-name) и
  системы системы @b(system-name). Если имя системы равно @b(nil),
  извлечение связанной с ней документации не выполняется.

  @b(Пример использования:)
@begin[lang=lisp](code)
(mnas-package:document :mnas-package :mnas-package) 
 => path_to_mnas-package_system/docs/mnas-package.scr
(mnas-package:document :mnas-package/view nil) 
 => path_to_mnas-package_system/docs/mnas-package-view.scr
@end(code)")

(make-doc
  #'MNAS-PACKAGE:RSYNC-DOC 'function
  "@b(Описание:) функция @b(rsync-doc) выполняет копирование
  документации на удаленный сервер.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (rsync-doc \"mnas-package\")
@end(code)
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
@end(code)")

(make-doc
  #'MNAS-PACKAGE:MAKE-MAINFEST-LISP 'function
  "@b(Описание:) функция @b(make-mainfest-lisp) создает файл
  @i(docs/manifest.lisp).

 @b(Переменые:)
@begin(list)
 @item(systems - список систем;)
 @item(title - заголовок;)
 @item(authors - список авторов;)
 @item(sources - список исходных файлов;)
 @item(output-format - атрибуты формата вывода.)
@end(list)


 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-mainfest-lisp 
  '(:MNAS-PACKAGE)
  \"Mnas-Package\"
  '(\"Mykola Matvyeyev\")
  '(\"mnas-file-dialog.scr\" \"mnas-file-dialog-graph.scr\"))
@end(code)
")

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
  #'MNAS-PACKAGE::CODEX-HTML-PATHNAME 'function
  "@b(Описание:) функция @b(codex-html-pathname) возвращает строку,
содержащую расположение каталога ./docs/build/mnas-package/html системы 
@b(system-designator) на диске.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (codex-html-pathname :mnas-package) 
 => \"D:/PRG/msys32/home/namatv/quicklisp/local-projects/mnas/mnas-package/docs/build/mnas-package/html\"
@end(code)")

(make-doc
  #'MNAS-PACKAGE::CODEX-DOCS-PATHNAME 'function
  "@b(Описание:) функция @b(codex-docs-pathname) возвращает строку,
содержащую расположение каталога ./docs системы @b(system-designator) на диске.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (codex-docs-pathname :mnas-package) 
 => \"D:/PRG/msys32/home/namatv/quicklisp/local-projects/mnas/mnas-package/docs\"
@end(code)")

(make-doc
  #'MNAS-PACKAGE::FIND-ALL-GENERICS 'function
  "@b(Описание:) функция @b(find-all-generics) возвращает список
обобщенных функций, связанных с классом @b(class), начинающихся с 
префикса @b(prefix).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (require :temperature-fild/t-fild)
 (find-all-generics (find-class 'mtf/t-fild:<t-fild>) \"SPLOT\")
@end(code)")

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
@end(code)")

(make-doc
  #'MNAS-PACKAGE::CODEX-BUILD-PATHNAME 'function
  "@b(Описание:) функция @b(codex-docs-pathname) возвращает строку,
содержащую расположение каталога ./docs системы @b(system-designator) на диске.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (codex-build-pathname :mnas-package) 
 => \"D:/PRG/msys32/home/namatv/quicklisp/local-projects/mnas/mnas-package/docs\"
@end(code)")

(make-doc
  #'MNAS-PACKAGE::CODEX-HTML-PATHNAME/ 'function
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
 (codex-html-pathname/ \"mnas-package\") 
 => \"/home/namatv/quicklisp/local-projects/mnas/mnas-package/docs/build/mnas-package/html/\"
@end(code)

NIL
")
