;;;; ./mnas-package/src/pkg/pkg-doc.lisp
(in-package :mnas-package/pkg)

(defmacro make-doc (obj-name obj-type doc-string)
  `(setf (documentation ,obj-name ,obj-type)
         ,doc-string))

(defun find-slot (slot-name class)
  (find slot-name (closer-mop:class-direct-slots (find-class class))
        :key #'closer-mop:slot-definition-name))

(make-doc
 #'MNAS-PACKAGE/PKG:WHO-CALLS-LST 'function
 "Not yet documented")

(make-doc
  #'MNAS-PACKAGE/PKG:PACKAGE-SETF-FUNCTIONS 'function
  "@b(Описание:) функция @b(package-setf-functions) возвращает список
setf-функций пакета @b(package-name).

 @b(Пример использования:) @begin[lang=lisp](code)
 (package-setf-functions :mnas-package/example :internal t)
 => (#<FUNCTION (SETF MNAS-PACKAGE/EXAMPLE::FOO)>)
@end(code)")

(make-doc
  #'MNAS-PACKAGE/PKG:PACKAGE-METHODS 'function
  "@b(Описание:) функция @b(package-methods) возвращает список методов пакета
@b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
  (package-methods :mnas-package/example :internal t)
  (#<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE:M-A-EXP (MNAS-PACKAGE/EXAMPLE::<C-A-INT> MNAS-PACKAGE/EXAMPLE::<C-B-INT> MNAS-PACKAGE/EXAMPLE:<C-C-EXP>) {1004634963}>
   #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE:M-B-EXP (MNAS-PACKAGE/EXAMPLE::<C-A-INT> MNAS-PACKAGE/EXAMPLE::<C-B-INT> MNAS-PACKAGE/EXAMPLE:<C-C-EXP>) {1004634983}>
   #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE:M-C-EXP (MNAS-PACKAGE/EXAMPLE::<C-A-INT> MNAS-PACKAGE/EXAMPLE::<C-B-INT> MNAS-PACKAGE/EXAMPLE::<C-C-INT>) {10046349A3}>
   #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE:M-C-EXP (MNAS-PACKAGE/EXAMPLE:<C-A-EXP> MNAS-PACKAGE/EXAMPLE:<C-B-EXP> MNAS-PACKAGE/EXAMPLE:<C-C-EXP>) {10049D5BD3}>)

@end(code)")

(make-doc
  #'MNAS-PACKAGE/PKG:PACKAGE-MACROSES 'function
  "@b(Описание:) функция @b(package-macroses) возвращает список
 макросов пакета @b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (package-macroses :mnas-package/example :internal t)
 => (#<FUNCTION (MACRO-FUNCTION MNAS-PACKAGE/EXAMPLE::MAK-A-SHORT) {52D45ECB}>
     #<FUNCTION (MACRO-FUNCTION MNAS-PACKAGE/EXAMPLE::MAK-A) {52D454BB}>)
@end(code)")

(make-doc
  #'MNAS-PACKAGE/PKG:FILTER-FUNCTIONS 'function
  "@b(Описание:) функция filter-functions возвращает список символов, являющихся
сопряженными с функциями.

 @b(Переменые:)
@begin(list)
@item(symbols - список символов пакета.)
@end(list)")

(make-doc
  #'MNAS-PACKAGE/PKG:PACKAGE-VARIABLES 'function
  "@b(Описание:) функция @b(package-variables) возвращает список символов пакета
@b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (package-variables :mnas-package :inherited t)
@end(code)")

(make-doc
  #'MNAS-PACKAGE/PKG:FILTER-MACROSES 'function
  " @b(Описание:) функция filter-functions возвращает список символов сопряженных
с макросами.

 @b(Переменые:) @begin(list) @item(symbols - список символов пакета.)
 @end(list)")

(make-doc
  #'MNAS-PACKAGE/PKG:PACKAGE-FUNCTIONS 'function
  "@b(Описание:) функция @b(package-functions) возвращает список функций пакета @b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (package-functions :mnas-package/example :internal t)
 => (#<FUNCTION MNAS-PACKAGE/EXAMPLE:BAZ-SHORT> #<FUNCTION MNAS-PACKAGE/EXAMPLE:BAZ>)
@end(code)")

(make-doc
  #'MNAS-PACKAGE/PKG:PACKAGE-CLASSES 'function
  "@b(Описание:) package-classes возвращает список классов пакета.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (package-classes :mnas-package/example) => (#<STANDARD-CLASS MNAS-PACKAGE/EXAMPLE:<C>>)
 (package-classes :mnas-package/example :external nil :internal t)
  => (#<STANDARD-CLASS MNAS-PACKAGE/EXAMPLE::<A>>
      #<STANDARD-CLASS MNAS-PACKAGE/EXAMPLE::<B>>
      #<STANDARD-CLASS MNAS-PACKAGE/EXAMPLE::<B-SHORT>>
      #<STANDARD-CLASS MNAS-PACKAGE/EXAMPLE::<C-SHORT>>
      #<STANDARD-CLASS MNAS-PACKAGE/EXAMPLE::<A-SHORT>>)
@end(code)")

(make-doc
  #'MNAS-PACKAGE/PKG:FILTER-SETF-FUNCTIONS 'function
  "@b(Описание:) функция filter-functions возвращает список символов,
являющихся сопряженными с setf-функциями.

 @b(Переменые:)
@begin(list) 
@item(symbols - список символов пакета.)
@end(list)")

(make-doc
  #'MNAS-PACKAGE/PKG:FILTER-GENERICS 'function
  "@b(Описание:) функция @b(filter-generics) возвращает список
символов сопряженных с обобщеными функциями.

 @b(Переменые:)
@begin(list)
@item(symbols - список символов пакета.)
@end(list)")

(make-doc
  #'MNAS-PACKAGE/PKG:PACKAGE-SYMBOLS-BY-CATEGORY 'function
  "@b(Описание:) package-symbols-by-category выполнят поиск символов, 
определенных пакетом @b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (package-symbols-by-category 'mnas-package :internal nil)                 ;; отбор только внешних символов;
 (package-symbols-by-category :mnas-package)                               ;; отбор внешних и внутренних символов;
 (package-symbols-by-category \"MNAS-PACKAGE\" :internal nil :inherited t) ;; отбор только внешних и заимствованных символов;
@end(code)")

(make-doc
  #'MNAS-PACKAGE/PKG:FUNC-TO-STRING 'function
  "Not yet documented")

(make-doc
  #'MNAS-PACKAGE/PKG:FILTER-SETF-METHODS 'function
  "@b(Описание:) функция @b(filter-setf-methods) возвращает список символов,
являющихся сопряженными с setf-методами.

 @b(Переменые:)
@begin(list) 
@item(symbols - список символов пакета.)
@end(list)")

(make-doc
  #'MNAS-PACKAGE/PKG:FILTER-SETF-GENERICS 'function
  "@b(Описание:) функция @b(filter-setf-generics) возвращает список символов,
являющихся сопряженными с обобщенными setf-функциями.

 @b(Переменые:)
@begin(list) 
@item(symbols - список символов пакета.)
@end(list)")

(make-doc
  #'MNAS-PACKAGE/PKG:FILTER-VARIABLES 'function
  "@b(Описание:) функция filter-variables возвращает список символов, являющихся
сопряженными со значениями.

 @b(Переменые:)
@begin(list) 
@item(symbols - список символов пакета.)
@end(list)")

(make-doc
  #'MNAS-PACKAGE/PKG:PACKAGE-GENERICS 'function
  "@b(Описание:) функция @b(package-generics) возвращает список обобщенных функций
пакета @b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
  (package-generics :mnas-package/example :internal t) 
 => (#<STANDARD-GENERIC-FUNCTION MNAS-PACKAGE/EXAMPLE:M-C-EXP (1)>
     #<STANDARD-GENERIC-FUNCTION MNAS-PACKAGE/EXAMPLE:M-B-EXP (1)>
     #<STANDARD-GENERIC-FUNCTION MNAS-PACKAGE/EXAMPLE:M-A-EXP (1)>
     #<STANDARD-GENERIC-FUNCTION MNAS-PACKAGE/EXAMPLE::<C-C-INT>-C (1)>
     #<STANDARD-GENERIC-FUNCTION MNAS-PACKAGE/EXAMPLE::<C-B-INT>-B (1)>
     #<STANDARD-GENERIC-FUNCTION MNAS-PACKAGE/EXAMPLE::<C-A-INT>-A (1)>)
@end(code)")

(make-doc
  #'MNAS-PACKAGE/PKG:WHO-REFERENCES-LST 'function
  "Not yet documented")

(make-doc
  #'MNAS-PACKAGE/PKG:PACKAGE-SETF-METHODS 'function
  "@b(Описание:) функция @b(package-setf-methods) возвращает список setf-методов пакета
@b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
  (package-setf-methods :mnas-package/example :internal t)
    (#<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE::M-FOO :AROUND (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> T) {10019B69A3}>
     ...
     #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE::M-FOO-SHORT (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> MNAS-PACKAGE/EXAMPLE:<C>) {1001C6CAC3}>)
@end(code)")

(make-doc
  #'MNAS-PACKAGE/PKG:PACKAGE-SETF-GENERICS 'function
  "@b(Описание:) функция @b(package-setf-generics) возвращает список функций пакета
@b(package-name).

 @b(Пример использования:) @begin[lang=lisp](code)
 (package-setf-functions :mnas-package/example :internal t)
 => (#<FUNCTION (SETF MNAS-PACKAGE/EXAMPLE::FOO)>)
@end(code)")

(make-doc
  #'MNAS-PACKAGE/PKG:FILTER-METHODS 'function
  "@b(Описание:) функция @b(filter-methods) возвращает список символов,
являющихся сопряженными с setf-методами.

 @b(Переменые:)
@begin(list) 
@item(symbols - список символов пакета.)
@end(list)")

(make-doc
  #'MNAS-PACKAGE/PKG::DEFU-DEFM-NAME 'function
  "Not yet documented")

(make-doc
  #'MNAS-PACKAGE/PKG::PACKAGE-SYMBOLS-ALL 'function
  "@b(Описание:) package-symbols-all Выполнят поиск всех символов, 
определенных пакетом @b(package-name).

 @b(Пример использования:)
 @begin[lang=lisp](code)
 (package-symbols-all 'mnas-package)
 (package-symbols-all :mnas-package)
 (package-symbols-all (find-package :mnas-package))
 (package-symbols-all \"MNAS-PACKAGE\")
@end(code)")

(make-doc
  #'MNAS-PACKAGE/PKG::ORDERED-DEP-TREE 'function
  "Not yet documented")

(make-doc
  #'MNAS-PACKAGE/PKG::WHO-REFERENCES 'function
  " Выполняет поиск функций, в которых есть ссылка на внешнюю переменную
var. Возвращает список, каждым элементом которого является список следующего
формата: (функция переменная).
 
 @b(Пример использования:)
@begin[lang=lisp](code)
 (who-references '*sample-var*) 
 => ((\"who-references\" \"*sample-var*\"))
@end(code)")

(make-doc
  #'MNAS-PACKAGE/PKG::WHO-CALLS 'function
    "Not yet documented")

(make-doc
  #'MNAS-PACKAGE/PKG:DEPENDENCIES-OF 'function
  "Not yet documented")

(make-doc
  #'MNAS-PACKAGE/PKG:DEPENDENCY-TREE 'function
  "Not yet documented")

(make-doc
  #'MNAS-PACKAGE/PKG::->KEY 'function
  "Not yet documented")

(make-doc
  (find-method #'MNAS-PACKAGE/PKG::->KEY NIL '(STRING))
  t
  NIL)

(make-doc
  (find-method #'MNAS-PACKAGE/PKG::->KEY NIL '(SYMBOL))
  t
  NIL)

(make-doc
  (find-method #'MNAS-PACKAGE/PKG::->KEY NIL '(CONS))
  t
  "Выполнить отладку!")

(make-doc
  (find-method #'MNAS-PACKAGE/PKG:DEPENDENCY-TREE NIL '(SYMBOL))
  t
  "Not yet documented")

(make-doc
  (find-method #'MNAS-PACKAGE/PKG:DEPENDENCIES-OF NIL '(SYMBOL))
  t
  "Not yet documented")

