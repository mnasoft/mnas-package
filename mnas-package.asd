;;;; mnas-package.asd

(defsystem "mnas-package"
  :description " Система @b(mnas-package) предназначена для подготовки
документации, извлекаемой из asdf-систем.

@begin(section) @title(Мотивация)

 Система @b(Codex) является достаточно удобной для выполнения
 документирования систем, написанных с использованием @b(Common
 Lisp). Она позволяет получить на выходе документацию приемлемого
 вида.

 К недостатку сустемы @b(Codex) можно отнести то, что формирование
 шаблона документации не выполняется автоматически. Указание на
 включение разделов документации, относящихся к отдельным сущностям к
 которым можно отнести: 
@begin(list) 
@item(системы;) 
@item(пакеты;)
@item(классы;) 
@item(функции, setf-функции;) 
@item(обобщенные функции,методы, setf-методы;) 
@item(макросы;) @item(и т.д., и т.п.)
@end(list) приходится формировать вручную.

 Этот проект пытается устранить данный недостаток системы @b(Codex) за
счет определения функций и методов позволяющих: 
@begin(list)
@item(формировать код, предназначенный для передачи в систему
 @b(Codex);)
@item(формировать представление отдельных частей системы в виде
 графов.)
@end(list)
@end(section)
"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :version "0.1.5"
  :serial nil
  :in-order-to ((test-op (test-op "mnas-package/tests")))
  :depends-on ("mnas-package/obj"
               "mnas-package/pkg"
               "mnas-package/sys"
               "mnas-package/make"
               "mnas-package/view"
               "mnas-package/example"
               "mnas-string") ;;  "mnas-graph" "codex" "trivial-documentation"
  :components
  ((:module "src" 
    :serial nil
    :components
    ((:file "mnas-package")))))

(defsystem "mnas-package/obj"
  :description "Содержит некоторые функции и обобщенные функции,
используемые в проекте повсеместно"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/obj/tests")))
;;  :depends-on ()
  :components ((:module "src/obj"
		:serial nil
                :components ((:file "obj")))))

(defsystem "mnas-package/pkg"
  :description "Содержит некоторые функции и обобщенные функции,
используемые в проекте повсеместно"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/obj/tests")))
  :depends-on ("alexandria"
               "slynk"
               "mnas-package/obj"
               )
  :components ((:module "src/pkg"
		:serial nil
                :components ((:file "pkg")))))

(defsystem "mnas-package/sys"
  :description "Содержит некоторые функции для извлечения иформации о системах, поределенных с помощью @b(asdf)"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/obj/tests")))
  :depends-on ("alexandria")
  :components ((:module "src/sys"
		:serial nil
                :components ((:file "sys")))))

(defsystem "mnas-package/make"
  :description "Содержит некоторые функции и обобщенные функции,
используемые в проекте повсеместно"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/obj/tests")))
  :depends-on ("mnas-graph" "mnas-package/pkg")
  :components ((:module "src/make"
		:serial nil
                :components ((:file "make")))))

(defsystem "mnas-package/view"
  :description "Содержит некоторые функции и обобщенные функции,
используемые в проекте повсеместно"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/obj/tests")))
  :depends-on ("mnas-package/make" "mnas-graph/view")
  :components ((:module "src/view"
		:serial nil
                :components ((:file "view")))))

(defsystem "mnas-package/tests"
  :description "Тестирование систем, входящих  в проект mnas-package"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("mnas-package" "mnas-package/example" "fiveam")
  :perform (test-op (o s)
		    (uiop:symbol-call :mnas-package/tests :run-tests))
  :components ((:module "src/tests"
		:serial nil
                :components ((:file "package")
                             (:file "all"  :depends-on ("package"))
                             (:file "obj"  :depends-on ("all"))
                             (:file "pkg"  :depends-on ("all"))
                             (:file "make" :depends-on ("all"))
                             (:file "view" :depends-on ("all"))
                             (:file "main" :depends-on ("all"))
                             (:file "run"  :depends-on ("obj" "pkg" "make" "view" "main"))
                             ))))

(defsystem "mnas-package/example"
  :description "Пример системы для выполнения тестирования пакета mnas-package"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :components ((:module "src/example"
			:serial t
			:components ((:file "example")))))

(defsystem "mnas-package/docs"
  :description "Зависимости для сборки документации"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("mnas-package" "codex"))
