;;;; mnas-package.asd

(defsystem "mnas-package"
  :description "Describe mnas-package here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :version "0.0.8"
  :serial nil
  :depends-on ("mnas-package/obj"
               "mnas-package/pkg"
               "mnas-package/make"
               "mnas-package/view"
               "mnas-string") ;;  "mnas-graph" "codex" "trivial-documentation"
  :components
  ((:file "package")
   (:module "src" :depends-on ("package")
    :serial nil
    :components
    ((:file "mnas-package")
     (:file "codex" :depends-on ("mnas-package"))
     ;; (:file "cl-user-import-symbols" :depends-on ("mnas-package"))
     (:file "codex-method")
     ;; (:file "demos" :depends-on ("mnas-package"))     
     ))))

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
  :depends-on ("alexandria")
  :components ((:module "src/pkg"
		:serial nil
                :components ((:file "pkg")))))

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
  :depends-on ("mnas-package/make")
  :components ((:module "src/view"
		:serial nil
                :components ((:file "view")))))

(defsystem "mnas-package/tests"
  :description "Тестирование систем, входящих  в проект mnas-package"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("mnas-package" "mnas-package/example" "fiveam")
  :perform (test-op (o s)
		    (uiop:symbol-call :math-tests :test-math))
  :components ((:module "tests"
			:serial t
			:components ((:file "package")
				     (:file "main")
				     (:file "matrix")))))

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
