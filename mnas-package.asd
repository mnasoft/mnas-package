;;;; mnas-package.asd

(defsystem "mnas-package"
  :description "Describe mnas-package here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :version "0.0.7"
  :serial nil
  :depends-on ("mnas-string"
               "mnas-graph" "codex" "trivial-documentation"
               "mnas-package/core" "mnas-package/pkg"
               "mnas-package/make-graph")
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

(defsystem "mnas-package/core"
  :description "Содержит некоторые функции и обобщенные функции,
используемые в проекте повсеместно"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/core/tests")))
;;;;  :depends-on ()
  :components ((:module "src/core"
		:serial nil
                :components ((:file "core")))))

(defsystem "mnas-package/pkg"
  :description "Содержит некоторые функции и обобщенные функции,
используемые в проекте повсеместно"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/core/tests")))
;;;;  :depends-on ()
  :components ((:module "src/pkg"
		:serial nil
                :components ((:file "pkg")))))

(defsystem "mnas-package/make-graph"
  :description "Содержит некоторые функции и обобщенные функции,
используемые в проекте повсеместно"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/core/tests")))
  :depends-on ("mnas-package/pkg")
  :components ((:module "src/make-graph"
		:serial nil
                :components ((:file "make-graph")))))
