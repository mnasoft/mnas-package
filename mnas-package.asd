;;;; mnas-package.asd

(defsystem "mnas-package"
  :description "Describe mnas-package here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :version "0.0.6"
  :serial nil
  :depends-on ("mnas-string" "mnas-graph" "codex" "trivial-documentation")
  :components
  ((:file "package")
   (:module "src" :depends-on ("package")
    :serial nil
    :components
    ((:file "mnas-package")
     (:file "codex" :depends-on ("mnas-package"))
     (:file "cl-user-import-symbols" :depends-on ("mnas-package"))
     (:file "codex-method")
;; (:file "demos" :depends-on ("mnas-package"))     
     ))))
