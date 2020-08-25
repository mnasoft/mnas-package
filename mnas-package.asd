;;;; mnas-package.asd

(defsystem #:mnas-package
  :description "Describe mnas-package here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :version "0.0.4"
  :serial nil
  :depends-on (#:cl-annot #:mnas-graph) ;;;; #:codex
  :components ((:file "package")
	       (:file "mnas-package" :depends-on ("package"))
	       (:file "codex" :depends-on ("package" "mnas-package"))
;;;;	       (:file "demos" :depends-on ("mnas-package"))
	       (:file "cl-user-import-symbols" :depends-on ("mnas-package"))))
