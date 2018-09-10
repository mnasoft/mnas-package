;;;; mnas-call-graph.asd

(defsystem #:mnas-call-graph
  :description "Describe mnas-call-graph here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :version "0.0.3"
  :serial nil
  :depends-on (#:mnas-graph)
  :components ((:file "mnas-call-graph")
	       (:file "demos" :depends-on ("mnas-call-graph"))
	       (:file "cl-user-import-symbols" :depends-on ("mnas-call-graph"))))

