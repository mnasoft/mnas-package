;;;; mnas-call-graph.asd

(defsystem #:mnas-call-graph
  :description "Describe mnas-call-graph here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license  "GNU GPLv3"
  :version "0.0.2"
  :serial nil
  :depends-on (#:mnas-graph)
  :components ((:file "mnas-call-graph")
	       (:file "demos" :depends-on ("mnas-call-graph"))
	       (:file "cl-user-import-symbols" :depends-on ("mnas-call-graph"))))

