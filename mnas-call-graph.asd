;;;; mnas-call-graph.asd

(defsystem #:mnas-call-graph
  :description "Describe mnas-call-graph here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:mnas-graph)
  :components ((:file "mnas-call-graph")
	       (:file "demos")))
