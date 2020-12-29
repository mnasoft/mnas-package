;;;; package.lisp

(defpackage #:mnas-package
  (:use #:cl )
  (:nicknames "MPKG")
  (:export function-name)
  (:export make-codex-documentation
           make-codex-section-functions
           make-codex-section-generic-functions
           )
  (:export make-codex-graphs
           make-symbol-graph
           make-class-graph
           make-call-graph
           make-system-graph)
  (:export view-call-graph
	   view-system-graph
           view-class-graph
           view-symbol-graph)
  (:export doc-template)
  (:export functions
           generic-functions
	   symbols
           package-classes)
  (:export use-mnas-package
           unuse-mnas-package)
  (:export make-doc-generics
           make-doc-methods))

;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))

