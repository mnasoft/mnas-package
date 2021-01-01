;;;; package.lisp

(defpackage #:mnas-package
  (:use #:cl )
  (:nicknames "MPKG")
  (:export obj-package
           obj-package-string
           obj-name
           obj-name-string
           insert-codex-doc)
  (:export class-undirect-subclasses)
  (:export make-codex-documentation)
  (:export make-codex-section-variables
           make-codex-section-functions
           make-codex-section-generics
           make-codex-section-methods
           make-codex-section-classes)
  (:export make-codex-graphs
           make-symbol-graph
           make-class-graph
           make-call-graph
           make-system-graph)
  (:export view-call-graph
	   view-system-graph
           view-class-graph
           view-symbol-graph)
  (:export package-functions
           package-generics
	   package-variables
           package-classes
           package-methods)
  (:export make-doc-generics
           make-doc-methods)
  (:documentation "
COOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOL"
  ))

;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
