;;;; demos.lisp

(in-package :mnas-call-graph)

(export 'demo-1)
(defun demo-1 ()
  (require :mnas-string)
  (package-symbols-by-category :mnas-call-graph :internal nil))

(export 'demo-2)
(defun demo-2 ()
  (require :mnas-string)
  (package-symbols-by-category :mnas-call-graph))

(export 'demo-3)
(defun demo-3 ()
  (mnas-call-graph:make-call-praph :mnas-call-graph))

(export 'demo-10)
(defun demo-10 ()
  (require :mnas-string)
  (make-call-praph :mnas-string))

(format t "(mnas-call-graph:demo-1)~%(mnas-call-graph:demo-2)~%(mnas-call-graph:demo-3)~%(mnas-call-graph:demo-10)")
