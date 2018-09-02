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
  (make-call-praph :mnas-call-graph))

(export 'demo-10)
(defun demo-10 ()
  (require :mnas-string)
  (make-call-praph :mnas-string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn 
  (defclass a1 ()      ())
  (defclass a2 (a1)    ())
  (defclass a3 (a2)    ())
  (defclass a4 (a3)    ())

  (defclass b1 ()      ())
  (defclass b2 (b1)    ())
  (defclass b3 (b2)    ())
  (defclass b4 (b3)    ())

  (defclass c1 (a3 b3) ()))

(export 'demo-11)
(defun demo-11 ()
  (make-call-praph  :mnas-call-graph)
  (make-class-graph :mnas-call-graph))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "(mnas-call-graph:demo-1)~%(mnas-call-graph:demo-2)~%(mnas-call-graph:demo-3)~%(mnas-call-graph:demo-10)~%(mnas-call-graph:demo-11)")
