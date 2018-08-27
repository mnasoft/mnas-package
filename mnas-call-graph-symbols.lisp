;;;; mnas-call-graph-symbols.lisp

(in-package #:mnas-call-graph)

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


(defun find-subclasses (class graph)
  (break "~S" class)
  (mapcar
   #'(lambda (el)
       (mnas-graph:insert-to
	(make-instance
	 'mnas-graph:edge
	 :from (make-instance 'mnas-graph:node :node (string (class-name class)))
	 :to   (make-instance 'mnas-graph:node :node (string (class-name el))))
	graph)
       (find-subclasses el graph))
   (sb-mop:class-direct-subclasses class))
  graph)

(progn
  (defparameter *g* (make-instance 'mnas-graph:graph) )
  (mnas-graph:view-graph
   (progn 
     (find-subclasses (find-class 'a1) *g*)
     (find-subclasses (find-class 'b1) *g*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-call-praph :mnas-graph )
