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

(defun package-classes (package-name
			&aux (lst nil) (package (find-package package-name)))
  (declare ((or package string symbol) package-name))
  (let ((rez nil)
	(class nil)) 
    (mapc 
     #'(lambda (el)
	 (setf class (find-class el nil))
	 (when class (push class rez)))
     (package-symbols-by-category package))
    rez))

(defun find-subclasses (class graph)
;;;;  (break "~S" class)
  (mapcar
   #'(lambda (el)
       (mnas-graph:insert-to
	(make-instance
	 'mnas-graph:edge
	 :from (make-instance 'mnas-graph:node :owner graph :name (string (class-name class)))
	 :to   (make-instance 'mnas-graph:node :owner graph :name (string (class-name el))))
	graph)
;       (find-subclasses el graph)
       )
   (sb-mop:class-direct-subclasses class))
  graph)

(defun package-class-hiearchy (package-name graph
			       &aux (lst nil) (package (find-package package-name)))
  (declare ((or package string symbol) package-name))
  (mapc
   #'(lambda (el)
       (find-subclasses (find-class el) graph))
   (package-classes package-name))
  graph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (defparameter *g* (make-instance 'mnas-graph:graph) )
  (mnas-graph:view-graph
   (package-class-hiearchy :mnas-call-graph *g*)))


   (progn 
     (find-subclasses (find-class 'a1) *g*)
     (find-subclasses (find-class 'b1) *g*))))

(sb-mop:class-direct-subclasses (find-class 'A3))
(find-subclasses (find-class 'A3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-call-praph :mnas-graph )
(package-classes :mnas-call-graph )


