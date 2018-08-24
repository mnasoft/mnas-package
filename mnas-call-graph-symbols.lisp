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

;;;; BAK - implementation

(defun read-file (path)
  (let ((sb-impl-d-e-f sb-impl::*default-external-format*)
	(sb-impl::*default-external-format* :UTF-8))
    (with-open-file (s path)
      (do ((rez nil)
	   (form (read s nil 'done) (read s nil 'done)))
	  ((eq form 'done) (progn     (setf sb-impl::*default-external-format* sb-impl-d-e-f ) rez))
	(push form rez)))
    ))

(defun defun-code (code)
  (let ((rez nil))
    (mapc
     #'(lambda (el)
	 (when  (eql (car el) 'defun)
	   (push el rez)))
     code)
    (reverse rez)))

(defun defun-name (code)
  (remove-duplicates (mapcar 'second (defun-code code))))

(defun defmethod-code (code)
  (let ((rez nil))
    (mapc
     #'(lambda (el)
	 (when  (eql (car el) 'defmethod)
	   (push el rez)))
     code)
    (reverse rez)))

(defun defmethod-name (code)
  (remove-duplicates (mapcar 'second (defmethod-code code))))

(defun def-name (code)
  (append (defun-name code) (defmethod-name code)))


(defun make-call-praph (lisp-file )
  (mnas-graph:view-graph
   (mnas-graph:generate-graph
    (mnas-call-graph:who-calls-lst
     (mnas-call-graph:def-name
	 (mnas-call-graph:read-file lisp-file))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require    :mnas-graph)
(in-package :mnas-graph)


(mnas-call-graph:make-call-praph  "~/quicklisp/local-projects/mnas/mnas-graph/mnas-graph.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require    :algorithm)
(in-package :algorithm)

(mnas-call-graph:make-call-praph (find-package :algorithm))

(mnas-call-graph:make-call-praph
"/home/namatv/quicklisp/local-projects/clisp/algorithm/algorithm.lisp")
