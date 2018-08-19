;;;; mnas-call-graph-symbols.lisp

(in-package #:mnas-call-graph)

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
