;;;; mnas-call-graph.lisp

(in-package #:cl-user)

(defpackage #:mnas-call-graph
  (:use #:cl)
  (:export read-file
	   defun-code
	   defun-name
	   defmethod-code
	   defmethod-name
	   def-name
	   defu-defm-name
	   who-calls
	   who-calls-lst
	   make-call-praph))

(in-package #:mnas-call-graph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun defu-defm-name (func)
    (cond
      ((listp (first func))
       (second (first func)))
      ((null (listp (first func)))
       (first func))))

(defun who-calls (func)
  (let
      ((rez (swank/backend:who-calls func))
       (func-str (string-downcase (string func))))
    (mapcar
     #'(lambda (el1)
	 (list el1 func-str))
     (remove-duplicates 
      (mapcar
       #'(lambda (el)
	   (string-downcase
	    (string
	     (defu-defm-name el))))
       rez)
      :test #'equal))))

(defun who-calls-lst (func-lst)
  (apply #'append
   (mapcar #'who-calls
	   func-lst)))

(defun make-call-praph (lisp-file )
  (mnas-graph:view-graph
   (mnas-graph:generate-graph
    (mnas-call-graph:who-calls-lst
     (mnas-call-graph:def-name
	 (mnas-call-graph:read-file lisp-file))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require    :mnas-graph)
(in-package :mnas-graph)

(mnas-call-graph:make-call-praph
  "~/quicklisp/local-projects/mnas/mnas-graph/mnas-graph.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require    :algorithm)
(in-package :algorithm)

(mnas-call-graph:make-call-praph
"/home/namatv/quicklisp/local-projects/clisp/algorithm/algorithm.lisp")
