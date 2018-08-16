;;;; mnas-call-graph.lisp

(in-package #:cl-user)

(defpackage #:mnas-call-graph
  (:use #:cl))

(in-package #:mnas-call-graph)

(setf sb-impl::*default-external-format* :UTF-8)

(defun load-lisp-lisp-as-code (fname &key (temporary-fname "~/quicklisp/local-projects/tem-temporary-tempo.lisp"))
  (with-open-file (o-stream temporary-fname :direction :output :if-exists :supersede :external-format :UTF-8) ;;;; 
    (princ "(defparameter *code* '(" o-stream)
    (princ #\newline o-stream)
    (with-open-file (stream fname :external-format  :UTF-8) ;;;; 
      (loop for line = (read-line stream nil 'foo)
	 until (eq line 'foo)
	 do (progn
	      (princ line o-stream)
	      (princ #\newline o-stream))))
    (princ #\newline o-stream)
    (princ "))" o-stream))
  (load temporary-fname)
  *code*)

(defparameter *code* 
  (load-lisp-lisp-as-code "~/quicklisp/local-projects/mnas/mnas-graph/mnas-graph.lisp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun-name *code*)
(defmethod-name *code*)
(def-name *code*)

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

(export 'who-calls-lst)

(in-package #:mnas-graph)
(who-calls 'demo-5)
(who-calls 'MAKE-RANDOM-GRAPH)
(who-calls 'to-string)

(mnas-graph:view-graph
 (mnas-graph:generate-graph 
  (who-calls-lst (def-name *code*))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

