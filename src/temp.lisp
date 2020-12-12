;;;; temp.lisp

(in-package #:mnas-package)


(package-system-graph
 :mnas-package
 :fname "package-system-graph"
 :fpath "/home/namatv/quicklisp/local-projects/mnas/mnas-package/docs/"
 :out-type "png"
 :viewer nil)

(package-call-graph
 :mnas-package
 :fname "package-call-graph"
 :fpath "/home/namatv/quicklisp/local-projects/mnas/mnas-package/docs/"
 :out-type "png"
 :viewer nil)

(package-class-graph
 :mnas-package
 :fname "package-class-graph"
 :fpath "/home/namatv/quicklisp/local-projects/mnas/mnas-package/docs/"
 :out-type "png"
 :viewer nil)

(package-symbol-graph
 :mnas-graph
 :fname "package-symbol-graph"
 :fpath "/home/namatv/quicklisp/local-projects/mnas/mnas-package/docs/"
 :out-type "png"
 :viewer nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-all-methods (class prefix)
"(pprint (find-all-methods (find-class 'mtf:<sector>) \"SEC\"))"
  (loop for method in (sb-mop:specializer-direct-methods class)
        for gf           = (sb-mop:method-generic-function method)
        for fname        = (sb-mop:generic-function-name gf)
        for fname-string = (when (symbolp fname) (symbol-name fname))
        when (and (stringp fname-string)
                  (>= (length fname-string)
                      (length prefix))
                  (string= fname-string prefix
                           :end1 (length prefix)
                           :end2 (length prefix)))
        collect method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(find-all-methods class prefix)

(require :temperature-fild/splot)
		  
(codex-documentation-docs :mnas-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require :temperature-fild)

(defparameter *d* (multiple-value-list
		   (trivial-documentation:package-api :temperature-fild)))


(defun td-doc (package)
  (second (multiple-value-list
	   (trivial-documentation:package-api package))))

(defun td-foo (package)
  (let* ((p-list (td-doc package))
	 (p-list-data (cdr p-list)))
    (loop :for i :in p-list      :by #'cddr
	;; :collect i
	;; :collect (format nil "~S" i)
	:collect (mapcar #'string-downcase
			 (mnas-string:split ":" (format nil "~S" i)))
	;; (symbol-name i)
	  )))


(nbutfirst '(1 2 3 4 5 6) 10)
 

(defun nbutfirst (lst &optional (n 1))
  	     (loop :for i :in lst
		   :for j :from 0 :below n
		   :collect i))

(defun td-bar (package)
  (labels ((td-baz (p-list)
	     (nbutfirst p-list)
	     p-list))
    (let ((p-list-data
	    (cdr
	     (second
	      (multiple-value-list
	       (trivial-documentation:package-api package))))))
      (loop :for j :in p-list-data :by #'cddr
	    :collect (apply #'td-baz j)))))

(td-foo :temperature-fild/plot)

(td-bar :temperature-fild/splot)

(td-doc :temperature-fild/splot)

splot-t-fild-relative-rect

TEMPERATURE-FILD/SPLOT:SPLOT-T-FILD-RELATIVE-RECT
 ((:KIND :GENERIC-FUNCTION :LAMBDA-LIST
   (t03 t2 u-tm t-fild &key scale d-pts intervals hights ocr preamble)
   :DOCUMENTATION NIL))

splot-t-fild-relative-rect (t03 t2 u-tm (t-fild mtf/t-fild:<t-fild>)
				       &key
					 (scale mtf/core:*scale-smoothing*)
					 (d-pts (mtf/core:d-pts-scale t-fild :scale scale))
					 (intervals 100)
					 (hights (list 1 0 intervals))
					 (ocr (mtf/core:ocr-intervals t-fild :intervals intervals))
					 (preamble nil))

(mapcar #'string-downcase
	(mnas-string:split ":" (format nil "~S" i)))

(butlast '(1 2 3 4 5) 2)

(documentation (find-package :temperature-fild/splot) t)

(loop :for symbol :being the external-symbols
	:in :temperature-fild/splot :collect symbol)

(loop :for symbol :being the inter-sy
	:in :temperature-fild/splot :collect symbol)
