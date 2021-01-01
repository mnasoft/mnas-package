;;;; temp.lisp

(in-package :mnas-package)

(require :temperature-fild)

(make-codex-documentation :math :internal t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Исследование

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Тестирование
(require :temperature-fild/plot)

(in-package :temperature-fild/plot)

(defparameter *m* (mnas-package::find-all-methods (find-class 'mtf/t-fild:<t-fild>) ""))

(let ((mm (elt *m* 1)))
  (mnas-package::td-standard-method-doc mm))

;;;;;;;;;;;;;;;;;;;;

(let ((mm (elt *m* 1)))
  (mnas-package::td-make-doc
    mm))

;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mnas-package::td-make-docs
 (find-package :mtf/plot)
 (find-class 'mtf/t-fild:<t-fild>) "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require :temperature-fild/splot)
		  
(codex-documentation-docs :mnas-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
