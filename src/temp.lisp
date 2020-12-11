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

(require :temperature-fild/plot)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;home
(require :temperature-fild)
(defparameter *m*
  (let* ((m-all (find-all-methods (find-class 'mtf/t-fild:<t-fild>) ""))
         (m-0 (elt m-all 0))
         (m-0-type (type-of m-0))
         (m-0-doc (when (member m-0-type '(standard-method))
                    (documentation m-0 t)))
         (m-0-ll (mopp:method-lambda-list m-0))
         (m-0-nm (mopp:generic-function-name
                  (mopp:method-generic-function m-0))))
    m-all
    m-0
    m-0-type
    m-0-doc
    m-0-ll
    m-0-nm
    ))

(type-of (first (find-all-methods (find-class 'mtf/sector:<sector>) "")))
(sb-mop:method-lambda-list (first (find-all-methods (find-class 'mtf/sector:<sector>) "")))
(type-of (first (find-all-methods (find-class 'mtf/sector:<sector>) "")))
(documentation (first (find-all-methods (find-class 'mtf/sector:<sector>) "")) t)
'STANDARD-METHOD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(find-all-methods class prefix)

(require :temperature-fild/splot)
		  
(codex-documentation-docs :mnas-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *a* (getf  *d* (first *d*) "SSSS"))

(defparameter *d* (multiple-value-list
		   (trivial-documentation:package-api :temperature-fild)))


(defun td-foo (package)
  (let ((p-list (second
		  (multiple-value-list
		   (trivial-documentation:package-api package))))
	 (p-list-data (cdr p-list)))
    (loop :for i :in p-list      :by #'cddr
	;; :collect i
	;; :collect (format nil "~S" i)
	:collect (mapcar #'string-downcase
			 (mnas-string:split ":" (format nil "~S" i)))
	;;(symbol-name i)
	  )))

(defun td-bar (package)
  (let ((p-list-data (cdr (second
			   (multiple-value-list
			    (trivial-documentation:package-api package))))))
    (loop :for j :in p-list-data :by #'cddr
	  :collect (apply #'td-baz j)
	  )))

(defun td-baz (p-list)
  p-list
  )

(td-foo :temperature-fild/plot)

(td-bar :temperature-fild/splot)

(mapcar #'string-downcase
	(mnas-string:split ":" (format nil "~S" i)))

(symbol-plist 


