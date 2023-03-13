;;;; ./src/make/make.lisp

(defpackage :mnas-package/make
  (:use #:cl ) ;; :mnas-package/pkg
  (:nicknames "MPKG/MAKE")
  (:export system-graph
           symbol-graph
           class-graph
           call-graph
           )
  (:export class-slot-graph
           generic-graph
           )
  (:documentation
   "Пакет @b(mnas-package/make) предназначен для создания графов следующих типов:
@begin(list)
 @item(system-graph     - зависимостей систем;)
 @item(call-graph       - вызовов функций;)
 @item(symbol-graph     - использования символов функциями;)
 @item(class-graph      - наследования классов;)
 @item(class-slot-graph - слотов класса;)
 @item(generic-graph    - типов основных параметров обобщенных функций.)
@end(list)
"))

(in-package :mnas-package/make)

(defun system-graph (system)
  (mnas-graph:make-graph 
   (mapcar
    #'(lambda (el)
	(mapcar #'symbol-name el))
    (reduce
     #'(lambda (x y)
	 (append x (mapcar #'(lambda (el) (list y el)) (mpkg/pkg:dependencies-of y))))
     (append (list system) (mpkg/pkg:dependency-tree system))
     :initial-value (make-list 0)))))

(defun symbol-graph (package-name
			&aux
			  (package (find-package package-name))
			  (pkg-symbols nil))
  (declare ((or package string symbol) package-name))
  (cond
    (package
     (setf pkg-symbols (mpkg/pkg:filter-variables (mpkg/pkg:package-symbols-by-category package)))
     (mnas-graph:make-graph
      (mpkg/pkg:who-references-lst pkg-symbols)
      :nodes (mapcar #'(lambda (el) (mpkg/pkg:func-to-string el)) pkg-symbols)))
    (t (error "~S does not designate a package" package-name))))

(defun class-graph (package-name
                         &key
                           (external t)
                           (internal nil)
                           (inherited nil)
			 &aux
			   (package (find-package package-name))
			   (graph (make-instance 'mnas-graph:<graph>)))
  (declare ((or package string symbol) package-name))
  (flet ((find-subclasses (class)
	   (mapcar
	    #'(lambda (el)
		(mnas-graph:insert-to
		 (make-instance
		  'mnas-graph:<edge>
		  :tail (make-instance 'mnas-graph:<node> :owner graph :name (string (class-name class)))
		  :head (make-instance 'mnas-graph:<node> :owner graph :name (string (class-name el))))
		 graph))
	    (closer-mop:class-direct-subclasses class))
	   graph))
    (mapc
     #'(lambda (el)
  	 (mnas-graph:insert-to
	  (make-instance 'mnas-graph:<node> :owner graph :name (string (class-name el)))
	  graph)
	 (find-subclasses el))
     (mpkg/pkg:package-classes package :external  external :internal  internal :inherited inherited)))
  graph)

(defun call-graph (package-name
			&aux
			  (package (find-package package-name))
			  (pkg-functions nil))
  (declare ((or package string symbol) package-name))
  (cond
    (package
     (setf pkg-functions (mpkg/pkg:filter-functions (mpkg/pkg:package-symbols-by-category package)))
     (mnas-graph:make-graph
      (mpkg/pkg:who-calls-lst pkg-functions)
      :nodes (mapcar #'(lambda (el) (mpkg/pkg:func-to-string el)) pkg-functions)))
    (t (error "~S does not designate a package" package-name))))

(defun class-slot-graph (class
		         &aux
		           (graph (make-instance 'mnas-graph:<graph>)))
  ;;(declare ((or class symbol) class-name))
  (let ((cl-node (make-instance 'mnas-graph:<node> :owner graph :name (string (mnas-package/obj:obj-name class)))))
    (mnas-graph:insert-to cl-node graph)
    (mapc
     #'(lambda (el)
         (mnas-graph:insert-to
	  (make-instance
	   'mnas-graph:<edge>
	   :tail cl-node
	   :head (make-instance 'mnas-graph:<node> :owner graph :name (string (mnas-package/obj:obj-name el))))
	  graph)
         )
     (closer-mop:class-slots class))
    graph))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generic-graph (generic
                      &aux
                        (package *package*)
		        (graph (make-instance 'mnas-graph:<graph>)))
  generic
  graph)
