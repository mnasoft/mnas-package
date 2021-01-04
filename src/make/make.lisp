;;;; ./src/make/make.lisp

(defpackage #:mnas-package/make
  (:use #:cl ) ;; :mnas-package/pkg
  (:nicknames "MPKG/MAKE")
  (:export system-graph
           symbol-graph
           class-graph
           call-graph)
  (:documentation
   "Система mnas-package предназначена для извлечения информации из asdf-систем.

 Извлеченная информация представляется в виде графов.

 Система позволяет построить следующие графы:
@begin(list)
 @item(зависимостей систем @image[src=./system-graph-mnas-package.gv.png]())
 @item(вызовов функций     @image[src=./call-graph-mnas-package.gv.png]())
 @item(использования символов функциями @image[src=./symbol-graph-mnas-package.gv.png]())
 @item(наследования классов  @image[src=./class-graph-mnas-package.gv.png]())
@end(list)"
   ))

(in-package :mnas-package/make)

(defun system-graph (system)
"@b(Описание:) make-system-graph возвращает граф систем, от которых зависит
система @b(system).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (mnas-package:make-system-graph :mnas-package)
@end(code)
"
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
"@b(Описание:) make-symbol-graph строит граф использования методпми и функциями 
внешних символов.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-symbol-graph :mnas-string)
@end(code)
"
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
  "@b(Описание:) make-class-graph создает граф наследования классов.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-class-graph :mnas-package )
@end(code)
"
  (declare ((or package string symbol) package-name))
  (flet ((find-subclasses (class)
	   (mapcar
	    #'(lambda (el)
		(mnas-graph:insert-to
		 (make-instance
		  'mnas-graph:<edge>
		  :from (make-instance 'mnas-graph:<node> :owner graph :name (string (class-name class)))
		  :to   (make-instance 'mnas-graph:<node> :owner graph :name (string (class-name el))))
		 graph))
	    (sb-mop:class-direct-subclasses class))
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
"@b(Описание:) make-call-graph возвращает граф вызовов пакета @b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (mnas-package:make-call-graph :mnas-package)
@end(code)
"
  (declare ((or package string symbol) package-name))
  (cond
    (package
     (setf pkg-functions (mpkg/pkg:filter-functions (mpkg/pkg:package-symbols-by-category package)))
     (mnas-graph:make-graph
      (mpkg/pkg:who-calls-lst pkg-functions)
      :nodes (mapcar #'(lambda (el) (mpkg/pkg:func-to-string el)) pkg-functions)))
    (t (error "~S does not designate a package" package-name))))
