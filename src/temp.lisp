;;;; temp.lisp

(in-package :mnas-package)

(mpkg/view:class-graph :mnas-package/example)

(defun sub-class-graph (class-01)
"@b(Описание:) функция @b(class-undirect-subclasses) возвращает граф
 всех подклассов для класса @b(class-01).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (progn
  (require :mnas-package/example)
  (sub-class-graph (find-class 'mnas-package/example::<a>object))
  (class-undirect-subclasses (find-class 'number)))
@end(code)
"
  (let ((rez-classes nil)
	(l-not-obr (list class-01)))
    (flet
	((bar (class)
	   (setf l-not-obr (append l-not-obr (sb-mop:class-direct-subclasses class)))))
      (do ((class nil))
	  ((null l-not-obr) rez-classes)
	(setf class (pop l-not-obr))
	(push class rez-classes)
	(bar class)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sub-class-graph (class-01 &aux (graph (make-instance 'mnas-graph:<graph>)))
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
     (list class-01)))
  graph)

(sub-class-graph (find-class 'mnas-package/example::<b>))
(defparameter *g*  (sub-class-graph (find-class 'number)))

(mnas-graph:inlet-nodes *g*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun super-class-graph (class-01)
"@b(Описание:) функция @b(class-undirect-subclasses)
 выполняет поиск всех подклассов класса class-01 и 
 возвращает список всех найденных классов.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (progn
  (require :dxf)
  (class-undirect-subclasses (find-class 'dxf::object))
  (class-undirect-subclasses (find-class 'number)))
@end(code)
"
  (let ((rez-classes nil)
	(l-not-obr (list class-01)))
    (flet
	((bar (class)
	   (setf l-not-obr (append l-not-obr (sb-mop:class-direct-subclasses class)))))
      (do ((class nil))
	  ((null l-not-obr) rez-classes)
	(setf class (pop l-not-obr))
	(push class rez-classes)
	(bar class)))))
