;;;; codex.lisp

(in-package :mnas-package)

(defun codex-docs-pathname (system-designator)
  "@b(Описание:) функция @b(codex-docs-pathname) возвращает строку,
содержащую расположение каталога ./docs системы @b(system-designator) на диске.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (codex-docs-pathname :mnas-package) 
 => \"D:/PRG/msys32/home/namatv/quicklisp/local-projects/mnas/mnas-package/docs\"
@end(code)
"
    (let ((system (asdf:find-system system-designator)))
      (concatenate 'string
		   (namestring (asdf:system-source-directory system))
		   "docs")))

(defun codex-html-pathname (system-designator)
  "@b(Описание:) функция @b(codex-html-pathname) возвращает строку,
содержащую расположение каталога ./docs/build/mnas-package/html системы 
@b(system-designator) на диске.

 (codex-html-pathname :mnas-package) 
 \"D:/PRG/msys32/home/namatv/quicklisp/local-projects/mnas/mnas-package/docs/build/mnas-package/html\"
 
"
    (let ((system (asdf:find-system system-designator)))
      (concatenate 'string
		   (namestring (asdf:system-source-directory system))
		   "docs/build/"
		   (car (last (mnas-string:split "/" (namestring  (asdf:system-source-directory system-designator)))))
;;;; 		   (string-downcase (package-name (find-package package-designator)))		   
		   "/html")))

(export 'make-codex-graphs)

(defun make-codex-graphs (system-designator package-designator
                          &key
                            (external t)
                            (internal t)
                            (inherited nil)
                            )
  "  @b(Описание:) функция @b(make-codex-graphs) создает в каталоге
./docs/build/mnas-package/html gv-файлы и png-файлы, содержащие графы,
отображающие завмсимости
@begin(list)
 @item(классов;)
 @item(систем;)
 @item(символов;)
 @item(вызовов.)
@end(list)
"
  (let* ((pkg  package-designator)
	 (sys  system-designator)
	 (fpath (codex-html-pathname sys))
	 (pkg-name (mnas-string:replace-all
		    (string-downcase (package-name (find-package pkg)))
		    "/" "-")))
    (view-call-graph   pkg :out-type "png" :viewer nil :fpath fpath
			   :fname (concatenate 'string "call-graph"  "-" pkg-name))
    (view-system-graph sys :out-type "png" :viewer nil :fpath fpath
			   :fname (concatenate 'string "system-graph" "-" pkg-name))
    (view-class-graph  pkg
                       :external external
                       :internal internal
                       :inherited inherited
                       :out-type "png" :viewer nil :fpath fpath
		       :fname (concatenate 'string "class-graph" "-" pkg-name))
    (view-symbol-graph pkg :out-type "png" :viewer nil :fpath fpath
			   :fname (concatenate 'string "symbol-graph" "-" pkg-name))
    (with-open-file (os (concatenate 'string (codex-docs-pathname sys) "/" pkg-name "-graph.scr")
			:if-exists :supersede :direction :output)
      (format os " @begin(section) @title(Графы ~A)
  @begin(list)
   @item(system-graph @image[src=./system-graph-~A.gv.png]())
   @item(call-graph   @image[src=./call-graph-~A.gv.png]())
   @item(symbol-graph @image[src=./symbol-graph-~A.gv.png]())
   @item(class-graph  @image[src=./class-graph-~A.gv.png]())
  @end(list)
 @end(section)" pkg-name pkg-name pkg-name pkg-name pkg-name))))
#|
(make-codex-graphs :mnas-package :mnas-package)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(make-codex-section-classes))

(defun make-codex-section-classes (package-name
                                     &key
                                       (stream t)
                                       (external t)
                                       (internal nil)
                                       (inherited nil)
                                       (sort t)
                                       (min-doc-length 80)
                                     &aux (package (find-package package-name)))
  "@b(Описание:) функция @b(make-codex-section-classes) выводит в поток @b(stream)
секцию с документацией в формате codex, содержащую классы из пакета @b(package-name).

 @b(Переменые:)
@begin(list)
@item(package-name - пакет с функциями для вывода в поток.)
@item(stream       - поток, в который выводятся даннные о функциях.)
@item(external     - если не nil - в поток выводятся информация о эксполртируемых функциях.)
@item(internal     - если не nil - в поток выводятся информация о внутренних функциях.)
@item(inherited    - если не nil - в поток выводятся информация о заимствованных функциях.)
@item(sort         - если не nil - функции сортируются в алфавитном порядке.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (require :dxf)
 (make-codex-section-classes :dxf :internal t)
 @end(code)
"  
  (declare ((or package string symbol) package-name))
  (let ((classes (package-classes package :external external :internal internal :inherited inherited)))
    (format stream "@begin(section)~% @title(Классы)~% @cl:with-package[name=~S]("
	    (obj-name package))
    (map nil #'(lambda (el) (insert-codex-doc el :stream stream :min-doc-length min-doc-length))
	 (if sort
	     (sort classes #'string< :key #'(lambda (elem) (string-downcase (obj-name elem))))
	     classes))
    (format stream ")~%@end(section)~%")))

(export '(make-codex-section-variables))

(defun make-codex-section-variables (package-name
                                     &key
                                       (stream t)
                                       (external t)
                                       (internal nil)
                                       (inherited nil)
                                       (sort t)
                                       (min-doc-length 80)
                                     &aux (package (find-package package-name)))
  "@b(Описание:) функция @b(make-codex-section-classes) выводит в поток @b(stream)
секцию с документацией в формате codex, содержащую переменные из пакета @b(package-name).

 @b(Переменые:)
@begin(list)
@item(package-name - пакет с функциями для вывода в поток.)
@item(stream       - поток, в который выводятся даннные о функциях.)
@item(external     - если не nil - в поток выводятся информация о эксполртируемых функциях.)
@item(internal     - если не nil - в поток выводятся информация о внутренних функциях.)
@item(inherited    - если не nil - в поток выводятся информация о заимствованных функциях.)
@item(sort         - если не nil - функции сортируются в алфавитном порядке.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (require :dxf)
 (make-codex-section-classes :dxf :internal t)
 @end(code)
"  
  (declare ((or package string symbol) package-name))
  (let ((pkg-old *package*)
        (print-case *print-case*))
    (setf *package* package *print-case* :downcase)
    (let ((variables (package-variables package :external external :internal internal :inherited inherited)))
      (format stream "@begin(section)~% @title(Переменные)~% @cl:with-package[name=~S]("
	      (obj-name package))
      (map nil #'(lambda (el) (insert-codex-doc el :stream stream :min-doc-length min-doc-length))
	   (if sort
	       (sort variables #'string< :key #'(lambda (elem) (string-downcase (obj-name elem))))
	       variables))
      (format stream ")~%@end(section)~%"))
    (setf *package* pkg-old *print-case* print-case)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
#|
(require :dxf)
(package-generics :dxf)
(method-name (first (mopp:generic-function-methods (first (package-generics :dxf)))))
|#

(defun package-methods (package-name &key (external t) (internal nil) (inherited nil))
  "@b(Описание:) функция @b(package-methods) возвращает список методов
пакета @b(package-name).

 @b(Переменые:)
@begin(list)
@item(package-name - пакет;) 
@item(external - если равно @b(t) функция возвращает экспортируемые фукции пакета;)
@item(internal - если равно @b(t) функция возвращает внутренние фукции пакета;)
@item(internal - если равно @b(t) импортированные функции пакета.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
  (package-methods :mnas-package :internal t)
   => (#<STANDARD-METHOD MNAS-PACKAGE::DEPENDENCY-TREE (SYMBOL) {1007EE7CA3}>
       ...
       #<STANDARD-METHOD MNAS-PACKAGE:OBJ-PACKAGE-STRING (T) {1007EE79C3}>)
@end(code)
"
  (apply #'append
         (loop :for generic :in (package-generics package-name :external external :internal internal :inherited inherited)
        :collect (mopp:generic-function-methods generic))))

(defun make-codex-section-methods (package-name
                                   &key
                                     (stream    t)
                                     (external  t)
                                     (internal  nil)
                                     (inherited nil)
                                     (sort t)
                                     (min-doc-length 80)
                                   &aux (package (find-package package-name)))
  "@b(Описание:) функция @b(make-codex-section-methods) выводит в поток @b(stream)
секцию с документацией в формате codex, содержащую методы из пакета @b(package-name).

 @b(Переменые:)
@begin(list)
@item(package-name - пакет с функциями для вывода в поток.)
@item(stream       - поток, в который выводятся даннные о функциях.)
@item(external     - если не nil - в поток выводятся информация о эксполртируемых функциях.)
@item(internal     - если не nil - в поток выводятся информация о внутренних функциях.)
@item(inherited    - если не nil - в поток выводятся информация о заимствованных функциях.)
@item(sort         - если не nil - функции сортируются в алфавитном порядке.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (require :temperature-fild)
 (make-codex-section-methods :mtf/splot)
 @end(code)
"
  (declare ((or package string symbol) package-name))
  (let ((pkg-old *package*)
        (print-case *print-case*))
    (setf *package* package *print-case* :downcase)
    (let ((methods (package-methods package :external external :internal internal :inherited inherited)))
      (format stream "@begin(section)~% @title(Методы)~% @cl:with-package[name=~S]("
              (obj-name package))
      (map nil
	   #'(lambda (el)
               (insert-codex-doc el :stream stream :min-doc-length min-doc-length))
	   (if sort
	       (sort methods #'string<
                     :key #'(lambda (elem)
                              (string-downcase (obj-name elem))))
	       methods))
      (format stream ")~%@end(section)~%"))
    (setf *package* pkg-old *print-case* print-case)))

;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(export '(make-codex-documentation))

(defun make-codex-documentation (package-name
                                 &key
                                   (stream t)
                                   (external t)
                                   (internal nil)
                                   (inherited nil)
                                   (sort t)
                                 &aux (package (find-package package-name)))
  "@b(Описание:) функция @b(make-codex-documentation) выводит в поток @b(stream)
секции с документацией в формате codex, содержащие:
@begin(list)
 @item(переменные;)
 @item(функции;)
 @item(обобщенные функции;)
 @item(классы.)
@end(list)
из пакета @b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (require :temperature-fild)
 (make-codex-documentation :mtf/splot)
 (make-codex-documentation :mtf/t-fild)
@end(code)
"
  (make-codex-section-variables package :stream stream :external external :internal internal :inherited inherited :sort sort)
  (make-codex-section-functions package :stream stream :external external :internal internal :inherited inherited :sort sort)
  (make-codex-section-generics  package :stream stream :external external :internal internal :inherited inherited :sort sort)
  (make-codex-section-methods   package :stream stream :external external :internal internal :inherited inherited :sort sort)
  (make-codex-section-classes   package :stream stream :external external :internal internal :inherited inherited :sort sort))
