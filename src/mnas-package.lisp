;;;; mnas-package.lisp

(in-package #:mnas-package)

(defun package-symbols (package-name &aux (lst nil) (package (find-package package-name)))
"@b(Описание:) package-symbols Выполнят поиск всех символов, 
определенных пакетом @b(package-name).

 @b(Пример использования:)
 @begin[lang=lisp](code)
 (package-symbols 'mnas-package)
 (package-symbols :mnas-package)
 (package-symbols (find-package :mnas-package))
 (package-symbols \"MNAS-PACKAGE\")
@end(code)
"
  (declare ((or package string symbol) package-name))
  (cond
    (package (do-symbols (s package ) (push s lst)) lst)
    (t (error "~S does not designate a package" package-name))))

(defun package-symbols-by-category
    (package-name
     &key (external t) (internal t) (inherited nil)
     &aux
       (external-lst  nil)
       (internal-lst  nil)
       (inherited-lst nil)
       (rez nil)
       (package (find-package package-name)))
  "@b(Описание:) package-symbols-by-category выполнят поиск символов, 
определенных пакетом @b(package-name).

 @b(Переменые:)
@begin(list)
 @item(package-name - имя пакета. Его можно указывать в виде нескольких вариантов:
'mnas-package; :mnas-package; \"MNAS-PACKAGE\". 
В случае указания имени пакета как строки символы должны быть в верхнем регистре;)
 @item(external     - отбирать (t) или не отбирать (nil) внешиние символы;)
 @item(internal     - отбирать (t) или не отбирать (nil) внутренние символы;)
 @item(inherited    - отбирать (t) или не отбирать (nil) заимствованные символы.)
@end(list)
 @b(Пример использования:)
@begin[lang=lisp](code)
 (package-symbols-by-category 'mnas-package :internal nil)                 ;; отбор только внешних символов;
 (package-symbols-by-category :mnas-package)                               ;; отбор внешних и внутренних символов;
 (package-symbols-by-category \"MNAS-PACKAGE\" :internal nil :inherited t) ;; отбор только внешних и заимствованных символов;
@end(code)
"
  (declare ((or package string symbol) package-name))
  (cond
    (package
     (mapc
      #'(lambda (el)
	  (multiple-value-bind (smbl tp) (find-symbol (string el) package)
	    (case tp
	      (:internal  (push smbl internal-lst))
	      (:external  (push smbl external-lst))
	      (:inherited (push smbl inherited-lst)))))
      (package-symbols package))
     (when external  (setf rez (union rez external-lst)))
     (when internal  (setf rez (union rez internal-lst)))
     (when inherited (setf rez (union rez inherited-lst)))
     rez)
    (t (error "~S does not designate a package" package-name))))

(defun package-filter-symbols (symbols)
"@b(Описание:) функция package-filter-symbols возвращает список символов, 
являющихся сопряженными со значениями.

 @b(Переменые:)
@begin(list) 
@item(symbols - список символов пакета.)
@end(list)
"
  (let ((rez nil))
    (mapc
     #'(lambda (el)
	 (when (boundp el)
	   (push el rez)))
     symbols)
    rez))

(defun package-filter-functions (symbols)
"@b(Описание:) функция package-filter-functions возвращает список символов,
являющихся сопряженными с функциями.

 @b(Переменые:)
@begin(list) 
@item(symbols - список символов пакета.)
@end(list)
"
  (let ((rez nil))
    (mapc
     #'(lambda (el) (when (fboundp el) (push el rez)))
     symbols)
    rez))

(export 'symbols )

(defun symbols (package-name &key (external t) (internal nil) (inherited nil))
  "@b(Описание:) функция @b(symbols) возвращает список функций пакета @b(package-name).
"
  (package-filter-symbols
   (package-symbols-by-category
    package-name
    :external external
    :internal internal
    :inherited inherited)))

(export 'functions )

(defun functions (package-name &key (external t) (internal nil) (inherited nil) )
  "@b(Описание:) функция @b(functions) возвращает список функций пакета.

 @b(Переменые:)
@begin(list)
@item(package-name - пакет;) 
@item(external - если равно @b(t) функция возвращает экспортируемые фукции пакета;)
@item(internal - если равно @b(t) функция возвращает внутренние фукции пакета;)
@item(internal - если равно @b(t) импортированные функции пакетаж;)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (functions :mnas-package)
  => (#<FUNCTION MAKE-CLASS-GRAPH> #<FUNCTION GENERIC-FUNCTIONS>
      #<FUNCTION PACKAGE-CLASSES> #<FUNCTION SYMBOLS> #<FUNCTION VIEW-CALL-GRAPH>
      #<FUNCTION MAKE-CODEX-SECTION-FUNCTIONS> #<FUNCTION DOC-TEMPLATE>
      #<FUNCTION USE-MNAS-PACKAGE> #<FUNCTION VIEW-SYSTEM-GRAPH>
      #<FUNCTION UNUSE-MNAS-PACKAGE> #<FUNCTION VIEW-CLASS-GRAPH>
      #<FUNCTION MAKE-SYMBOL-GRAPH> #<FUNCTION MAKE-CALL-GRAPH>
      #<FUNCTION VIEW-SYMBOL-GRAPH> #<FUNCTION MAKE-CODEX-DOCUMENTATION>
      #<FUNCTION FUNCTIONS> #<FUNCTION MAKE-SYSTEM-GRAPH>)
@end(code)
"
  (let ((rez nil))
    (map nil
	 #'(lambda (el)
	     (when (equal 'function (type-of (symbol-function el)))
	       (push (symbol-function el) rez)))
	 (package-filter-functions
	  (package-symbols-by-category
	   package-name
	   :external external
	   :internal internal
	   :inherited inherited)))
    rez))

(export 'make-codex-section-functions)
(require :math/stat)
(defun make-codex-section-functions (package-name &key (stream t) (external t) (internal nil) (inherited nil) (sort t) &aux (package (find-package package-name)))
"@b(Описание:) функция make-codex-section-functions выводит в поток stream
секцию с документацией в формате codex, содержащую функции из пакета package-name.

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
 (make-codex-section-functions :math/stat :external t :internal t :sort t) 
@end(code)
"  
  (declare ((or package string symbol) package-name))
  (let ((funcs (functions package :external external :internal internal :inherited inherited)))
    (format stream "@begin(section)~% @title(Функции)~% @cl:with-package[name=~S]("
	    (string-downcase (package-name (find-package package-name))))
    (map nil
	 #'(lambda (el)
	     (format stream "~%  @cl:doc(function ~A)"
		     (string-downcase (slynk-backend:function-name el))))
	 (if sort
	     (sort funcs #'string< :key #'(lambda (elem) (string-downcase (slynk-backend:function-name elem))))
	     funcs))
    (format stream ")~%@end(section)")))

(export 'generic-functions )

(defun generic-functions (package-name &key (external t) (internal nil) (inherited nil) )
  (let ((rez nil))
    (map nil
	 #'(lambda (el)
	     (when (equal 'standard-generic-function (type-of (symbol-function el)))
	       (push el rez)))
	 (package-filter-functions
	  (package-symbols-by-category
	   package-name
	   :external external
	   :internal internal
	   :inherited inherited)))
    rez))

;;;;(generic-functions :math/stat :external t :internal t  :inherited t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun func-to-string (func)
  (cond
    ((symbolp func)
     (string-downcase (string func)))
    (t
     (string-downcase (format nil "~S" func)))))

(defun defu-defm-name (func)
""
  (cond
    ((listp (first func))
     (second (first func)))
    ((null (listp (first func)))
     (first func))))

(defun who-calls (func)
""
  (let
      (
       ;;;;(rez (swank/backend:who-calls func))
       (rez (slynk-backend:who-calls func))
       (func-str (func-to-string func)))
    (mapcar
     #'(lambda (el1)
	 (list el1 func-str))
     (remove-duplicates 
      (mapcar
       #'(lambda (el)
	   (func-to-string (defu-defm-name el)))
       rez)
      :test #'equal))))

(defun who-calls-lst (func-lst)
""
  (apply #'append
   (mapcar #'who-calls
	   func-lst)))

(export 'make-call-graph )
(defun make-call-graph (package-name
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
     (setf pkg-functions (package-filter-functions (package-symbols-by-category package)))
     (mnas-graph:make-graph
      (who-calls-lst
       pkg-functions)
      :nodes (mapcar #'(lambda (el) (func-to-string el)) pkg-functions)))
    (t (error "~S does not designate a package" package-name))))

(export 'view-call-graph )
(defun view-call-graph (package-name
			   &key
			     (fpath mnas-graph:*output-path*)
			     (fname  (format nil "graph-~6,'0D" (incf mnas-graph::*graph-count*)))
			     (graphviz-prg :filter-dot)
			     (out-type "pdf")
			     (dpi "300")
			     (viewer mnas-graph:*viewer-path*)
			     (system-name package-name)
			     )
"@b(Описание:) view-call-graph выполняет визуализацию графа вызовов 
пакета @b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (view-call-graph :mnas-package)
@end(code)
"
  (when (symbolp package-name) (require system-name))
  (when (stringp package-name) (require system-name))
  (mnas-graph:view-graph
   (make-call-graph package-name)
   :fpath        fpath
   :fname        fname
   :graphviz-prg graphviz-prg
   :out-type     out-type
   :dpi          dpi
   :viewer       viewer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'package-classes )
(defun package-classes (package-name
			&aux
			  (rez nil)
			  (class nil)
			  (package (find-package package-name)))
"@b(Описание:) package-classes возвращает список классов пакета.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (mnas-package::package-classes :mnas-package)
 (mnas-package::package-classes (find-package \"MNAS-PACKAGE\"))
 (mnas-package::package-classes (find-package :mnas-package))
@end(code)
"
  (declare ((or package string symbol) package-name))
  (mapc 
   #'(lambda (el)
       (setf class (find-class el nil))
       (when class (push class rez)))
   (package-symbols-by-category package))
  rez)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'make-class-graph )
(defun make-class-graph (package-name
			 &aux
			   (package (find-package package-name))
			   (graph (make-instance 'mnas-graph:<graph>)))
"@b(Описание:) make-class-graph создает граф наследования классов.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-class-graph :mnas-package)
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
     (package-classes package)))
  graph)

(export 'view-class-graph )
(defun view-class-graph (package-name
			 &key
			   (fpath mnas-graph:*output-path*)
			   (fname  (format nil "graph-~6,'0D" (incf mnas-graph::*graph-count*)))
			   (graphviz-prg :filter-dot)
			   (out-type "pdf")
			   (dpi "300")
			   (viewer mnas-graph:*viewer-path*))
"@b(Описание:) view-class-graph выводит визуальное представление 
иерархии классов (графа наследования).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (mnas-package:mnas-package-demo-11)
@end(code)
"
  (when (symbolp package-name) (require package-name))
  (when (stringp package-name) (require package-name))
  (mnas-graph:view-graph
   (make-class-graph package-name)
   :fpath        fpath
   :fname        fname
   :graphviz-prg graphviz-prg
   :out-type     out-type
   :dpi          dpi
   :viewer       viewer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun class-undirect-subclasses (class-01)
"@b(Описание:) class-undirect-subclasses выполняет поиск всех 
подклассов класса class-01 и возвращает список всех найденных классов.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (class-undirect-subclasses (find-class 'number))
@end(code)
"
  (let ((rez-classes nil)
	(l-not-obr (list class-01)))
    (flet
	((bar (class)
	   (format t "~S~%" (class-name class))
	   (setf l-not-obr (append l-not-obr (sb-mop:class-direct-subclasses class)))))
      (do ((class nil))
	  ((null l-not-obr) rez-classes)
	(setf class (pop l-not-obr))
	(push class rez-classes)
	(bar class)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-mnas-systems ()
"Необходимо сделать описание"
  (let* ((sos (make-string-output-stream))
	 (sis (make-string-input-stream 
	       (progn
		 (when (uiop:directory-exists-p (pathname "d:/PRG/msys32/home/namatv/quicklisp/local-projects/mnas/mnas-systems"))
		   (uiop:delete-directory-tree (pathname "d:/PRG/msys32/home/namatv/quicklisp/local-projects/mnas/mnas-systems/") :validate t))
		 (sb-ext:run-program
		  (cond
		    ((uiop:os-windows-p) "d:/PRG/msys32/usr/bin/bash.exe")
		    (t "/bin/bash"))
		  '("-c" "find /home/namatv/quicklisp/local-projects/ -name '*.asd'") :output sos)
		 (get-output-stream-string sos))))
	 (asd (loop for line = (read-line sis nil nil)
		 while line
		 collect (pathname-name line))))
    (ensure-directories-exist
     (pathname
      (concatenate 'string (namestring (user-homedir-pathname)) "quicklisp/local-projects/mnas/mnas-systems/")))
    (with-open-file
	(asd-file
	 (pathname (concatenate 'string (namestring (user-homedir-pathname)) "quicklisp/local-projects/mnas/mnas-systems/" "mnas-systems.asd"))
	 :direction :output :if-exists :supersede)
      (format asd-file ";;;; mnas-systems.asd~%~%")
      (format asd-file "(defsystem #:mnas-systems~%")
      (format asd-file "  :components ((:file \"mnas-systems\"))~%")
      (format asd-file "  :depends-on (~%")
      (loop for i in asd
	 do (format asd-file "	       #:~a~%" i))
      (format asd-file "  ))"))
    (with-open-file
	(lisp-file
	 (pathname (concatenate 'string (namestring (user-homedir-pathname)) "quicklisp/local-projects/mnas/mnas-systems/" "mnas-systems.lisp"))
	 :direction :output :if-exists :supersede)
      (format lisp-file ";;;; mnas-systems.lisp~%~%")
      (format lisp-file "(defpackage #:mnas-systems)~%~%")
      (format lisp-file "(in-package #:mnas-systems)"))))

;;; (make-mnas-systems)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun who-references (var)
"Выполняет поиск функций, в которых есть ссылка на внешнюю переменную var.
Возвращает список, каждым элементом которого является список следующего формата:
 (функция переменная).
Пример использования:
 (who-references '*sample-var*) 
 => ((\"who-references\" \"*sample-var*\"))
"
  (let
      (
       ;;;;(rez (swank/backend:who-references var))
       (rez (slynk-backend:who-references var))
       (func-str (func-to-string var)))
    (mapcar
     #'(lambda (el1)
	 (list el1 func-str))
     (remove-duplicates 
      (mapcar
       #'(lambda (el)
	   (func-to-string (defu-defm-name el)))
       rez)
      :test #'equal))))

(defun who-references-lst (var-lst)
"who-references-lst"
  (apply #'append
   (mapcar #'who-references
	   var-lst)))

(export 'make-symbol-graph )
(defun make-symbol-graph (package-name
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
     (setf pkg-symbols (package-filter-symbols (package-symbols-by-category package)))
     (mnas-graph:make-graph
      (who-references-lst
       pkg-symbols)
      :nodes (mapcar #'(lambda (el) (func-to-string el)) pkg-symbols)))
    (t (error "~S does not designate a package" package-name))))

(export 'view-symbol-graph )
(defun view-symbol-graph (package-name
			     &key
			       (fpath mnas-graph:*output-path*)
			       (fname  (format nil "graph-~6,'0D" (incf mnas-graph::*graph-count*)))
			       (graphviz-prg :filter-dot)
			       (out-type "pdf")
			       (dpi "300")
			       (viewer mnas-graph:*viewer-path*))
"@b(Описание:) view-symbol-graph отображает граф зависимостей глобальных символов.

 Позволяет ответить на вопрос: в какой функции используется тот или иной глобальный символ. 

 @b(Пример использования:)
@begin[lang=lisp](code)
 (view-symbol-graph :mnas-package)
@end(code)
"
  (when (symbolp package-name) (require package-name))
  (when (stringp package-name) (require package-name))
  (mnas-graph:view-graph (make-symbol-graph package-name)
			    :fpath        fpath
   :fname        fname
   :graphviz-prg graphviz-prg
   :out-type     out-type
   :dpi          dpi
   :viewer       viewer))

(export 'doc-template )
(defun doc-template (&optional (pkg *package*))
"Пример использования:
@begin[lang=lisp](code)
 (doc-template)
@end(code)
"
  (let ((f-b nil)
	(b   nil))
    (map 'nil
	 #'(lambda (el)
	     (when (fboundp (read-from-string el)) (push el f-b))
	     (when (boundp  (read-from-string el)) (push el   b)))
	 (let ((lst ()))                                                     
	   (do-external-symbols (s pkg)
	     (when (eq (find-package pkg) (symbol-package s)) (push (string-downcase (symbol-name s)) lst)))
	   (sort lst #'string> )))
    (format t "@cl:with-package[name=~S](~%" (string-downcase (package-name pkg)))
    (map 'nil #'(lambda (el) (format t "@cl:doc(function ~a)~%" el) ) f-b)
    (format t ")~%~%")
    (format t "@cl:with-package[name=~S](~%" (string-downcase (package-name pkg)))
    (map 'nil #'(lambda (el) (format t "@cl:doc(variable ~a)~%" el) )   b)
    (format t ")~%~%")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric ->key (thing))

(defmethod ->key ((thing string))
  (intern (string-upcase thing) :keyword))

(defmethod ->key ((thing symbol))
  (if (keywordp thing)
      thing
      (intern (symbol-name thing) :keyword)))

;;;; (defmethod ->key ((thing cons)) (second thing))

;;;; (defmethod ->key ((thing string)) thing)

(defgeneric dependencies-of (system))

(defmethod dependencies-of ((system symbol))
  (mapcar #'->key (slot-value (asdf/system:find-system system) 'asdf/component:sideway-dependencies)))

(defun ordered-dep-tree (dep-tree)
  (let ((res))
    (labels ((in-res? (dep-name) (member dep-name res))
             (insert-pass (remaining)
                (loop for (dep . sub-deps) in remaining
                      for unmet-sub-deps = (remove-if #'in-res? sub-deps)
                      if (null unmet-sub-deps) do (push dep res)
                      else collect (cons dep unmet-sub-deps) into next-rems
                      finally (return next-rems))))
      (loop for (dep . callers) in dep-tree for deps-of = (dependencies-of dep)
            if (null deps-of) do (push dep res)
            else collect (cons dep deps-of) into non-zeros
            finally (loop while non-zeros
                          do (setf non-zeros (insert-pass non-zeros)))))
      (reverse res)))

(defgeneric dependency-tree (system))

(defmethod dependency-tree ((system symbol))
  (let ((res (make-hash-table)))
    (labels ((rec (sys) 
               (loop with deps = (dependencies-of sys)
                  for dep in deps for dep-k = (->key dep)
                  unless (gethash dep-k res) do (rec dep)
                  do (pushnew (->key sys) (gethash dep-k res)))))
      (rec system))
    (ordered-dep-tree (alexandria:hash-table-alist res))))

(export 'make-system-graph )
(defun make-system-graph (system)
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
	 (append x (mapcar #'(lambda (el) (list y el)) (dependencies-of y))))
     (append (list system) (dependency-tree system))
     :initial-value (make-list 0)))))

(export 'view-system-graph )
(defun view-system-graph (system
			     &key
			       (fpath mnas-graph:*output-path*)
			       (fname  (format nil "graph-~6,'0D" (incf mnas-graph::*graph-count*)))
			       (graphviz-prg :filter-dot)
			       (out-type "pdf")
			       (dpi "300")
			       (viewer mnas-graph:*viewer-path*))
"@b(Описание:) view-system-graph визуализирует граф систем, от которых зависит
система @b(system).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (mnas-package:view-system-graph :mnas-package :out-type \"png\" :viewer nil)
@end(code)
"
  (mnas-graph:view-graph
   (make-system-graph system)
   :fpath        fpath
   :fname        fname
   :graphviz-prg graphviz-prg
   :out-type     out-type
   :dpi          dpi
   :viewer       viewer))
