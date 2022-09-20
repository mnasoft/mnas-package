;;;; ./src/mnas-package.lisp

(defpackage #:mnas-package
  (:use #:cl ) ;;;; #:mnas-package/make-graph
  (:nicknames "MPKG")

  (:export document)
  (:export copy-doc->public-html)
  (:export rsync-doc)
  (:export make-html-path)
  
  (:intern make-codex-documentation)
  
  (:export sub-class-graph
           super-class-graph)
  (:export make-codex-graphs)
  (:export make-doc-generics
           make-doc-methods)
  (:export make-mainfest-lisp
           find-sources)
  (:export *intranet-hosts*)
  (:documentation
   "@b(Описание:) пакет @b(mnas-package) является основным в системе @b(mnas-package).

 Основными функциями в системе являются:
@begin(list)
 @item(document;)
 @item(make-codex-graphs;)
@end(list)

 Перечисленные ниже функции имеют схожий набор аргументов:
@begin(list)
 @item(document;)
 @item(make-codex-documentation;)
 @item(section-system;)
 @item(section-package;) 
 @item(section-variables;)
 @item(section-functions;) 
 @item(section-macroses;) 
 @item(section-setf-functions;)
 @item(section-generics;) 
 @item(section-setf-generics;)
 @item(section-methods;) 
 @item(section-classes.)
@end(list)

  @b(Аргументы:)
@begin(list)
 @item(package-name - пакет из которого извлекаются
       сущности (глобальными переменными, функциями, и т.д. и т.п.);)
 @item(stream - поток, в который выводятся информация о сущностях;)
 @item(external - если не nil - в поток выводятся информация о
       экспортируемых сущностях;)
 @item(internal - если не nil - в поток выводятся информация о
       внутренних сущностях;)
 @item(inherited - если не nil - в поток выводятся информация о
       заимствованных сущностях;)
 @item(sort - если не nil - сущности сортируются в алфавитном
 порядке;)
 @item(min-doc-length - минимальная длина строки документации,
       связанной с сущностью, при которой созается ссылка указаение на
       вставку документации.)
@end(list)
"
   ))

(in-package #:mnas-package)

(defun codex-docs-pathname (system-designator)
  "@b(Описание:) функция @b(codex-docs-pathname) возвращает строку,
содержащую расположение каталога ./docs системы @b(system-designator) на диске.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (codex-docs-pathname :mnas-package) 
 => \"D:/PRG/msys32/home/namatv/quicklisp/local-projects/mnas/mnas-package/docs\"
@end(code)"  
    (let ((system (asdf:find-system system-designator)))
      (concatenate 'string
		   (namestring (asdf:system-source-directory system))
		   "docs")))

(defun codex-build-pathname (system-designator)
  "@b(Описание:) функция @b(codex-docs-pathname) возвращает строку,
содержащую расположение каталога ./docs системы @b(system-designator) на диске.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (codex-build-pathname :mnas-package) 
 => \"D:/PRG/msys32/home/namatv/quicklisp/local-projects/mnas/mnas-package/docs\"
@end(code)"  
    (let ((system (asdf:find-system system-designator)))
      (concatenate 'string
		   (namestring (asdf:system-source-directory system))
		   "docs/build")))

(defun codex-html-pathname (system-designator)
  "@b(Описание:) функция @b(codex-html-pathname) возвращает строку,
содержащую расположение каталога ./docs/build/mnas-package/html системы 
@b(system-designator) на диске.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (codex-html-pathname :mnas-package) 
 => \"D:/PRG/msys32/home/namatv/quicklisp/local-projects/mnas/mnas-package/docs/build/mnas-package/html\"
@end(code)"
    (let ((system (asdf:find-system system-designator)))
      (concatenate 'string
		   (namestring (asdf:system-source-directory system))
		   "docs/build/"
		   (car (last (mnas-string:split "/" (namestring  (asdf:system-source-directory system-designator)))))
;;;; 		   (string-downcase (package-name (find-package package-designator)))		   
		   "/html")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mk-pathname (dir-list)
  (let ((rez))
    (loop :for i :in dir-list
          :do
             (push "/" rez) (push i rez))
    (apply #'concatenate 'string (nreverse rez))))

(defun remove-msys-prefix (path)
  "@b(Описание:) функция @b(remove-msys-prefix)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (remove-msys-prefix
    \"D:/home/_namatv/PRG/msys64/home/namatv/quicklisp/local-projects/mnas/mnas-package/docs/build/mnas-package/html\")
    => \"/home/namatv/quicklisp/local-projects/mnas/mnas-package/docs/build/mnas-package/html\"

 (remove-msys-prefix
  \"D:/home/_namatv/PRG/msys64/home/namatv/quicklisp/local-projects/mnas/mnas-package/docs/build/mnas-package/html/\")
  => \"/home/namatv/quicklisp/local-projects/mnas/mnas-package/docs/build/mnas-package/html/\"
@end(code)"
  (let ((msystem-prefix (uiop:getenv "MSYSTEM_PREFIX")))
    (if msystem-prefix
        (let ((msystem-prefix-len (1- (length (mnas-string:split "/" msystem-prefix))))
              (path-split (mnas-string:split "/" path :omit-nulls nil)))
          (mk-pathname (nthcdr msystem-prefix-len path-split)))
        path)))

(defun codex-html-pathname/ (system-name)
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
 (codex-html-pathname/ \"mnas-package\") 
 => \"/home/namatv/quicklisp/local-projects/mnas/mnas-package/docs/build/mnas-package/html/\"
@end(code)
"
  (concatenate 'string (codex-html-pathname
                        (asdf:find-system system-name))
               "/"))

(defun copy-doc->public-html (system-name)
  "@b(Описание:) функция @b(copy-doc->public-html) выполняет
  копирование документации системы @b(system-name) в каталог
  ~/public_html/Common-Lisp-Programs."
  (ensure-directories-exist
   (concatenate 'string
                (namestring (uiop/common-lisp:user-homedir-pathname))
                "public_html/Common-Lisp-Programs/"))
  (inferior-shell:run/lines
   `(rsync "-Pazh"
           "--delete"
           ,(remove-msys-prefix (codex-html-pathname/ system-name))
           ,(remove-msys-prefix (concatenate 'string
                                             (namestring (uiop/common-lisp:user-homedir-pathname))
                                             "public_html/Common-Lisp-Programs/"
                                             system-name)))))

(defparameter *intranet-hosts*
  '(("mnasoft-deb"
     "/home/namatv/rclone/db/Public/Common-Lisp-Programs/")
    ("N000308"
     "//n133906/home/_namatv/public_html/Site/Development/Common-Lisp-Programs/")
    ("N142013"
     "//n133906/home/_namatv/public_html/Site/Development/Common-Lisp-Programs/"))
  "@b(Описание:) переменная @b(*intranet-hosts*) определяет имена
 хостов, на которых нет выхода в Интернет.")

(defun rsync-doc (system-name)
  "@b(Описание:) функция @b(rsync-doc) выполняет копирование
  документации на удаленный сервер.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (rsync-doc \"mnas-package\")
@end(code)
"
  (let ((host (find (uiop:hostname) *intranet-hosts* :test #'string= :key #'first)))
    (when host
      (inferior-shell:run/lines `(rsync
                                  "-Pazh"
                                  "--delete"
                                  ,(remove-msys-prefix (codex-html-pathname/ system-name))
                                  ,(concatenate 'string (second host) system-name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-all-generics (class prefix)
  "@b(Описание:) функция @b(find-all-generics) возвращает список
обобщенных функций, связанных с классом @b(class), начинающихся с 
префикса @b(prefix).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (require :temperature-fild/t-fild)
 (find-all-generics (find-class 'mtf/t-fild:<t-fild>) \"SPLOT\")
@end(code)"  
  (loop :for method :in (closer-mop:specializer-direct-methods class)
        :for gf           = (closer-mop:method-generic-function method)
        :for fname        = (closer-mop:generic-function-name gf)
        :for fname-string = (when (symbolp fname) (symbol-name fname))
        :when (and (stringp fname-string)
                   (>= (length fname-string)
                       (length prefix))
                   (string= fname-string prefix
                            :end1 (length prefix)
                            :end2 (length prefix)))
          collect gf))

(defun find-all-methods (class prefix)
  "(pprint (find-all-methods (find-class 'mtf:<sector>) \"SEC\"))"
  (loop :for method :in (closer-mop:specializer-direct-methods class)
        :for gf           = (closer-mop:method-generic-function method)
        :for fname        = (closer-mop:generic-function-name gf)
        :for fname-string = (when (symbolp fname) (symbol-name fname))
        :when (and (stringp fname-string)
                   (>= (length fname-string)
                       (length prefix))
                   (string= fname-string prefix
                            :end1 (length prefix)
                            :end2 (length prefix)))
          collect method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-doc-generics (package class prefix &key (stream t) (min-doc-length mpkg/sec:*min-doc-length*))
    "@b(Описание:) функция @b(make-doc-methods) выводит в поток
@b(stream) раздел документации, подготовленной для вставки в scr-файл
системы документирования codex. Этот раздел содержит обобщенные
функции класса @b(class), имена которых начинаются с префикса
@b(prefix).

 @b(Пример использования:)
@begin[lang=lisp](code)
(make-doc-generics
 (find-package :mnas-package/example)
 (find-class 'mnas-package/example:<c-с-exp>)
 \"\")
@end(code)
->  @cl:with-package[name=\"MNAS-PACKAGE/EXAMPLE\"](
     @cl:doc(generic m-a-exp)
     @cl:doc(generic m-b-exp)
     @cl:doc(generic m-c-exp))
=> #<package \"MNAS-PACKAGE\">
"
#+nil
  (make-doc-generics (find-package :mnas-package/example) (find-class 'mnas-package/example:<c-с-exp>) "") 
  (mnas-package/sec:with-package package
    (mnas-package/sec:with-downcase
      (format stream " @cl:with-package[name=~s](~%" (mpkg/obj:obj-name package))
      (block make-doc-for-generics
        (map 'nil
             #'(lambda (el)
                 (mpkg/sec:insert-codex-doc el
                                            :stream stream
                                            :min-doc-length min-doc-length ))
             (find-all-generics class prefix)))
      (format stream ")~%"))))

(defun make-doc-methods (package class prefix &key (stream t) (min-doc-length mpkg/sec:*min-doc-length*))
  "@b(Описание:) функция @b(make-doc-methods) выводит в поток
@b(stream) раздел документации, подготовленной для вставки в 
scr-файл системы документирования codex. Этот раздел содержит 
методы класса @b(class), имена которых начинаются 
с префикса @b(prefix).

 @b(Пример использования:)
@begin[lang=lisp](code)
(make-doc-generics
 (find-package :mnas-package/example)
 (find-class 'mnas-package/example:<c-с-exp>)
 \"\")
-> @cl:with-package[name=\"MNAS-PACKAGE/EXAMPLE\"](
     @cl:doc(method m-a-exp (x <c-a-int>) (y <c-b-int>) (z <c-с-exp>))
     @cl:doc(method m-b-exp (x <c-a-int>) (y <c-b-int>) (z <c-с-exp>))
     @cl:doc(method m-c-exp (x <c-a-int>) (y <c-b-int>) (z <c-с-exp>)))
=> #<package \"MNAS-PACKAGE\">
@end(code)"
  (mnas-package/sec:with-package package
    (mnas-package/sec:with-downcase
      (format stream " @cl:with-package[name=~S](~%" (mpkg/obj:obj-name package))
      (block make-doc-for-methods
        (map 'nil
             #'(lambda (el)
                 (mpkg/sec:insert-codex-doc el
                                            :stream stream
                                            :min-doc-length min-doc-length))
             (find-all-methods class prefix)))
      (format stream ")~%"))))

(defun make-codex-documentation (package-name
                                 &key (stream t)
                                   (system-name nil)
                                   (external t)
                                   (internal nil)
                                   (inherited nil)
                                   (sort t)
                                   (min-doc-length mpkg/sec:*min-doc-length*)
                                 &aux (package (find-package package-name)))
  "@b(Описание:) функция @b(make-codex-documentation) выводит в поток @b(stream)
секции с документацией в формате codex, содержащие:
@begin(list)
 @item(переменные;)
 @item(функции;)
 @item(макросы;)
 @item(setf-функции;)
 @item(обобщенные функции;)
 @item(методы;)
 @item(классы.)
@end(list)
из пакета @b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-codex-documentation :mnas-package/example :internal t)
@end(code)"
  (when system-name (mpkg/sec:section-system system-name :stream stream))
  (mpkg/sec:section-package package :stream stream
                           :external external :internal internal :inherited inherited
                           :sort sort
                           :min-doc-length min-doc-length))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-codex-graphs (system-designator package-designator
                          &key (external t) (internal t) (inherited nil))
  " @b(Описание:) функция @b(make-codex-graphs) создает в каталоге
./docs/build/mnas-package/html gv-файлы и png-файлы, содержащие графы,
отображающие завмсимости
@begin(list)
 @item(классов;)
 @item(систем;)
 @item(символов;)
 @item(вызовов.)
@end(list)"  
  (let* ((pkg  package-designator)
	 (sys  system-designator)
	 (fpath (codex-html-pathname sys))
	 (pkg-name (mnas-string:replace-all
		    (string-downcase (package-name (find-package pkg)))
		    "/" "-"))
         (call-graph (mpkg/view:call-graph pkg :out-type "png" :viewer nil :fpath fpath
                                               :fname (concatenate 'string "call-graph"  "-" pkg-name)))
         (system-graph (mpkg/view:system-graph sys :out-type "png" :viewer nil :fpath fpath
                                                   :fname (concatenate 'string "system-graph" "-" pkg-name)))
         (class-graph (mpkg/view:class-graph  pkg
                                              :external external :internal internal :inherited inherited
                                              :out-type "png" :viewer nil :fpath fpath
		                              :fname (concatenate 'string "class-graph" "-" pkg-name)))
         (symbol-graph (mpkg/view:symbol-graph pkg :out-type "png" :viewer nil :fpath fpath
			                           :fname (concatenate 'string "symbol-graph" "-" pkg-name))))
    (with-open-file (os (concatenate 'string (codex-docs-pathname sys) "/" pkg-name "-graph.scr")
			:if-exists :supersede :direction :output)
      (format os " ")
      (when (< 0 (+ (mnas-graph:count-nodes system-graph)
                    (mnas-graph:count-nodes call-graph  )
                    (mnas-graph:count-nodes symbol-graph)
                    (mnas-graph:count-nodes class-graph )))
        (format os " @begin(section) @title(Графы ~A)~%" pkg-name)
        (format os "  @begin(list)~%")
        (when (< 0 (mnas-graph:count-nodes system-graph))
          (format os "   @item(system-graph @image[src=./system-graph-~A.gv.png]())~%" pkg-name))
        (when (< 0 (mnas-graph:count-nodes call-graph))
          (format os "   @item(call-graph   @image[src=./call-graph-~A.gv.png]())~%" pkg-name))
        (when (< 0 (mnas-graph:count-nodes symbol-graph))
          (format os "   @item(symbol-graph @image[src=./symbol-graph-~A.gv.png]())~%" pkg-name))
        (when (< 0 (mnas-graph:count-nodes class-graph))
          (format os "   @item(class-graph  @image[src=./class-graph-~A.gv.png]())~%" pkg-name))
        (format os "  @end(list)~% @end(section)")))))

#|
(make-codex-graphs :mnas-package :mnas-package) ;
(< 0 (mnas-graph:count-nodes (mpkg/view:symbol-graph (find-package :mnas-package))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sub-class-graph (class &aux (graph (make-instance 'mnas-graph:<graph>)))
  " @b(Описание:) метод @b(sub-class-graph) возвращает граф,
содержащий иерархию подклассов класса @b(class).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (mnas-graph:view-graph (sub-class-graph (find-class 'mnas-package/example::<a>)))
 (mnas-graph:view-graph (sub-class-graph (find-class 'list)))
@end(code)
"
  (flet ((find-sub-classes (class)
           (let ((from-node (mnas-graph:find-node (string (class-name class)) graph)))
             (when from-node
	       (mapc
	        #'(lambda (el)
		    (mnas-graph:insert-to
		     (make-instance
		      'mnas-graph:<edge>
		      :tail from-node
		      :head (make-instance 'mnas-graph:<node> :owner graph :name (string (class-name el))))
		     graph))
	        (closer-mop:class-direct-subclasses class)))))
         )
    (mnas-graph:insert-to
     (make-instance 'mnas-graph:<node> :owner graph :name (string (class-name class)))
     graph)
    (do* ((classes-tmp (list class)))
         ((null classes-tmp) graph)
      (setf classes-tmp (apply #'append (mapcar  #'(lambda (el) (find-sub-classes el)) classes-tmp))))
    graph))

(defun super-class-graph (class &aux (graph (make-instance 'mnas-graph:<graph>)))
 " @b(Описание:) метод @b(sub-class-graph) возвращает граф,
содержащий иерархию предков для класса @b(class).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (mnas-graph:view-graph (super-class-graph (find-class 'mnas-package/example:<c>)))
 (mnas-graph:view-graph (super-class-graph (find-class 'list)))
@end(code)"
  (flet ((find-super-classes (class)
           (let ((to-node (mnas-graph:find-node (string (class-name class)) graph)))
             (when to-node
	       (mapc
	        #'(lambda (el)
		    (mnas-graph:insert-to
		     (make-instance
		      'mnas-graph:<edge>
		      :tail (make-instance 'mnas-graph:<node> :owner graph :name (string (class-name el)))
		      :head to-node)
		     graph))
	        (closer-mop:class-direct-superclasses class)))))
         )
    (mnas-graph:insert-to
     (make-instance 'mnas-graph:<node> :owner graph :name (string (class-name class)))
     graph)
    (do* ((classes-tmp (list class)))
         ((null classes-tmp) graph)
      (setf classes-tmp (apply #'append (mapcar  #'(lambda (el) (find-super-classes el)) classes-tmp))))
    graph))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun document (package-name system-name                 
                 &key
                   (external t)
                   (internal nil)
                   (inherited nil)
                   (sort t)
                   (min-doc-length mpkg/sec:*min-doc-length*))
  " @b(Описание:) функция @b(document) формирует scr-файл (сценарий
  системы codex), содержащий документацию о пакете @b(package-name) и
  системы системы @b(system-name). Если имя системы равно @b(nil),
  извлечение связанной с ней документации не выполняется.

  @b(Пример использования:)
@begin[lang=lisp](code)
(mnas-package:document :mnas-package :mnas-package) 
 => path_to_mnas-package_system/docs/mnas-package.scr
(mnas-package:document :mnas-package/view nil) 
 => path_to_mnas-package_system/docs/mnas-package-view.scr
@end(code)"
  (with-open-file
      (stream (concatenate 'string
                           (codex-docs-pathname package-name)
                           "/"
                           (mnas-string:replace-all
                            (string-downcase (mpkg/obj:obj-name (find-package package-name))) "/" "-")
                           ".scr")
              :direction :output
              :if-exists :supersede)
    (make-codex-documentation package-name
                              :system-name system-name
                              :stream stream
                              :external external :internal internal :inherited inherited
                              :sort sort :min-doc-length min-doc-length)))

(defparameter +mainfest-lisp-template+
  "(:docstring-markup-format
   :scriba
   :systems ~S
   :documents ((:title ~S
	        :authors ~S
	        :output-format ~S
                :sources ~S
                )))"
 "@b(Описание:) переменая @b(+mainfest-lisp-template+) определяет
 шаблон для создания файла @i(docs/manifest.lisp).
")

(defun find-sources (system)
  (let* ((path-doc  (merge-pathnames #P"docs/" (asdf:system-source-directory (asdf:find-system system))))
         (scr-graph (uiop:directory-files path-doc #P"*-graph.scr"))
         (scr-all   (uiop:directory-files path-doc #P"*.scr"))
         (path-scr (concatenate 'list
                                (sort (set-difference scr-all scr-graph)
                                      #'uiop:timestamp<
                                      :key #'file-write-date)
                                (sort scr-graph  #'uiop:timestamp<
                                      :key #'file-write-date))))
    (mapcar #'(lambda (el)
                (concatenate 'string (pathname-name el) "." (pathname-type el)))
            path-scr)))

(defun make-mainfest-lisp (systems title authors sources
                           &key (output-format '(:type :multi-html :template :minima)))
  "@b(Описание:) функция @b(make-mainfest-lisp) создает файл
  @i(docs/manifest.lisp).

 @b(Переменые:)
@begin(list)
 @item(systems - список систем;)
 @item(title - заголовок;)
 @item(authors - список авторов;)
 @item(sources - список исходных файлов;)
 @item(output-format - атрибуты формата вывода.)
@end(list)


 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-mainfest-lisp 
  '(:MNAS-PACKAGE)
  \"Mnas-Package\"
  '(\"Mykola Matvyeyev\")
  '(\"mnas-file-dialog.scr\" \"mnas-file-dialog-graph.scr\"))
@end(code)
"
  (with-open-file
      (stream
       (merge-pathnames #P"docs/manifest.lisp"
                         (asdf:system-source-directory
                          (asdf:find-system (first systems))))
       :direction :output
       :if-exists :supersede)
    (format stream
            +mainfest-lisp-template+
            systems
            title
            authors
            output-format
            sources)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-html-path (system)
  "@b(Описание:) функция @b(make-html-path) в качестве побочного
   эффекта создает каталог, в который система codex выводит
   html-докуметы.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-html-path :mnas-path)
@end(code)"
  (ensure-directories-exist
   (mnas-package::codex-html-pathname/ system)))

#+nil
(find-sources :mnas-package)
#+nil
(pathname-name
 (second
  (uiop:directory-files #P"D:/home/_namatv/PRG/msys64/home/namatv/quicklisp/local-projects/mnas/mnas-package/docs/"  #P"*-graph.scr")))

#+nil
(pathname-type
 (second
  (uiop:directory-files #P"D:/home/_namatv/PRG/msys64/home/namatv/quicklisp/local-projects/mnas/mnas-package/docs/"  #P"*-graph.scr")))
#+nil
(pathname-directory
 (second
  (uiop:directory-files #P"D:/home/_namatv/PRG/msys64/home/namatv/quicklisp/local-projects/mnas/mnas-package/docs/"  #P"*-graph.scr")))
#+nil
(pathname-device
 (second
  (uiop:directory-files #P"D:/home/_namatv/PRG/msys64/home/namatv/quicklisp/local-projects/mnas/mnas-package/docs/"  #P"*-graph.scr")))
#+nil
(uiop:pathname-root
 (second
  (uiop:directory-files #P"D:/home/_namatv/PRG/msys64/home/namatv/quicklisp/local-projects/mnas/mnas-package/docs/"  #P"*-graph.scr")))
