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
  (:export *internet-hosts*)
  (:export *intranet-hosts*
           *intranet-server*
           )
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
    (let ((system (asdf:find-system system-designator)))
      (concatenate 'string
		   (namestring (asdf:system-source-directory system))
		   "docs")))

(defun codex-build-pathname (system-designator)
    (let ((system (asdf:find-system system-designator)))
      (concatenate 'string
		   (namestring (asdf:system-source-directory system))
		   "docs/build")))

(defun codex-html-pathname (system-designator)
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
  (let ((msystem-prefix (uiop:getenv "MSYSTEM_PREFIX")))
    (when msystem-prefix
      (mk-pathname
       (nthcdr   
        (1- (length (mnas-string:split "/" msystem-prefix)))
        (mnas-string:split "/" path))))))

(defun codex-html-pathname/ (system-name)
  (concatenate 'string (remove-msys-prefix (codex-html-pathname (asdf:find-system system-name))) "/"))

(defun copy-doc->public-html (system-name)
  (inferior-shell:run/lines
   `(mkdir -p
           ,(concatenate 'string
                         (namestring (uiop/common-lisp:user-homedir-pathname))
                         "public_html/Common-Lisp-Programs/")))
  (inferior-shell:run/lines
   `(rsync "-Pazh"
           "--delete"
           ,(codex-html-pathname/ system-name)  
           ,(remove-msys-prefix (concatenate 'string
                                             (namestring (uiop/common-lisp:user-homedir-pathname))
                                             "public_html/Common-Lisp-Programs/"
                                             system-name)))))

(defparameter *internet-hosts* '("MNASOFT-01" "mnasoft-00"))

(defparameter *intranet-hosts* '("N000308" "N133907"))

(defparameter *intranet-server*
  #+nil
  "//n133619/home/_namatv/public_html/Site/Development/Common-Lisp-Programs/"
  "//n000171/home/_namatv/public_html/Site/Development/Common-Lisp-Programs/"
)

(defun rsync-doc (system-name)
  (when (find (uiop:hostname) *internet-hosts* :test #'string=)
    #+nil (inferior-shell:run/lines `("sh" "rs-pi-html"))
    `(rsync "-Pavzhe"
            "ssh"
            "--delete"
            "~/public_html/Common-Lisp-Programs/"
            "namatv@mnasoft.ddns.mksat.net:/usr/share/nginx/html/Common-Lisp-Programs/"))
  (when (find (uiop:hostname) *intranet-hosts* :test #'string=)
    (inferior-shell:run/lines `(rsync
                                "-Pazh"
                                "--delete"
                                ,(codex-html-pathname/ system-name)
                                ,(concatenate 'string *intranet-server* system-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-all-generics (class prefix)
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
#+nil
  (make-doc-generics (find-package :mnas-package/example) (find-class 'mnas-package/example:<c-с-exp>) "") 
  (with-package package
    (with-downcase
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
#+nil
  (make-doc-methods (find-package :mnas-package/example) (find-class 'mnas-package/example:<c-с-exp>) "")
  (with-package package
    (with-downcase
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
  (when system-name (mpkg/sec:section-system system-name :stream stream))
  (mpkg/sec:section-package package :stream stream
                           :external external :internal internal :inherited inherited
                           :sort sort
                           :min-doc-length min-doc-length))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-codex-graphs (system-designator package-designator
                          &key (external t) (internal t) (inherited nil))
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

(defparameter +mainfest-lisp-template+)

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
  (with-open-file
      (stream
       (merge-pathnames #P"docs/manifest.lisp"
                         (asdf:system-source-directory
                          (asdf:find-system (first systems))))
       #+nil
       (namestring
        )
       :direction :output
       :if-exists :supersede)
    (format stream +mainfest-lisp-template+ systems title authors output-format sources)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-html-path (system)
  (inferior-shell:run/lines
   `(mkdir -p ,(mnas-package::codex-html-pathname system))))

(make-html-path :mnas-path)

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
