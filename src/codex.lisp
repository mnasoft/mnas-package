;;;; codex.lisp

(in-package :mnas-package)

(defun codex-documentation-docs (system-designator)
  "@b(Описание:) функция @b(codex-documentation-docs) возвращает строку,
содержащую расположение каталога ./docs системы @b(system-designator) на диске.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (codex-documentation-docs :mnas-package) 
 => \"D:/PRG/msys32/home/namatv/quicklisp/local-projects/mnas/mnas-package/docs\"
@end(code)
"
    (let ((system (asdf:find-system system-designator)))
      (concatenate 'string
		   (namestring (asdf:system-source-directory system))
		   "docs")))

(defun codex-documentation-html (system-designator)
  "@b(Описание:) функция @b(codex-documentation-html) возвращает строку,
содержащую расположение каталога ./docs/build/mnas-package/html системы 
@b(system-designator) на диске.

 (codex-documentation-html :mnas-package) 
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

(defun make-codex-graphs (system-designator package-designator)
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
	 (fpath (codex-documentation-html sys))
	 (pkg-name (mnas-string:replace-all
		    (string-downcase (package-name (find-package pkg)))
		    "/" "-")))
    (view-call-graph   pkg :out-type "png" :viewer nil :fpath fpath
			   :fname (concatenate 'string "call-graph"  "-" pkg-name))
    (view-system-graph sys :out-type "png" :viewer nil :fpath fpath
			   :fname (concatenate 'string "system-graph" "-" pkg-name))
    (view-class-graph  pkg :out-type "png" :viewer nil :fpath fpath
			   :fname (concatenate 'string "class-graph" "-" pkg-name))
    (view-symbol-graph pkg :out-type "png" :viewer nil :fpath fpath
			   :fname (concatenate 'string "symbol-graph" "-" pkg-name))
    (with-open-file (os (concatenate 'string (codex-documentation-docs sys) "/" pkg-name "-graph.scr")
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
