;;;; ./src/view/view.lisp

(defpackage #:mnas-package/view
  (:use #:cl ) ;; :mnas-package/pkg
  (:nicknames "MPKG/VIEW")
  (:export system-graph
           symbol-graph
           class-graph
           call-graph
           class-slot-graph
           )
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

(in-package :mnas-package/view)

(defun system-graph (system
			     &key
			       (fpath mnas-graph:*output-path*)
			       (fname  (format nil "graph-~6,'0D" (incf mnas-graph::*graph-count*)))
			       (graphviz-prg :filter-dot)
			       (out-type "pdf")
			       (dpi "300")
			       (viewer mnas-graph:*viewer-path*))
"@b(Описание:) system-graph визуализирует граф систем, от которых зависит
система @b(system).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (mnas-package/view:system-graph :mnas-package :out-type \"png\" :viewer nil)
@end(code)
"
  (mnas-graph:view-graph
   (mpkg/make:system-graph system)
   :fpath        fpath
   :fname        fname
   :graphviz-prg graphviz-prg
   :out-type     out-type
   :dpi          dpi
   :viewer       viewer))

#|
 (mnas-package/view:system-graph :mnas-package :out-type "png" :viewer nil)
|#

(defun symbol-graph (package-name
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
  (mnas-graph:view-graph (mpkg/make:symbol-graph package-name)
			 :fpath        fpath
                         :fname        fname
                         :graphviz-prg graphviz-prg
                         :out-type     out-type
                         :dpi          dpi
                         :viewer       viewer))

(defun class-graph (package-name
                    &key
                      (external t)
                      (internal t)
                      (inherited nil)
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
   (mpkg/make:class-graph package-name :external     external
                                       :internal     internal
                                       :inherited    inherited)
   :fpath        fpath
   :fname        fname
   :graphviz-prg graphviz-prg
   :out-type     out-type
   :dpi          dpi
   :viewer       viewer))

(defun call-graph (package-name
			   &key
			     (fpath mnas-graph:*output-path*)
			     (fname  (format nil "graph-~6,'0D" (incf mnas-graph::*graph-count*)))
			     (graphviz-prg :filter-dot)
			     (out-type "pdf")
			     (dpi "300")
			     (viewer mnas-graph:*viewer-path*)
			     (system-name package-name))
" @b(Описание:) функция @b(view-call-graph) выполняет визуализацию графа вызовов 
пакета @b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (view-call-graph :mnas-package)
@end(code)
"
  (when (symbolp package-name) (require system-name))
  (when (stringp package-name) (require system-name))
  (mnas-graph:view-graph
   (mpkg/make:call-graph package-name)
   :fpath        fpath
   :fname        fname
   :graphviz-prg graphviz-prg
   :out-type     out-type
   :dpi          dpi
   :viewer       viewer))

(defun class-slot-graph (class
                         &key
			   (fpath mnas-graph:*output-path*)
			   (fname  (format nil "graph-~6,'0D" (incf mnas-graph::*graph-count*)))
			   (graphviz-prg :filter-dot)
			   (out-type "pdf")
			   (dpi "300")
			   (viewer mnas-graph:*viewer-path*))
  "@b(Описание:) функция @b(class-slot-graph) - ...
"
  (mnas-graph:view-graph
   (mpkg/make:class-slot-graph class)
   :fpath        fpath
   :fname        fname
   :graphviz-prg graphviz-prg
   :out-type     out-type
   :dpi          dpi
   :viewer       viewer))
