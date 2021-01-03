;;;; mnas-package.lisp

(in-package #:mnas-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric insert-codex-doc (obj &key stream min-doc-length)
  (:documentation "@b(Описание:) обобщенная функция @b(make-codex-doc)
выводит в поток @b(stream) код для вставки документации, относящейся к 
объекту @b(obj). Документация объекта выводится в поток только если
ее длина превышает @b(min-doc-length).
"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod insert-codex-doc ((function function) &key (stream t) (min-doc-length 80))
  (when (and (eq (mpkg/core:obj-package function) *package*)
             (< min-doc-length (length (documentation function t))))
    (format stream "~%  @cl:doc(function ~s)" (mpkg/core:obj-name function))))

(defmethod insert-codex-doc ((generic standard-generic-function) &key (stream t) (min-doc-length 80))
  (when (and (eq (mpkg/core:obj-package generic) *package*)
             (< min-doc-length (length (documentation generic t))))
    (format stream "~&  @cl:doc(generic ~s)" (mpkg/core:obj-name generic))))

#|
(insert-codex-doc (first (find-all-generics (find-class 'standard-generic-function) "")))
|#

(defmethod insert-codex-doc ((class class) &key (stream t) (min-doc-length 80))
  (when (and (eq (mpkg/core:obj-package class) *package*)
             (< min-doc-length (length (documentation class t))))
    (format stream "~&  @cl:doc(class ~s)" (mpkg/core:obj-name class))))

#|
(mpkg::insert-codex-doc (find-class 'dxf::acad-line))
|#

(defmethod insert-codex-doc ((method method) &key (stream t) (min-doc-length 80))
  "(insert-codex-doc (find-package :mpkg))"
  (when (and (eq (mpkg/core:obj-package method) *package*)
             (< min-doc-length (length (documentation method t))))
    (block method-name
      (format stream "~&  @cl:doc(method ~s" (mpkg/core:obj-name method))
      (let ((mll (mopp:method-lambda-list method))
            (msp (mopp:method-specializers method)))
        (block method-required-args
          (map 'nil
               #'(lambda (name class)
                   (cond
                     ((eq class (find-class t))
                      (format stream " ~s" name))
                     ((not (eq class (find-class t)))
                      (format stream " (~s ~s)" name (mpkg/core:obj-name class)))))
               mll msp))
        (block method-rest-args
          (map 'nil
               #'(lambda (el) (format stream "~a" (format nil " ~s" el)))
               (nthcdr (length msp) mll)))
        (block method-end
          (format stream ")"))))))

(defmethod insert-codex-doc ((package package) &key (stream t) (min-doc-length 80))
  "(insert-codex-doc (find-package :mpkg))"
  (when (and (eq (mpkg/core:obj-package package) *package*)
             (< min-doc-length (length (documentation package t))))
    (format stream "~a" (documentation package t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-codex-section-functions (package-name
                                     &key
                                       (stream t)
                                       (external t)
                                       (internal nil)
                                       (inherited nil)
                                       (sort t)
                                       (min-doc-length 80)
                                     &aux (package (find-package package-name)))
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
  (let ((pkg-old *package*)
        (print-case *print-case*))
    (setf *package* package *print-case* :downcase)
    (let ((funcs (mpkg/pkg:package-functions package :external external :internal internal :inherited inherited)))
      (format stream "@begin(section)~% @title(Функции)~% @cl:with-package[name=~S]("
	      (mpkg/core:obj-name package))
      (map nil #'(lambda (el) (insert-codex-doc el :stream stream :min-doc-length min-doc-length))
	   (if sort
	       (sort funcs #'string< :key #'(lambda (elem) (string-downcase (slynk-backend:function-name elem))))
	       funcs))
      (format stream ")~%@end(section)~%"))
    (setf *package* pkg-old *print-case* print-case)))

#|
(setf *print-case* :upcase)
(make-codex-section-functions :mnas-package)
|#

(defun make-codex-section-generics (package-name
                                    &key
                                      (stream t)
                                      (external t)
                                      (internal nil)
                                      (inherited nil)
                                      (sort t)
                                      (min-doc-length 80)
                                    &aux (package (find-package package-name)))
  "@b(Описание:) функция @b(make-codex-section-generics) выводит в поток stream
секцию с документацией в формате codex, содержащую обобщенные функции из пакета @b(package-name).

 @b(Переменые:)
@begin(list)
@item(package-name   - пакет с функциями для вывода в поток;)
@item(stream         - поток, в который выводятся даннные о функциях;)
@item(external       - если не nil - в поток выводятся информация о эксполртируемых функциях;)
@item(internal       - если не nil - в поток выводятся информация о внутренних функциях;)
@item(inherited      - если не nil - в поток выводятся информация о заимствованных функциях;)
@item(sort           - если не nil - функции сортируются в алфавитном порядке;)
@item(min-doc-length - минимальная длина, при которой вывод документации выполняется.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-codex-section-generics :math/core :sort t) 
@end(code)
"  
  (declare ((or package string symbol) package-name))
  (let ((pkg-old *package*)
        (print-case *print-case*))
    (setf *package* package *print-case* :downcase)
    (let ((g-funcs (mpkg/pkg:package-generics package :external external :internal internal :inherited inherited)))
      (format stream "@begin(section)~% @title(Обобщенные функции)~% @cl:with-package[name=~S]("
              (mpkg/core:obj-name package))
      (map nil #'(lambda (el) (insert-codex-doc el :stream stream :min-doc-length min-doc-length))
	   (if sort
               (sort g-funcs #'string< :key #'(lambda (elem) (string-downcase (mpkg/core:obj-name elem))))
	       g-funcs))
      (format stream ")~%@end(section)~%"))
    (setf *package* pkg-old *print-case* print-case)))

#|
;;;;; Примет использования
(require :math)
(make-codex-section-generics :math/core :sort t) 
|#
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(export 'view-call-graph)

(defun view-call-graph (package-name
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
   (mpkg/make-graph:call-graph package-name)
   :fpath        fpath
   :fname        fname
   :graphviz-prg graphviz-prg
   :out-type     out-type
   :dpi          dpi
   :viewer       viewer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(export 'view-class-graph )

(defun view-class-graph (package-name
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
   (mpkg/make-graph:class-graph package-name :external     external
                                  :internal     internal
                                  :inherited    inherited)
   :fpath        fpath
   :fname        fname
   :graphviz-prg graphviz-prg
   :out-type     out-type
   :dpi          dpi
   :viewer       viewer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(class-undirect-subclasses))

(defun class-undirect-subclasses (class-01)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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
  (mnas-graph:view-graph (mpkg/make-graph:symbol-graph package-name)
			    :fpath        fpath
   :fname        fname
   :graphviz-prg graphviz-prg
   :out-type     out-type
   :dpi          dpi
   :viewer       viewer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
   (mpkg/make-graph:system system)
   :fpath        fpath
   :fname        fname
   :graphviz-prg graphviz-prg
   :out-type     out-type
   :dpi          dpi
   :viewer       viewer))

;;;; (unuse-package :mnas-package/make-graph)
