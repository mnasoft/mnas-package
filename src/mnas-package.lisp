;;;; mnas-package.lisp

(in-package #:mnas-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(export '(obj-name))

(defgeneric obj-name (obj)
  (:documentation "@b(Описание:) обобщенная функция @b(obj-name)
возвращает символ, представляющий имя объекта obj."))

(export '(obj-name-string))

(defgeneric obj-name-string (obj)
  (:documentation "@b(Описание:) обобщенная функция @b(obj-name-string)
возвращает символ, представляющий имя объекта obj."))

(export '(obj-package))

(defgeneric obj-package (obj)
  (:documentation "@b(Описание:) обобщенная функция @b(obj-package)
возвращает пакет, в котором определен объект obj."))

(export '(obj-package-string))

(defgeneric obj-package-string (obj)
  (:documentation "@b(Описание:) обобщенная функция @b(obj-package-string)
возвращает строку, представляющую имя пакета, в котором определен объект @b(obj)."))

(defgeneric insert-codex-doc (obj &key stream min-doc-length)
  (:documentation "@b(Описание:) обобщенная функция @b(make-codex-doc)
выводит в поток @b(stream) код для вставки документации, относящейся к 
объекту @b(obj). Документация объекта выводится в поток только если
ее длина превышает @b(min-doc-length).
"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod obj-name ((symbol symbol))
  "@b(Описание:) метод @b(obj-name) возвращает символ,
представляющий имя функции."
  symbol)

(defmethod obj-name ((function function))
  "@b(Описание:) метод @b(obj-name) возвращает символ,
представляющий имя функции.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (obj-name (second (package-functions :mnas-package)))
@end(code)"
  (nth-value 2 (function-lambda-expression function)))

(defmethod obj-name ((generic standard-generic-function))
  "@b(Описание:) метод @b(obj-name) возвращает символ,
представляющий имя обобщенной функции.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (require :dxf)
 (obj-name (first (package-generics :dxf)))
@end(code)
"
  (mopp:generic-function-name generic))

(defmethod obj-name ((method method))
      "@b(Описание:) метод @b(obj-name) возвращает символ,
представляющий имя метода.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (require :dxf)
 (obj-name (second (mopp:generic-function-methods (first (package-generics :dxf)))))
@end(code)"
  (mopp:generic-function-name
   (mopp:method-generic-function method)))

(defmethod obj-name ((class class))
  "@b(Описание:) метод @b(obj-name) возвращает символ,
представляющий имя класса.

 @b(Пример использования:)
@begin[lang=lisp](code)
  (obj-name (first (package-classes :dxf :internal t)))
@end(code)"
  (class-name class))

(defmethod obj-name ((package package))
  "@b(Описание:) метод @b(obj-name) возвращает символ,
представляющий имя класса.

 @b(Пример использования:)
@begin[lang=lisp](code)
  (obj-name (find-package :dxf))
@end(code)"
  (package-name package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod obj-name-string ((obj t))
  "@b(Описание:) метод @b(obj-name) возвращает строку,
представляющую имя функции.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (progn
   (require :dxf)
   (obj-name-string (second (package-functions :mnas-package)))
   (obj-name-string (second (package-functions :dxf)))
   (obj-name (first (package-generics :dxf)))
   (obj-name (second (mopp:generic-function-methods (first (package-generics :dxf))))))
@end(code)"
  (format nil "~s" (obj-name obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod obj-package ((function function))
  "@b(Описание:) метод @b(obj-package) возвращает символ,
представляющий имя функции.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (obj-package (second (package-functions :mnas-package)))
@end(code)"
  (symbol-package (obj-name function)))

(defmethod obj-package ((generic standard-generic-function))
    "@b(Описание:) метод @b(obj-package) возвращает символ,
представляющий имя обобщенной функции.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (require :dxf)
 (obj-package (first (package-generics :dxf)))
@end(code)
"
  (symbol-package (obj-name generic)))

(defmethod obj-package ((method method))
      "@b(Описание:) метод @b(obj-package) возвращает символ,
представляющий имя метода.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (require :dxf)
 (obj-package (second (mopp:generic-function-methods (first (package-generics :dxf)))))
@end(code)"
  (symbol-package (obj-name method)))

(defmethod obj-package ((class class))
  "@b(Описание:) метод @b(obj-package) возвращает символ,
представляющий имя класса.

 @b(Пример использования:)
@begin[lang=lisp](code)
  (obj-package (first (package-classes :dxf :internal t)))
@end(code)"
  (symbol-package (obj-name class)))

(defmethod obj-package ((package package))
  "@b(Описание:) метод @b(obj-package) возвращает символ,
представляющий имя класса.

 @b(Пример использования:)
@begin[lang=lisp](code)
  (require :dxf)
  (obj-package (find-package :dxf))
@end(code)"
  package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod obj-package-string ((obj t))
    "@b(Описание:) метод @b(obj-package-string)
возвращает строку, представляющую имя пакета, в котором определен объект @b(obj).

 @b(Пример использования:)
@begin[lang=lisp](code)
  (require :dxf)
  (obj-package-string (find-package :dxf))
@end(code)"
  (package-name (obj-package obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod insert-codex-doc ((function function) &key (stream t) (min-doc-length 80))
  (when (and (eq (obj-package function) *package*)
             (< min-doc-length (length (documentation function t))))
    (format stream "~%  @cl:doc(function ~s)" (obj-name function))))


(defmethod insert-codex-doc ((generic standard-generic-function) &key (stream t) (min-doc-length 80))
  (when (and (eq (obj-package generic) *package*)
             (< min-doc-length (length (documentation generic t))))
    (format stream "~&  @cl:doc(generic ~s)" (obj-name generic))))

#|
(insert-codex-doc (first (find-all-generics (find-class 'standard-generic-function) "")))
|#

(defmethod insert-codex-doc ((class class) &key (stream t) (min-doc-length 80))
  (when (and (eq (obj-package class) *package*)
             (< min-doc-length (length (documentation class t))))
    (format stream "~&  @cl:doc(class ~s)" (obj-name class))))

#|
(mpkg::insert-codex-doc (find-class 'dxf::acad-line))
|#

(defmethod insert-codex-doc ((method method) &key (stream t) (min-doc-length 80))
  "(insert-codex-doc (find-package :mpkg))"
  (when (and (eq (obj-package method) *package*)
             (< min-doc-length (length (documentation method t))))
    (block method-name
      (format stream "~&  @cl:doc(method ~s" (obj-name method))
      (let ((mll (mopp:method-lambda-list method))
            (msp (mopp:method-specializers method)))
        (block method-required-args
          (map 'nil
               #'(lambda (name class)
                   (cond
                     ((eq class (find-class t))
                      (format stream " ~s" name))
                     ((not (eq class (find-class t)))
                      (format stream " (~s ~s)" name (class-name class)))))
               mll msp))
        (block method-rest-args
          (map 'nil
               #'(lambda (el) (format stream "~a" (format nil " ~s" el)))
               (nthcdr (length msp) mll)))
        (block method-end
          (format stream ")"))))))

(defmethod insert-codex-doc ((package package) &key (stream t) (min-doc-length 80))
  "(insert-codex-doc (find-package :mpkg))"
  (when (and (eq (obj-package package) *package*)
             (< min-doc-length (length (documentation package t))))
    (format stream "~s" (documentation package t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun package-symbols-all (package-name &aux (lst nil) (package (find-package package-name)))
"@b(Описание:) package-symbols-all Выполнят поиск всех символов, 
определенных пакетом @b(package-name).

 @b(Пример использования:)
 @begin[lang=lisp](code)
 (package-symbols-all 'mnas-package)
 (package-symbols-all :mnas-package)
 (package-symbols-all (find-package :mnas-package))
 (package-symbols-all \"MNAS-PACKAGE\")
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
      (package-symbols-all package))
     (when external  (setf rez (union rez external-lst)))
     (when internal  (setf rez (union rez internal-lst)))
     (when inherited (setf rez (union rez inherited-lst)))
     rez)
    (t (error "~S does not designate a package" package-name))))

(defun filter-variables (symbols)
"@b(Описание:) функция filter-variables возвращает список символов, 
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

(defun filter-functions (symbols)
"@b(Описание:) функция filter-functions возвращает список символов,
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

(export 'package-variables )

(defun package-variables (package-name &key (external t) (internal nil) (inherited nil))
  "@b(Описание:) функция @b(package-variables) возвращает список символов пакета @b(package-name).

 @b(Переменые:)
@begin(list)
@item(package-name - пакет;) 
@item(external - если равно @b(t) функция возвращает экспортируемые символы пакета;)
@item(internal - если равно @b(t) функция возвращает внутренние символы пакета;)
@item(internal - если равно @b(t) импортированные символы пакета.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (package-variables :mnas-package :inherited t)
@end(code)
"
  (filter-variables
   (package-symbols-by-category
    package-name
    :external external
    :internal internal
    :inherited inherited)))

#|
(package-variables :mnas-package :inherited t)
|#

(export 'package-functions )

(defun package-functions (package-name &key (external t) (internal nil) (inherited nil) )
  "@b(Описание:) функция @b(package-functions) возвращает список функций пакета @b(package-name).

 @b(Переменые:)
@begin(list)
@item(package-name - пакет;) 
@item(external - если равно @b(t) функция возвращает экспортируемые фукции пакета;)
@item(internal - если равно @b(t) функция возвращает внутренние фукции пакета;)
@item(internal - если равно @b(t) импортированные функции пакета.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (package-functions :mnas-package)
  => (#<FUNCTION MAKE-CLASS-GRAPH> #<FUNCTION GENERIC-FUNCTIONS> 
      ...
      #<FUNCTION FUNCTIONS> #<FUNCTION MAKE-SYSTEM-GRAPH>)
@end(code)
"
  (let ((rez nil))
    (map nil
	 #'(lambda (el)
	     (when (equal 'function (type-of (symbol-function el)))
	       (push (symbol-function el) rez)))
	 (filter-functions
	  (package-symbols-by-category
	   package-name
	   :external external
	   :internal internal
	   :inherited inherited)))
    rez))

(export 'make-codex-section-functions)

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
    (let ((funcs (package-functions package :external external :internal internal :inherited inherited)))
      (format stream "@begin(section)~% @title(Функции)~% @cl:with-package[name=~S]("
	      (obj-name package))
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

(export '(package-generics))

(defun package-generics (package-name &key (external t) (internal nil) (inherited nil))
  "@b(Описание:) функция @b(package-generics) возвращает список обобщенных функций
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
  (package-generics :mnas-package :internal t)
   => (#<STANDARD-GENERIC-FUNCTION MNAS-PACKAGE::->KEY (2)>
       ...
       #<STANDARD-GENERIC-FUNCTION MNAS-PACKAGE::DEPENDENCIES-OF (1)>)
@end(code)
"
  (let ((rez nil))
    (map nil
	 #'(lambda (el)
	     (when (equal 'standard-generic-function (type-of (symbol-function el)))
	       (push (ensure-generic-function el) rez)))
	 (filter-functions
	  (package-symbols-by-category
	   package-name
	   :external external
	   :internal internal
	   :inherited inherited)))
    rez))

(export '(make-codex-section-generics))

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
    (let ((g-funcs (package-generics package :external external :internal internal :inherited inherited)))
      (format stream "@begin(section)~% @title(Обобщенные функции)~% @cl:with-package[name=~S]("
              (obj-name package))
      (map nil #'(lambda (el) (insert-codex-doc el :stream stream :min-doc-length min-doc-length))
	   (if sort
               (sort g-funcs #'string< :key #'(lambda (elem) (string-downcase (obj-name elem))))
	       g-funcs))
      (format stream ")~%@end(section)~%"))
    (setf *package* pkg-old *print-case* print-case)))

#|
;;;;; Примет использования
(require :math)
(make-codex-section-generics :math/core :sort t) 
|#


            
            

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
     (setf pkg-functions (filter-functions (package-symbols-by-category package)))
     (mnas-graph:make-graph
      (who-calls-lst
       pkg-functions)
      :nodes (mapcar #'(lambda (el) (func-to-string el)) pkg-functions)))
    (t (error "~S does not designate a package" package-name))))

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
   (make-call-graph package-name)
   :fpath        fpath
   :fname        fname
   :graphviz-prg graphviz-prg
   :out-type     out-type
   :dpi          dpi
   :viewer       viewer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'package-classes)

(defun package-classes (package-name
                        &key
                          (external t)
                          (internal nil)
                          (inherited nil)
			&aux
			  (rez nil)
			  (class nil)
			  (package (find-package package-name)))
  "@b(Описание:) package-classes возвращает список классов пакета.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (package-classes :mtf/t-fild)
 (package-classes :mtf/sector)
@end(code)
"
  (declare ((or package string symbol) package-name))
  (mapc 
   #'(lambda (el)
       (setf class (find-class el nil))
       (when class (push class rez)))
   (package-symbols-by-category
    package
    :external  external 
    :internal  internal
    :inherited inherited))
  rez)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'make-class-graph )

(defun make-class-graph (package-name
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
     (package-classes
      package
      :external  external 
      :internal  internal
      :inherited inherited)))
  graph)

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
   (make-class-graph package-name :external     external
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
     (setf pkg-symbols (filter-variables (package-symbols-by-category package)))
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

(export 'make-system-graph)

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
