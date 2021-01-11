;;;; ./src/mnas-package.lisp

(defpackage #:mnas-package
  (:use #:cl ) ;;;; #:mnas-package/make-graph
  (:nicknames "MPKG")
  (:intern insert-codex-doc)
  (:export class-undirect-subclasses)
  (:export make-codex-documentation)
  (:intern make-codex-section-system ;;; Информация о системе пока не реализована
           make-codex-section-package 
           make-codex-section-variables 
           make-codex-section-functions
           make-codex-section-macroses
           make-codex-section-setf-functions
           make-codex-section-generics
           make-codex-section-methods
           make-codex-section-classes)
  (:export make-codex-graphs)
  (:export make-doc-generics
           make-doc-methods)
  (:documentation
   " Система @b(mnas-package) предназначена для подготовки документации,
извлекаемой из asdf-систем.

@begin(section) @title(Мотивация)

 Система @b(Codex) является достаточно удобной для выполнения документирования
систем написанных с использованием @b(Common Lisp). Он позволяет получить на
выходе документацию приемлемого вида.

 К недостатку сустемы @b(Codex) можно отнести то, что формирование шаблона
документации не выполняется автоматически. Указание на включение разделов
документации, относящихся к отдельным сущностям к которым можно отнести:
@begin(list) @item(системы;) @item(пакеты;) @item(классы;) @item(функции,
setf-функции;) @item(обобщенные функции, методы, setf-методы;) @item(макросы;)
@item(и т.д., и т.п.)  @end(list) приходится формировать вручную.

 Этот проект пытается устранить данный недостаток системы @b(Codex) за счет
определения функций и методов позволяющих:
@begin(list)
 @item(формировать код, предназначенный для передачи в систему @b(Codex);)
 @item(формировать представление отдельных частей системы в виде графов.)
@end(list)

@end(section)

 Основными функциями в системе являются:
@begin(list)
 @item(make-codex-documentation;)
 @item(make-codex-graphs;)
@end(list)

 Перечисленные ниже функции имеют схожий набор аргументов:
@begin(list)
 @item(make-codex-documentation;)
 @item(make-codex-section-system;)
 @item(make-codex-section-package;) 
 @item(make-codex-section-variables;)
 @item(make-codex-section-functions;) 
 @item(make-codex-section-macroses;) 
 @item(make-codex-section-setf-functions;)
 @item(make-codex-section-generics;) 
 @item(make-codex-section-methods;) 
 @item(make-codex-section-classes.)
@end(list)

  @b(Аргументы:)
@begin(list)
 @item(package-name - пакет из которого извлекаются сущности (глобальными
       переменными, функциями, и т.д. и т.п.);)
 @item(stream         - поток, в который выводятся информация о сущностях;)
 @item(external - если не nil - в поток выводятся информация о экспортируемых
       сущностях;)
 @item(internal - если не nil - в поток выводятся информация о внутренних
       сущностях;)
 @item(inherited - если не nil - в поток выводятся информация о заимствованных
       сущностях;)
 @item(sort - если не nil - сущности сортируются в алфавитном порядке;)
 @item(min-doc-length - минимальная длина строки документации, связанной с
       сущностью, при которой созается ссылка указаение на вставку
       документации.)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-all-generics (class prefix)
  "@b(Описание:) функция @b(find-all-generics) возвращает список
обобщенных функций, связанных с классом @b(class), начинающихся с 
префикса @b(prefix).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (require :temperature-fild/t-fild)
 (find-all-generics (find-class 'mtf/t-fild:<t-fild>) \"SPLOT\")
@end(code)
"
  (loop :for method :in (sb-mop:specializer-direct-methods class)
        :for gf           = (sb-mop:method-generic-function method)
        :for fname        = (sb-mop:generic-function-name gf)
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
  (loop :for method :in (sb-mop:specializer-direct-methods class)
        :for gf           = (sb-mop:method-generic-function method)
        :for fname        = (sb-mop:generic-function-name gf)
        :for fname-string = (when (symbolp fname) (symbol-name fname))
        :when (and (stringp fname-string)
                   (>= (length fname-string)
                       (length prefix))
                   (string= fname-string prefix
                            :end1 (length prefix)
                            :end2 (length prefix)))
          collect method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric insert-codex-doc (obj &key stream min-doc-length)
  (:documentation "@b(Описание:) обобщенная функция @b(make-codex-doc)
выводит в поток @b(stream) код для вставки документации, относящейся к 
объекту @b(obj). Документация объекта выводится в поток только если
ее длина превышает @b(min-doc-length). 

 Возвращает: 
@begin(list)
 @item(@b(t) - если документация была выведена в поток;)
 @item(@b(nil) - если документация не была выведена в поток.)
@end(list)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod insert-codex-doc ((symbol symbol)
                             &key (stream t) (min-doc-length 80))
  " @b(Пример использования:) 
@begin[lang=lisp](code)
 (mapcar #'insert-codex-doc (mpkg/pkg:package-variables :mnas-package/example :internal t))
@end(code)
"
  (when (< min-doc-length (length (documentation symbol 'variable)))
    (format stream "~%  @cl:doc(variable ~s)" (mpkg/obj:obj-name symbol))
    t))

(defmethod insert-codex-doc ((function function)
                             &key (stream t) (min-doc-length 80))
  (when (< min-doc-length (length (documentation function t)))
    (let ((name (nth-value 2 (function-lambda-expression function))))
      (cond ((symbolp name)
             (format stream "~%  @cl:doc(function ~s)"
                     (mpkg/obj:obj-name function))
             t)
            ((and (listp name) (eq 'macro-function (first name)))
             (format stream "~%  @cl:doc(macro ~s)"
                     (mpkg/obj:obj-name function))
             t)
            ((and (listp name) (eq 'setf (first name)))
             (format stream "~%  @cl:doc(setf-function ~s)"
                     (mpkg/obj:obj-name function))
             t)))))

(defmethod insert-codex-doc ((generic standard-generic-function)
                             &key (stream t) (min-doc-length 80))
  (when (< min-doc-length (length (documentation generic t)))
    (format stream "~&  @cl:doc(generic ~s)" (mpkg/obj:obj-name generic))
    t))

(defmethod insert-codex-doc ((class class) &key (stream t) (min-doc-length 80))
  (when (< min-doc-length (length (documentation class t)))
    (format stream "~&  @cl:doc(class ~s)" (mpkg/obj:obj-name class))
    t))

(defmethod insert-codex-doc ((method method) &key (stream t) (min-doc-length 80))
  "(insert-codex-doc (find-package :mpkg))"
  ;; (break ": ~S" method)
  (when (< min-doc-length (length (documentation method t)))
    (block method-name
      (format stream "~&  @cl:doc(method ~s" (mpkg/obj:obj-name method))
      (let ((mqs (sb-mop:method-qualifiers method)))
        (when (and mqs (listp mqs) (= 1 (length mqs)))
          (format stream " ~s" (mpkg/obj:obj-name (first mqs)))))
      (let ((mll (sb-mop:method-lambda-list method))
            (msp (sb-mop:method-specializers method)))
        (block method-required-args
          (map 'nil
               #'(lambda (name class)
                   (cond
                     ((eq class (find-class t))
                      (format stream " ~s" name))
                     ((not (eq class (find-class t)))
                      (format stream " (~s ~s)" name (mpkg/obj:obj-name class)))))
               mll msp))
        (block method-rest-args
          (map 'nil
               #'(lambda (el) (format stream "~a" (format nil " ~s" el)))
               (nthcdr (length msp) mll)))
        (block method-end
          (format stream ")"))))
    t))

(defmethod insert-codex-doc ((package package)
                             &key (stream t) (min-doc-length 80))
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (insert-codex-doc (find-package :mpkg))
@end(code) "
  (when (< min-doc-length (length (documentation package t)))
    (format stream "~a" (documentation package t))
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-codex-section-classes (package-name
                                   &key
                                     (stream t)
                                     (external t)
                                     (internal nil)
                                     (inherited nil)
                                     (sort nil)
                                     (min-doc-length 80)
                                   &aux (package (find-package package-name)))
  "@b(Описание:) функция @b(make-codex-section-classes) выводит в поток @b(stream)
секцию с документацией в формате codex, содержащую классы из пакета @b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (require :dxf)
 (make-codex-section-classes :dxf :internal t)
 @end(code)
"  
  (declare ((or package string symbol) package-name))
  (let ((print-case *print-case*))
    (setf *print-case* :downcase)
    (let ((classes (mpkg/pkg:package-classes package :external external :internal internal :inherited inherited)))
      (format stream "@begin(section)~% @title(Классы)~% @cl:with-package[name=~S]("
	      (mpkg/obj:obj-name package))
      (map nil #'(lambda (el)
                   (insert-codex-doc el :stream stream :min-doc-length min-doc-length))
	   (if sort
	       (sort classes #'string< :key #'(lambda (elem) (string-downcase (mpkg/obj:obj-name elem))))
	       classes))
      (format stream ")~%@end(section)~%"))
    (setf *print-case* print-case)))

(defun make-codex-section-variables (package-name
                                     &key
                                       (stream t)
                                       (external t)
                                       (internal nil)
                                       (inherited nil)
                                       (sort nil)
                                       (min-doc-length 80)
                                     &aux (package (find-package package-name)))
  "@b(Описание:) функция @b(make-codex-section-classes) выводит в поток @b(stream)
секцию с документацией в формате codex, содержащую переменные из пакета @b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (require :dxf)
 (make-codex-section-classes :dxf :internal t)
 @end(code)
"  
  (declare ((or package string symbol) package-name))
  (let ((print-case *print-case*))
    (setf *print-case* :downcase)
    (let ((variables (mpkg/pkg:package-variables package :external external :internal internal :inherited inherited)))
      (format stream "@begin(section)~% @title(Переменные)~% @cl:with-package[name=~S]("
	      (mpkg/obj:obj-name package))
      (map nil #'(lambda (el) (insert-codex-doc el :stream stream :min-doc-length min-doc-length))
	   (if sort
	       (sort variables #'string< :key #'(lambda (elem) (string-downcase (mpkg/obj:obj-name elem))))
	       variables))
      (format stream ")~%@end(section)~%"))
    (setf *print-case* print-case)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-codex-section-methods (package-name
                                   &key
                                     (stream    t)
                                     (external  t)
                                     (internal  nil)
                                     (inherited nil)
                                     (sort nil)
                                     (min-doc-length 80)
                                   &aux (package (find-package package-name)))
  "@b(Описание:) функция @b(make-codex-section-methods) выводит в поток @b(stream)
секцию с документацией в формате codex, содержащую методы из пакета @b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-codex-section-methods :mnas-package/example :internal t)
 @end(code)
"
  (declare ((or package string symbol) package-name))
  (let ((print-case *print-case*))
    (setf *print-case* :downcase)
    (let ((methods (mpkg/pkg:package-methods package :external external :internal internal :inherited inherited)))
      (format stream "@begin(section)~% @title(Методы)~% @cl:with-package[name=~S]("
              (mpkg/obj:obj-name package))
      (map nil
	   #'(lambda (el)
               (insert-codex-doc el :stream stream :min-doc-length min-doc-length))
	   (if sort
	       (sort methods #'string<
                     :key #'(lambda (elem)
                              (string-downcase (mpkg/obj:obj-name elem))))
	       methods))
      (format stream ")~%@end(section)~%"))
    (setf *print-case* print-case)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-codex-section-package (package-name
                                   &key (stream t) (min-doc-length 80)
                                   &aux (package (find-package package-name)))
  "(make-codex-section-package :mnas-package)"
  (format stream "@begin(section) @title(Обзор)~2%")
  (insert-codex-doc package :stream stream :min-doc-length min-doc-length)
  (format stream "@end(section)~%"))

(defun make-codex-section-functions (package-name
                                     &key
                                       (stream t)
                                       (external t)
                                       (internal nil)
                                       (inherited nil)
                                       (sort nil)
                                       (min-doc-length 80)
                                     &aux (package (find-package package-name)))
  "@b(Описание:) функция make-codex-section-functions выводит в поток stream
секцию с документацией в формате codex, содержащую функции из пакета package-name.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-codex-section-functions :math/stat :external t :internal t :sort t) 
@end(code)
"  
  (declare ((or package string symbol) package-name))
  (let ((print-case *print-case*))
    (setf *print-case* :downcase)
    (let ((funcs (mpkg/pkg:package-functions package :external external :internal internal :inherited inherited)))
      (format stream "@begin(section)~% @title(Функции)~% @cl:with-package[name=~S]("
	      (mpkg/obj:obj-name package))
      (map nil #'(lambda (el) (insert-codex-doc el :stream stream :min-doc-length min-doc-length))
	   (if sort
	       (sort funcs #'string< :key #'(lambda (elem) (string-downcase (slynk-backend:function-name elem))))
	       funcs))
      (format stream ")~%@end(section)~%"))
    (setf *print-case* print-case)))

(defun make-codex-section-macroses (package-name
                                     &key
                                       (stream t)
                                       (external t)
                                       (internal nil)
                                       (inherited nil)
                                       (sort nil)
                                       (min-doc-length 80)
                                     &aux (package (find-package package-name)))
  "@b(Описание:) функция make-codex-section-functions выводит в поток stream
секцию с документацией в формате codex, содержащую функции из пакета package-name.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-codex-section-macroses :mnas-package/example :external t :internal t :sort t)
 (make-codex-section-macroses :mnas-package/example :external t :internal t :sort t :min-doc-length 10) 
@end(code)
"  
  (declare ((or package string symbol) package-name))
  (let ((print-case *print-case*))
    (setf *print-case* :downcase)
    (let ((macroses (mpkg/pkg:package-macroses package :external external :internal internal :inherited inherited)))
      (format stream "@begin(section)~% @title(Макросы)~% @cl:with-package[name=~S]("
	      (mpkg/obj:obj-name package))
      (map nil #'(lambda (el) (insert-codex-doc el :stream stream :min-doc-length min-doc-length))
	   (if sort
	       (sort macroses #'string< :key #'(lambda (elem) (string-downcase (mpkg/obj:obj-name elem))))
	       macroses))
      (format stream ")~%@end(section)~%"))
    (setf *print-case* print-case)))

(defun make-codex-section-setf-functions (package-name
                                          &key
                                            (stream t)
                                            (external t)
                                            (internal nil)
                                            (inherited nil)
                                            (sort nil)
                                            (min-doc-length 80)
                                          &aux (package (find-package package-name)))
  "@b(Описание:) функция @b(make-codex-section-setf-functions) выводит в поток stream
секцию с документацией в формате codex, содержащую setf-функции из пакета package-name.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-codex-section-setf-functions :mnas-package/example :external t :internal t :sort t) 
@end(code)
"  
  (declare ((or package string symbol) package-name))
  (let ((print-case *print-case*))
    (setf *print-case* :downcase)
    (let ((setf-funcs (mpkg/pkg:package-setf-functions package :external external :internal internal :inherited inherited)))
      (format stream "@begin(section)~% @title(Setf Функции)~% @cl:with-package[name=~S]("
	      (mpkg/obj:obj-name package))
      (map nil #'(lambda (el) (insert-codex-doc el :stream stream :min-doc-length min-doc-length))
           (if sort
               (sort setf-funcs #'string< :key #'(lambda (elem) (mpkg/obj:obj-name elem)))
               setf-funcs))
      (format stream ")~%@end(section)~%"))
    (setf *print-case* print-case)))

(defun make-codex-section-generics (package-name
                                    &key
                                      (stream t)
                                      (external t)
                                      (internal nil)
                                      (inherited nil)
                                      (sort nil)
                                      (min-doc-length 80)
                                    &aux (package (find-package package-name)))
  "@b(Описание:) функция @b(make-codex-section-generics) выводит в поток stream
секцию с документацией в формате codex, содержащую обобщенные функции из пакета @b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-codex-section-generics :math/obj :sort t) 
@end(code)
"  
  (declare ((or package string symbol) package-name))
  (let ((print-case *print-case*))
    (setf *print-case* :downcase)
    (let ((g-funcs (mpkg/pkg:package-generics package :external external :internal internal :inherited inherited)))
      (format stream "@begin(section)~% @title(Обобщенные функции)~% @cl:with-package[name=~S]("
              (mpkg/obj:obj-name package))
      (map nil #'(lambda (el) (insert-codex-doc el :stream stream :min-doc-length min-doc-length))
	   (if sort
               (sort g-funcs #'string< :key #'(lambda (elem) (string-downcase (mpkg/obj:obj-name elem))))
	       g-funcs))
      (format stream ")~%@end(section)~%"))
    (setf *print-case* print-case)))

(defun make-doc-generics (package class prefix &key (stream t) (min-doc-length 80))
  "@b(Описание:) функция @b(make-doc-methods) выводит в поток
@b(stream) раздел документации, подготовленной для вставки в 
scr-файл системы документирования codex. Этот раздел содержит 
методы класса @b(class), имена которых начинаются 
с префикса @b(prefix).
"
  (let ((print-case *print-case*))
    (setf *print-case* :downcase)
    (format stream " @cl:with-package[name=~s](~%" (mpkg/obj:obj-name package))
    (block make-doc-for-generics
      (map 'nil #'(lambda (el) (insert-codex-doc el :stream stream :min-doc-length min-doc-length :pkg package))
           (find-all-generics class prefix)))
    (format stream ")~%")
    (setf *print-case* print-case)))

#|
(require :temperature-fild)
(with-open-file (os "~/123.scr" :direction :output :if-exists :supersede)
  (make-doc-generics (find-package 'mtf) (find-class 'mtf/t-fild::<t-fild>) "" :stream os :min-doc-length 50))
|#

(export '(make-doc-methods))

(defun make-doc-methods (package class prefix &key (stream t) (min-doc-length 80))
  "@b(Описание:) функция @b(make-doc-methods) выводит в поток
@b(stream) раздел документации, подготовленной для вставки в 
scr-файл системы документирования codex. Этот раздел содержит 
методы класса @b(class), имена которых начинаются 
с префикса @b(prefix).
"
  (let ((print-case *print-case*))
    (setf *print-case* :downcase)
    (format stream " @cl:with-package[name=~S](~%" (mpkg/obj:obj-name package))
    (block make-doc-for-methods
      (map 'nil
           #'(lambda (el)
               (insert-codex-doc el :stream stream :min-doc-length min-doc-length :pkg package))
           (find-all-methods class prefix)))
    (format stream ")~%")
    (setf *print-case* print-case)))

(defun make-codex-documentation (package-name
                                 &key
                                   (stream t)
                                   (external t)
                                   (internal nil)
                                   (inherited nil)
                                   (sort nil)
                                   (min-doc-length 80)
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
@end(code)
"
  (make-codex-section-package package :stream stream)
  (map nil
       #'(lambda (func)
           (funcall func package :stream stream :external external :internal internal
                                 :inherited inherited :sort sort
                                 :min-doc-length min-doc-length))
       (list #'make-codex-section-variables
             #'make-codex-section-functions 
             #'make-codex-section-macroses  
             #'make-codex-section-setf-functions 
             #'make-codex-section-generics  
             #'make-codex-section-methods   
             #'make-codex-section-classes)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    (mpkg/view:call-graph   pkg :out-type "png" :viewer nil :fpath fpath
			   :fname (concatenate 'string "call-graph"  "-" pkg-name))
    (mpkg/view:system-graph sys :out-type "png" :viewer nil :fpath fpath
			   :fname (concatenate 'string "system-graph" "-" pkg-name))
    (mpkg/view:class-graph  pkg
                       :external external
                       :internal internal
                       :inherited inherited
                       :out-type "png" :viewer nil :fpath fpath
		       :fname (concatenate 'string "class-graph" "-" pkg-name))
    (mpkg/view:symbol-graph pkg :out-type "png" :viewer nil :fpath fpath
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
