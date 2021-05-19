;;;; ./src/mnas-package.lisp

(defpackage #:mnas-package
  (:use #:cl ) ;;;; #:mnas-package/make-graph
  (:nicknames "MPKG")
  (:intern insert-codex-doc)
  (:export document)
  (:intern make-codex-documentation
           section-system 
           section-package)
  (:intern section-variables 
           section-functions
           section-macroses
           section-setf-functions
           section-generics
           section-setf-generics
           section-methods
           section-setf-methods
           section-classes)
  (:export sub-class-graph
           super-class-graph)
  (:export make-codex-graphs)
  (:export make-doc-generics
           make-doc-methods)
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

(defmacro with-downcase (&body body)
  (let ((print-case (gensym)))
    `(let ((,print-case *print-case*))
       (setf *print-case* :downcase)
       ,@body
       (setf *print-case* ,print-case))))

(defmacro with-package (package-new &body body)
  (let ((package-old (gensym)))
    `(let ((,package-old *package*))
       (setf *package* ,package-new)
       ,@body
       (setf *package* ,package-old))))

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

(defun codex-build-pathname (system-designator)
  "@b(Описание:) функция @b(codex-docs-pathname) возвращает строку,
содержащую расположение каталога ./docs системы @b(system-designator) на диске.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (codex-build-pathname :mnas-package) 
 => \"D:/PRG/msys32/home/namatv/quicklisp/local-projects/mnas/mnas-package/docs\"
@end(code)
"
    (let ((system (asdf:find-system system-designator)))
      (concatenate 'string
		   (namestring (asdf:system-source-directory system))
		   "docs/build")))

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
#+ nil
(defmethod insert-codex-doc ((generic standard-generic-function)
                             &key (stream t) (min-doc-length 80))
  (when (< min-doc-length (length (documentation generic t)))
    (format stream "~&  @cl:doc(generic ~s)" (mpkg/obj:obj-name generic))
    t))

(defmethod insert-codex-doc ((generic standard-generic-function)
                             &key (stream t) (min-doc-length 80))
  (when (< min-doc-length (length (documentation generic t)))
    (let ((name (mpkg/obj:obj-name generic)))
      (cond
        ((and (listp name) (eq 'setf (first name)))
#+ nil   (break "0001: ~S" name)
         (format stream "~&  @cl:doc(setf-generic ~s)" (second name)))
        (t
         (format stream "~&  @cl:doc(generic ~s)" name))))
    t))

(defmethod insert-codex-doc ((class class) &key (stream t) (min-doc-length 80))
  (when (< min-doc-length (length (documentation class t)))
    (format stream "~&  @cl:doc(class ~s)" (mpkg/obj:obj-name class))
    t))

(defmethod insert-codex-doc ((method method) &key (stream t) (min-doc-length 80))
  "(insert-codex-doc (find-package :mpkg))"
  (when (< min-doc-length (length (documentation method t)))
    (let ((mqs (closer-mop::method-qualifiers method))
          (mll (closer-mop:method-lambda-list method))
          (msp (closer-mop:method-specializers method)))
      (unless (and mqs (listp mqs) (= 1 (length mqs)))
        (let ((name (mpkg/obj:obj-name method)))
          (cond
            ((and (listp name) (eq 'setf (first name)))
             (format stream "~&  @cl:doc(setf-method ~s" (second name)))
            (t
             (format stream "~&  @cl:doc(method ~s" name))))
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
          (format stream ")")))
      t)))

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

(defun section-classes (package-name
                        &key
                          (stream t)
                          (external t)
                          (internal nil)
                          (inherited nil)
                          (sort nil)
                          (min-doc-length 80)
                        &aux (package (find-package package-name)))
  "@b(Описание:) функция @b(section-classes) выводит в поток @b(stream)
секцию с документацией в формате codex, содержащую классы из пакета @b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (require :dxf)
 (section-classes :dxf :internal t)
 @end(code)
"  
  (declare ((or package string symbol) package-name))
  (with-package package
    (with-downcase
      (let ((classes (mpkg/pkg:package-classes package :external external :internal internal :inherited inherited)))
        (when (some
               #'(lambda (class)
                   (when (< min-doc-length (length (documentation class t))) t))
               classes)
          (format stream "@begin(section)~% @title(Классы)~% @cl:with-package[name=~S]("
	          (mpkg/obj:obj-name package))
          (map nil #'(lambda (el)
                       (insert-codex-doc el :stream stream :min-doc-length min-doc-length))
	       (if sort
	           (sort classes #'string< :key #'(lambda (elem) (string-downcase (mpkg/obj:obj-name elem))))
	           classes))
          (format stream ")~%@end(section)~%"))))))

;;(when (some #'(lambda (class) (when (< min-doc-length (length (documentation class t))) t)) (mpkg/pkg:package-classes :mnas-package :external t :internal nil)))

(defun section-variables (package-name
                          &key
                            (stream t)
                            (external t)
                            (internal nil)
                            (inherited nil)
                            (sort t)
                            (min-doc-length 80)
                          &aux (package (find-package package-name)))
  "@b(Описание:) функция @b(section-classes) выводит в поток @b(stream)
секцию с документацией в формате codex, содержащую переменные из пакета @b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (require :dxf)
 (section-classes :dxf :internal t)
 @end(code)
"  
  (declare ((or package string symbol) package-name))
  (with-package package
    (with-downcase
      (let ((variables (mpkg/pkg:package-variables package :external external :internal internal :inherited inherited)))
        (when (some
               #'(lambda (var)
                   (when (< min-doc-length (length (documentation var 'variable))) t))
               variables)
          (format stream "@begin(section)~% @title(Переменные)~% @cl:with-package[name=~S]("
	          (mpkg/obj:obj-name package))
          (map nil #'(lambda (el) (insert-codex-doc el :stream stream :min-doc-length min-doc-length))
	       (if sort
	           (sort variables #'string< :key #'(lambda (elem) (string-downcase (mpkg/obj:obj-name elem))))
	           variables))
          (format stream ")~%@end(section)~%"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun section-methods (package-name
                        &key
                          (stream    t)
                          (external  t)
                          (internal  nil)
                          (inherited nil)
                          (sort nil)
                          (min-doc-length 80)
                        &aux (package (find-package package-name)))
  "@b(Описание:) функция @b(section-methods) выводит в поток @b(stream)
секцию с документацией в формате codex, содержащую методы из пакета @b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (section-methods :mnas-package/example :internal t)
 @end(code)
"
  (declare ((or package string symbol) package-name))
  (with-package package
    (with-downcase
      (let ((methods (mpkg/pkg:package-methods package :external external :internal internal :inherited inherited)))
        (when (some
               #'(lambda (method)
                   (when (< min-doc-length (length (documentation method t))) t))
               methods)
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
          (format stream ")~%@end(section)~%"))))))

(defun section-setf-methods (package-name
                             &key
                               (stream    t)
                               (external  t)
                               (internal  nil)
                               (inherited nil)
                               (sort nil)
                               (min-doc-length 80)
                             &aux (package (find-package package-name)))
  "@b(Описание:) функция @b(section-setf-methods) выводит в поток @b(stream)
секцию с документацией в формате codex, содержащую setf-методы из пакета @b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (section-setf-methods :mnas-package/example :internal t)
 @end(code)
"
  (declare ((or package string symbol) package-name))
  (with-package package
    (with-downcase
      (let ((methods (mpkg/pkg:package-setf-methods package :external external :internal internal :inherited inherited)))
        (when (some
               #'(lambda (method)
                   (when (< min-doc-length (length (documentation method t))) t))
               methods)
          (format stream "@begin(section)~% @title(Setf-методы)~% @cl:with-package[name=~S]("
                  (mpkg/obj:obj-name package))
          (map nil
	       #'(lambda (el)
                   (insert-codex-doc el :stream stream :min-doc-length min-doc-length))
	       (if sort
	           (sort methods #'string<
                         :key #'(lambda (elem)
                                  (string-downcase (mpkg/obj:obj-name elem))))
	           methods))
          (format stream ")~%@end(section)~%"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun section-package (package-name
                        &key (stream t)
                          (external t) (internal nil) (inherited nil)
                          (sort nil) (min-doc-length 80)
                        &aux (package (find-package package-name)))
  "(section-package :mnas-package)"
  (format stream "@begin(section) @title(~A)~2%"
          (mnas-package/obj:obj-name package))
  (insert-codex-doc package :stream stream :min-doc-length min-doc-length)
  (map nil
       #'(lambda (func)
           (funcall func package :stream stream
                                 ;; :sort sort
                                 :min-doc-length min-doc-length
                                 :external external :internal internal :inherited inherited))
       (list #'section-variables
             #'section-macroses  
             #'section-functions 
             #'section-generics
             #'section-setf-functions 
             #'section-setf-generics  
             #'section-methods
             #'section-setf-methods                
             #'section-classes))
  (format stream "@end(section)~%"))

(defun section-system (system-name
                       &key (stream t)
                       &aux (system (asdf:find-system system-name)))
  (format stream "@begin(section) @title(Обзор)~2%")
  (format stream (asdf:system-description system))
  (format stream "@end(section)~%"))
  

(defun section-functions (package-name
                          &key
                            (stream t)
                            (external t)
                            (internal nil)
                            (inherited nil)
                            (sort nil)
                            (min-doc-length 80)
                          &aux (package (find-package package-name)))
  "@b(Описание:) функция section-functions выводит в поток stream
секцию с документацией в формате codex, содержащую функции из пакета package-name.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (section-functions :math/stat :external t :internal t :sort t) 
@end(code)
"  
  (declare ((or package string symbol) package-name))
  (with-package package
    (with-downcase
      (let ((funcs (mpkg/pkg:package-functions package :external external :internal internal :inherited inherited)))
        (when (some
               #'(lambda (func)
                   (when (< min-doc-length (length (documentation func t))) t))
               funcs)
          (format stream "@begin(section)~% @title(Функции)~% @cl:with-package[name=~S]("
	          (mpkg/obj:obj-name package))
          (map nil #'(lambda (el) (insert-codex-doc el :stream stream :min-doc-length min-doc-length))
	       (if sort
	           (sort funcs #'string< :key #'(lambda (elem) (string-downcase (slynk-backend:function-name elem))))
	           funcs))
          (format stream ")~%@end(section)~%"))))))

(defun section-macroses (package-name
                         &key
                           (stream t)
                           (external t)
                           (internal nil)
                           (inherited nil)
                           (sort nil)
                           (min-doc-length 80)
                         &aux (package (find-package package-name)))
  "@b(Описание:) функция section-functions выводит в поток stream
секцию с документацией в формате codex, содержащую функции из пакета package-name.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (section-macroses :mnas-package/example :external t :internal t :sort t)
 (section-macroses :mnas-package/example :external t :internal t :sort t :min-doc-length 10) 
@end(code)
"  
  (declare ((or package string symbol) package-name))
  (with-package package
    (with-downcase
      (let ((macroses (mpkg/pkg:package-macroses package :external external :internal internal :inherited inherited)))
        (when (some
               #'(lambda (macro)
                   (when (< min-doc-length (length (documentation macro t))) t))
               macroses)
          (format stream "@begin(section)~% @title(Макросы)~% @cl:with-package[name=~S]("
	          (mpkg/obj:obj-name package))
          (map nil #'(lambda (el) (insert-codex-doc el :stream stream :min-doc-length min-doc-length))
	       (if sort
	           (sort macroses #'string< :key #'(lambda (elem) (string-downcase (mpkg/obj:obj-name elem))))
	           macroses))
          (format stream ")~%@end(section)~%"))))))

(defun section-setf-functions (package-name
                               &key
                                 (stream t)
                                 (external t)
                                 (internal nil)
                                 (inherited nil)
                                 (sort nil)
                                 (min-doc-length 80)
                               &aux (package (find-package package-name)))
  "@b(Описание:) функция @b(section-setf-functions) выводит в поток stream
секцию с документацией в формате codex, содержащую setf-функции из пакета package-name.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (section-setf-functions :mnas-package/example :external t :internal t :sort t) 
@end(code)
"  
  (declare ((or package string symbol) package-name))
  (with-package package
    (with-downcase
      (let ((setf-funcs (mpkg/pkg:package-setf-functions package :external external :internal internal :inherited inherited)))
        (when (some
               #'(lambda (setf-func)
                   (when (< min-doc-length (length (documentation setf-func t))) t))
               setf-funcs)
          (format stream "@begin(section)~% @title(Setf-функции)~% @cl:with-package[name=~S]("
	          (mpkg/obj:obj-name package))
          (map nil #'(lambda (el) (insert-codex-doc el :stream stream :min-doc-length min-doc-length))
               (if sort
                   (sort setf-funcs #'string< :key #'(lambda (elem) (mpkg/obj:obj-name elem)))
                   setf-funcs))
          (format stream ")~%@end(section)~%"))))))

(defun section-generics (package-name
                         &key
                           (stream t)
                           (external t)
                           (internal nil)
                           (inherited nil)
                           (sort nil)
                           (min-doc-length 80)
                         &aux (package (find-package package-name)))
  "@b(Описание:) функция @b(section-generics) выводит в поток stream
секцию с документацией в формате codex, содержащую обобщенные функции из пакета @b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (section-generics :math/obj :sort t) 
@end(code)
"  
  (declare ((or package string symbol) package-name))
  (with-package package
    (with-downcase
      (let ((g-funcs (mpkg/pkg:package-generics package :external external :internal internal :inherited inherited)))
        (when (some
               #'(lambda (g-func)
                   (when (< min-doc-length (length (documentation g-func t))) t))
               g-funcs)
          (format stream "@begin(section)~% @title(Обобщенные функции)~% @cl:with-package[name=~S]("
                  (mpkg/obj:obj-name package))
          (map nil #'(lambda (el) (insert-codex-doc el :stream stream :min-doc-length min-doc-length))
	       (if sort
                   (sort g-funcs #'string< :key #'(lambda (elem) (string-downcase (mpkg/obj:obj-name elem))))
	           g-funcs))
          (format stream ")~%@end(section)~%"))))))

(defun section-setf-generics (package-name
                              &key
                                (stream t)
                                (external t)
                                (internal nil)
                                (inherited nil)
                                (sort nil)
                                (min-doc-length 80)
                              &aux (package (find-package package-name)))
  "@b(Описание:) функция @b(section-setf-generics) выводит в поток stream
секцию с документацией в формате codex, содержащую обощенные setf-функции из пакета package-name.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (section-setf-generics :mnas-package/example :external t :internal t) 
@end(code)
"  
  (declare ((or package string symbol) package-name))
  (with-package package
    (with-downcase
      (let ((setf-funcs (mpkg/pkg:package-setf-generics package :external external :internal internal :inherited inherited)))
        (when (some
               #'(lambda (setf-func)
                   (when (< min-doc-length (length (documentation setf-func t))) t))
               setf-funcs)
          (format stream "@begin(section)~% @title(Обобщенные setf-функции)~% @cl:with-package[name=~S]("
	          (mpkg/obj:obj-name package))
          (map nil #'(lambda (el) (insert-codex-doc el :stream stream :min-doc-length min-doc-length))
               (if sort
                   (sort setf-funcs #'string< :key #'(lambda (elem) (mpkg/obj:obj-name elem)))
                   setf-funcs))
          (format stream ")~%@end(section)~%"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-doc-generics (package class prefix &key (stream t) (min-doc-length 80))
  "@b(Описание:) функция @b(make-doc-methods) выводит в поток
@b(stream) раздел документации, подготовленной для вставки в 
scr-файл системы документирования codex. Этот раздел содержит 
методы класса @b(class), имена которых начинаются 
с префикса @b(prefix).
"
  (with-package package
    (with-downcase
      (format stream " @cl:with-package[name=~s](~%" (mpkg/obj:obj-name package))
      (block make-doc-for-generics
        (map 'nil #'(lambda (el) (insert-codex-doc el :stream stream :min-doc-length min-doc-length :pkg package))
             (find-all-generics class prefix)))
      (format stream ")~%"))))

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
  (with-package package
    (with-downcase
      (format stream " @cl:with-package[name=~S](~%" (mpkg/obj:obj-name package))
      (block make-doc-for-methods
        (map 'nil
             #'(lambda (el)
                 (insert-codex-doc el :stream stream :min-doc-length min-doc-length))
             (find-all-methods class prefix)))
      (format stream ")~%"))))

(defun make-codex-documentation (package-name
                                 &key (stream t)
                                   (system-name nil)
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
  (when system-name (section-system system-name :stream stream))
  (section-package package :stream stream
                           :external external :internal internal :inherited inherited
                           :sort sort
                           :min-doc-length min-doc-length))

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
      (when (< 0 (+ (mnas-graph:<graph>-nodes-count system-graph)
                    (mnas-graph:<graph>-nodes-count call-graph  )
                    (mnas-graph:<graph>-nodes-count symbol-graph)
                    (mnas-graph:<graph>-nodes-count class-graph )))
        (format os " @begin(section) @title(Графы ~A)~%" pkg-name)
        (format os "  @begin(list)~%")
        (when (< 0 (mnas-graph:<graph>-nodes-count system-graph))
          (format os "   @item(system-graph @image[src=./system-graph-~A.gv.png]())~%" pkg-name))
        (when (< 0 (mnas-graph:<graph>-nodes-count call-graph))
          (format os "   @item(call-graph   @image[src=./call-graph-~A.gv.png]())~%" pkg-name))
        (when (< 0 (mnas-graph:<graph>-nodes-count symbol-graph))
          (format os "   @item(symbol-graph @image[src=./symbol-graph-~A.gv.png]())~%" pkg-name))
        (when (< 0 (mnas-graph:<graph>-nodes-count class-graph))
          (format os "   @item(class-graph  @image[src=./class-graph-~A.gv.png]())~%" pkg-name))
        (format os "  @end(list)~% @end(section)")))))

#|
(make-codex-graphs :mnas-package :mnas-package) ;
(< 0 (mnas-graph:<graph>-nodes-count (mpkg/view:symbol-graph (find-package :mnas-package))))
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
           (let ((from-node (mnas-graph:find-node graph (string (class-name class)))))
             (when from-node
	       (mapc
	        #'(lambda (el)
		    (mnas-graph:insert-to
		     (make-instance
		      'mnas-graph:<edge>
		      :from from-node
		      :to   (make-instance 'mnas-graph:<node> :owner graph :name (string (class-name el))))
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
@end(code)
"
  (flet ((find-super-classes (class)
           (let ((to-node (mnas-graph:find-node graph (string (class-name class)))))
             (when to-node
	       (mapc
	        #'(lambda (el)
		    (mnas-graph:insert-to
		     (make-instance
		      'mnas-graph:<edge>
		      :from (make-instance 'mnas-graph:<node> :owner graph :name (string (class-name el)))
		      :to   to-node)
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
                   (sort nil)
                   (min-doc-length 80)
                   )
  " @b(Описание:) функция @b(document) формирует scr-файл (сценарий
  системы codex), содержащий документацию о пакете @b(package-name) и
  системы системы @b(system-name). Если имя системы равно @b(nil),
  извлечение связанной с ней документации не выполняется.

  @b(Пример использования:)
@begin[lang=lisp](code)
 (mnas-package:document :mnas-package :mnas-package) => path_to_mnas-package_system/docs/mnas-package.scr
 (mnas-package:document :mnas-package/view nil) => path_to_mnas-package_system/docs/mnas-package-view.scr
@end(code)
"  
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
