;;;; ./src/pkg/pkg.lisp

(defpackage :mnas-package/pkg
  (:use #:cl )
  (:nicknames "MPKG/PKG")
  (:export package-variables
           package-methods
           package-generics
           package-functions
           package-macroses
           package-classes
           package-setf-functions 
           package-setf-generics  
           package-setf-methods ;;;;
           )
  (:export filter-variables
           filter-functions
           filter-macroses
           filter-setf-functions ;;;;
           filter-generics
           filter-setf-generics ;;;;
           filter-methods
           filter-setf-methods  ;;;;
           )
  (:export package-symbols-by-category
           who-calls-lst
           func-to-string
           who-references-lst
           dependencies-of
           dependency-tree
           )
  (:intern ->key
           ordered-dep-tree
           defu-defm-name 
           who-references
           who-calls
           package-symbols-all
           )
  (:documentation
   " Пакет @b(mnas-package/pkg) определяет операции извлечения символов по типу
их (видимости и наследования) и группировки символов по категориям, к которым
прнадлежат связанные с ними сущности.

 Основными функциями пакета @b(mnas-package/pkg) являются следующие функции:
@begin(list)
 @item(package-variables;)
 @item(package-methods;)
 @item(package-generics;)
 @item(package-functions;)
 @item(package-macroses;)
 @item(package-setf-functions;)
 @item(package-classes;)
 @item(package-symbols-by-category.)
@end(list)

 Все они имеют схожий набор параметров: 
@begin(list)

 @item(package-name - имя пакета символы которого извлекаются. Его
       можно указывать в виде нескольких вариантов: 'mnas-package;
       :mnas-package; \"MNAS-PACKAGE\".  В случае указания имени
       пакета как строки символы должны быть в верхнем регистре;)
 @item(external - отбирать (t) или не отбирать (nil) внешиние
       символы;)
 @item(internal - отбирать (t) или не отбирать (nil) внутренние
       символы;) @item(inherited - отбирать (t) или не отбирать (nil)
       заимствованные символы.)
@end(list)"
   ))

(in-package :mnas-package/pkg)

(defgeneric ->key (thing)
  (:documentation "Not yet documented"))

(defgeneric dependency-tree (system)
  (:documentation "Not yet documented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

 @b(Пример использования:)
@begin[lang=lisp](code)
 (package-symbols-by-category 'mnas-package :internal nil)                 ;; отбор только внешних символов;
 (package-symbols-by-category :mnas-package)                               ;; отбор внешних и внутренних символов;
 (package-symbols-by-category \"MNAS-PACKAGE\" :internal nil :inherited t) ;; отбор только внешних и заимствованных символов;
@end(code)"
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

(defun package-symbols-all (package-name
                            &aux
                              (lst nil)
                              (package
                               (find-package package-name)))
  "@b(Описание:) package-symbols-all Выполнят поиск всех символов, 
определенных пакетом @b(package-name).

 @b(Пример использования:)
 @begin[lang=lisp](code)
 (package-symbols-all 'mnas-package)
 (package-symbols-all :mnas-package)
 (package-symbols-all (find-package :mnas-package))
 (package-symbols-all \"MNAS-PACKAGE\")
@end(code)"
  (declare ((or package string symbol) package-name))
  (cond
    (package (do-symbols (s package ) (push s lst)) lst)
    (t (error "~S does not designate a package" package-name))))
  
(defmethod ->key ((thing string))
  "Not yet documented"
  (intern (string-upcase thing) :keyword))

(defmethod ->key ((thing symbol))
  "Not yet documented"
  (if (keywordp thing)
      thing
      (intern (symbol-name thing) :keyword)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod ->key ((thing cons))
  "Not yet documented"
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric dependencies-of (system)
  (:documentation "Not yet documented"))

(defmethod dependencies-of ((system symbol))
  "Not yet documented"
  (remove-if #'null
             (mapcar #'->key
                     (slot-value
                      (asdf/system:find-system system)
                      'asdf/component:sideway-dependencies))))
  
(defun ordered-dep-tree (dep-tree)
  "Not yet documented"
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

(defmethod dependency-tree ((system symbol))
  "Not yet documented"
  (let ((res (make-hash-table)))
    (labels ((rec (sys) 
               (loop with deps = (dependencies-of sys)
                  for dep in deps for dep-k = (->key dep)
                  unless (gethash dep-k res) do (rec dep)
                  do (pushnew (->key sys) (gethash dep-k res)))))
      (rec system))
    (ordered-dep-tree (alexandria:hash-table-alist res))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun func-to-string (func)
  "Not yet documented"
  (cond
    ((symbolp func)
     (string-downcase (string func)))
    (t
     (string-downcase (format nil "~S" func)))))

(defun defu-defm-name (func)
  "Not yet documented"
  (cond
    ((listp (first func))
     (second (first func)))
    ((null (listp (first func)))
     (first func))))

(defun who-references (var)
  "Выполняет поиск функций, в которых есть ссылка на внешнюю переменную
var. Возвращает список, каждым элементом которого является список
следующего формата: (функция переменная).
 
 @b(Пример использования:)
@begin[lang=lisp](code)
 (who-references '*sample-var*) 
 => ((\"who-references\" \"*sample-var*\"))
@end(code)"
  (let ((rez (slynk-backend:who-references var))
        ;; (rez (swank/backend:who-references var))
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
  "Not yet documented"
  (apply #'append
   (mapcar #'who-references
	   var-lst)))

(defun who-calls (func)
  "Not yet documented"
  (let ((rez (slynk-backend:who-calls func))
        ;;(rez (swank/backend:who-calls func))
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
  "Not yet documented"
  (apply #'append
         (mapcar #'who-calls
	         func-lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun filter-variables (symbols)
  "@b(Описание:) функция filter-variables возвращает список символов,
являющихся сопряженными со значениями.

 @b(Переменые:)
@begin(list) 
@item(symbols - список символов пакета.)
@end(list)"
  (let ((rez nil))
    (mapc
     #'(lambda (el)
	 (when (boundp el)
	   (push el rez)))
     symbols)
    rez))

(defun filter-functions (symbols)
    "@b(Описание:) функция filter-functions возвращает список символов, являющихся
сопряженными с функциями.

 @b(Переменые:)
@begin(list)
@item(symbols - список символов пакета.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (filter-functions
   (package-symbols-by-category :mnas-package/example))
 => (MNAS-PACKAGE/EXAMPLE:F-A-EXP
     MNAS-PACKAGE/EXAMPLE:F-B-EXP
     MNAS-PACKAGE/EXAMPLE:F-C-EXP
     MNAS-PACKAGE/EXAMPLE::F-B-INT
     MNAS-PACKAGE/EXAMPLE::F-C-INT
     MNAS-PACKAGE/EXAMPLE::F-A-INT)
@end(code)"
  (let ((rez nil))
    (mapc
     #'(lambda (el)
         (when (and (fboundp el)
                    (eq :function
                        (nth-value
                         1
                         (mpkg/obj:obj-name
                          (symbol-function el)))))
           (push el rez)))
     symbols)
    rez))

(defun filter-macroses (symbols)
    "@b(Описание:) функция filter-functions возвращает список символов
сопряженных с макросами.

 @b(Переменые:)
@begin(list)
@item(symbols - список символов пакета.)
@end(list)
 @b(Пример использования:)
@begin[lang=lisp](code)
 (filter-macroses
   (package-symbols-by-category :mnas-package/example))
 => (MNAS-PACKAGE/EXAMPLE:K-B-EXP
     MNAS-PACKAGE/EXAMPLE:K-C-EXP
     MNAS-PACKAGE/EXAMPLE:K-A-EXP
     MNAS-PACKAGE/EXAMPLE::K-B-INT
     MNAS-PACKAGE/EXAMPLE::K-C-INT
     MNAS-PACKAGE/EXAMPLE::K-A-INT)
@end(code)"
  (let ((rez nil))
    (map nil
     #'(lambda (el) (when (macro-function el) (push el rez)))
     symbols)
    rez))

(defun filter-setf-functions (symbols)
  "@b(Описание:) функция filter-functions возвращает список символов,
являющихся сопряженными с setf-функциями.

 @b(Переменые:)
@begin(list) 
@item(symbols - список символов пакета.)
@end(list)
 @b(Пример использования:)
@begin[lang=lisp](code)
 (filter-setf-functions
   (package-symbols-by-category :mnas-package/example))
 => (MNAS-PACKAGE/EXAMPLE:F-A-EXP
     MNAS-PACKAGE/EXAMPLE:F-B-EXP
     MNAS-PACKAGE/EXAMPLE:F-C-EXP)
@end(code)"  
  (let ((rez nil))
    (mapc
     #'(lambda (symbol)
         (let ((setf-name `(setf ,symbol)))
           (when (and (not (null (ignore-errors (fdefinition setf-name))))
                      (or (eq (type-of (alexandria:ensure-function setf-name)) 'function)
                          (eq (type-of (alexandria:ensure-function setf-name)) 'compiled-function)))
             (push symbol rez))))
     symbols)
    rez))

(defun filter-generics (symbols)
  "@b(Описание:) функция @b(filter-generics) возвращает список
символов сопряженных с обобщеными функциями.

 @b(Переменые:)
@begin(list)
@item(symbols - список символов пакета.)
@end(list)
 @b(Пример использования:)
@begin[lang=lisp](code)
 (filter-generics
   (package-symbols-by-category :mnas-package/example))
 => (MNAS-PACKAGE/EXAMPLE:M-A-EXP
     MNAS-PACKAGE/EXAMPLE:M-C-EXP
     MNAS-PACKAGE/EXAMPLE:M-B-EXP
     MNAS-PACKAGE/EXAMPLE::<C-C-INT>-C
     MNAS-PACKAGE/EXAMPLE::<C-B-INT>-B
     MNAS-PACKAGE/EXAMPLE::<C-A-INT>-A
     MNAS-PACKAGE/EXAMPLE::M-C-INT
     MNAS-PACKAGE/EXAMPLE::M-A-INT
     MNAS-PACKAGE/EXAMPLE::M-B-INT)
@end(code)"  
  (let ((rez nil))
    (mapc
     #'(lambda (el)
         (when (and (fboundp el)
                    (eq :generic
                        (nth-value
                         1
                         (mpkg/obj:obj-name
                          (symbol-function el)))))
           (push el rez)))
     symbols)
    rez))

(defun filter-setf-generics (symbols)
  "@b(Описание:) функция @b(filter-setf-generics) возвращает список символов,
являющихся сопряженными с обобщенными setf-функциями.

 @b(Переменые:)
@begin(list) 
@item(symbols - список символов пакета.)
@end(list)"
  (let ((rez nil))
    (mapc
     #'(lambda (symbol)
         (let ((setf-name `(setf ,symbol)))
           (when (and (not (null (ignore-errors (fdefinition setf-name))))
                      (or (eq (type-of (alexandria:ensure-function setf-name)) 'standard-generic-function)))
             (push symbol rez))))
     symbols)
    rez))

(defun filter-methods (methods)
  "@b(Описание:) функция @b(filter-methods) возвращает список символов,
являющихся сопряженными с setf-методами.

 @b(Переменые:)
@begin(list) 
@item(symbols - список символов пакета.)
@end(list)
 @b(Пример использования:)
@begin[lang=lisp](code)
 (filter-methods
   (apply #'append 
          (mapcar #'closer-mop:generic-function-methods
                  (package-generics :mnas-package/example))))
 => (#<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE:M-B-EXP  (MNAS-PACKAGE/EXAMPLE::<C-A-INT> MNAS-PACKAGE/EXAMPLE::<C-B-INT> MNAS-PACKAGE/EXAMPLE:<C-C-EXP>) {100E4888F3}>
     #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE:M-C-EXP  (MNAS-PACKAGE/EXAMPLE::<C-A-INT> MNAS-PACKAGE/EXAMPLE::<C-B-INT> MNAS-PACKAGE/EXAMPLE::<C-C-INT>) {100E488903}>
     #<S TANDARD-METHOD MNAS-PACKAGE/EXAMPLE:M-C-EXP (MNAS-PACKAGE/EXAMPLE:<C-A-EXP> MNAS-PACKAGE/EXAMPLE:<C-B-EXP> MNAS-PACKAGE/EXAMPLE:<C-C-EXP>) {100E53CD03}>
     #<S TANDARD-METHOD MNAS-PACKAGE/EXAMPLE:M-A-EXP (MNAS-PACKAGE/EXAMPLE::<C-A-INT> MNAS-PACKAGE/EXAMPLE::<C-B-INT> MNAS-PACKAGE/EXAMPLE:<C-C-EXP>) {100E4888E3}>)
@end(code)"  
  (let ((rez nil))
    (mapc
     #'(lambda (method)
         (when (eq 'standard-method (type-of method))
           (push method rez)))
     methods)
    rez))

(defun filter-setf-methods (methods)
  "@b(Описание:) функция @b(filter-setf-methods) возвращает список
символов, являющихся сопряженными с setf-методами.

 @b(Переменые:)
@begin(list) 
@item(symbols - список символов пакета.)
@end(list)
 @b(Пример использования:)
@begin[lang=lisp](code)
 (filter-setf-methods
   (apply #'append 
          (mapcar #'closer-mop:generic-function-methods
                  (package-generics :mnas-package/example))))
 => (#<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE:M-B-EXP (MNAS-PACKAGE/EXAMPLE::<C-A-INT> MNAS-PACKAGE/EXAMPLE::<C-B-INT> MNAS-PACKAGE/EXAMPLE:<C-C-EXP>) {100E4888F3}>
     #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE:M-C-EXP (MNAS-PACKAGE/EXAMPLE::<C-A-INT> MNAS-PACKAGE/EXAMPLE::<C-B-INT> MNAS-PACKAGE/EXAMPLE::<C-C-INT>) {100E488903}>
     #<S TANDARD-METHOD MNAS-PACKAGE/EXAMPLE:M-C-EXP (MNAS-PACKAGE/EXAMPLE:<C-A-EXP> MNAS-PACKAGE/EXAMPLE:<C-B-EXP> MNAS-PACKAGE/EXAMPLE:<C-C-EXP>) {100E53CD03}>
     #<S TANDARD-METHOD MNAS-PACKAGE/EXAMPLE:M-A-EXP (MNAS-PACKAGE/EXAMPLE::<C-A-INT> MNAS-PACKAGE/EXAMPLE::<C-B-INT> MNAS-PACKAGE/EXAMPLE:<C-C-EXP>) {100E4888E3}>)
@end(code)"  
  (let ((rez nil))
    (mapc
     #'(lambda (method)
         (when (eq 'standard-method (type-of method))
           (push method rez)))
     methods)
    rez))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun package-variables (package-name
                          &key
                            (external t)
                            (internal nil)
                            (inherited nil))
  "@b(Описание:) функция @b(package-variables) возвращает список символов
пакета @b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (package-variables :mnas-package :inherited t)
@end(code)"
  (filter-variables
   (package-symbols-by-category
    package-name
    :external external
    :internal internal
    :inherited inherited)))

(defun package-functions (package-name
                          &key
                            (external t)
                            (internal nil)
                            (inherited nil) )
  "@b(Описание:) функция @b(package-functions) возвращает список
функций пакета @b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (package-functions :mnas-package/example :internal t)
 => (#<FUNCTION MNAS-PACKAGE/EXAMPLE:BAZ-SHORT> #<FUNCTION MNAS-PACKAGE/EXAMPLE:BAZ>)
@end(code)"
  (mapcar #'symbol-function (filter-functions
	                     (package-symbols-by-category
	                      package-name
	                      :external external
	                      :internal internal
	                      :inherited inherited))))

(defun package-macroses (package-name
                         &key
                           (external t)
                           (internal nil)
                           (inherited nil))
  "@b(Описание:) функция @b(package-macroses) возвращает список
 макросов пакета @b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (package-macroses :mnas-package/example :internal t)
 => (#<FUNCTION (MACRO-FUNCTION MNAS-PACKAGE/EXAMPLE::MAK-A-SHORT) {52D45ECB}>
     #<FUNCTION (MACRO-FUNCTION MNAS-PACKAGE/EXAMPLE::MAK-A) {52D454BB}>)
@end(code)"
  (mapcar #'macro-function
	  (filter-macroses
	   (package-symbols-by-category
	    package-name
	    :external  external
	    :internal  internal
	    :inherited inherited))))

(defun package-setf-functions (package-name
                               &key
                                 (external t)
                                 (internal nil)
                                 (inherited nil))
  "@b(Описание:) функция @b(package-setf-functions) возвращает список
setf-функций пакета @b(package-name).

 @b(Пример использования:) @begin[lang=lisp](code)
 (package-setf-functions :mnas-package/example :internal t)
 => (#<FUNCTION (SETF MNAS-PACKAGE/EXAMPLE::FOO)>)
@end(code)"  
  (mapcar #'(lambda (el) (alexandria:ensure-function `(setf ,el)))
          (filter-setf-functions
           (package-symbols-by-category
            package-name
            :external  external
            :internal  internal
            :inherited inherited))))

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
 (package-classes :mnas-package/example) => (#<STANDARD-CLASS MNAS-PACKAGE/EXAMPLE:<C>>)
 (package-classes :mnas-package/example :external nil :internal t)
  => (#<STANDARD-CLASS MNAS-PACKAGE/EXAMPLE::<A>>
      #<STANDARD-CLASS MNAS-PACKAGE/EXAMPLE::<B>>
      #<STANDARD-CLASS MNAS-PACKAGE/EXAMPLE::<B-SHORT>>
      #<STANDARD-CLASS MNAS-PACKAGE/EXAMPLE::<C-SHORT>>
      #<STANDARD-CLASS MNAS-PACKAGE/EXAMPLE::<A-SHORT>>)
@end(code)"  
  (declare ((or package string symbol) package-name))
  (map nil 
       #'(lambda (el)
           (setf class (find-class el nil))
           (when class (push class rez)))
       (package-symbols-by-category
        package
        :external  external 
        :internal  internal
        :inherited inherited))
  rez)

(defun package-generics (package-name
                         &key
                           (external t)
                           (internal nil)
                           (inherited nil))
  "@b(Описание:) функция @b(package-generics) возвращает список
обобщенных функций пакета @b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
  (package-generics :mnas-package/example :internal t) 
 => (#<STANDARD-GENERIC-FUNCTION MNAS-PACKAGE/EXAMPLE:M-C-EXP (1)>
     #<STANDARD-GENERIC-FUNCTION MNAS-PACKAGE/EXAMPLE:M-B-EXP (1)>
     #<STANDARD-GENERIC-FUNCTION MNAS-PACKAGE/EXAMPLE:M-A-EXP (1)>
     #<STANDARD-GENERIC-FUNCTION MNAS-PACKAGE/EXAMPLE::<C-C-INT>-C (1)>
     #<STANDARD-GENERIC-FUNCTION MNAS-PACKAGE/EXAMPLE::<C-B-INT>-B (1)>
     #<STANDARD-GENERIC-FUNCTION MNAS-PACKAGE/EXAMPLE::<C-A-INT>-A (1)>)
@end(code)"  
  (mapcar #'symbol-function
          (filter-generics
	   (package-symbols-by-category
	    package-name
	    :external external
	    :internal internal
	    :inherited inherited))))

(defun package-setf-generics (package-name
                              &key
                                (external t)
                                (internal nil)
                                (inherited nil))
  "@b(Описание:) функция @b(package-setf-generics) возвращает список
функций пакета @b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
  (package-setf-functions :mnas-package/example :internal t)
 => (#<FUNCTION (SETF MNAS-PACKAGE/EXAMPLE::FOO)>)
@end(code)"  
  (mapcar #'(lambda (el) (alexandria:ensure-function `(setf ,el)))
          (filter-setf-generics
           (package-symbols-by-category
            package-name
            :external  external
            :internal  internal
            :inherited inherited))))

(defun package-methods (package-name
                        &key
                          (external t)
                          (internal nil)
                          (inherited nil))
  "@b(Описание:) функция @b(package-methods) возвращает список методов
пакета @b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
  (package-methods :mnas-package/example :internal t)
  (#<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE:M-A-EXP (MNAS-PACKAGE/EXAMPLE::<C-A-INT> MNAS-PACKAGE/EXAMPLE::<C-B-INT> MNAS-PACKAGE/EXAMPLE:<C-C-EXP>) {1004634963}>
   #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE:M-B-EXP (MNAS-PACKAGE/EXAMPLE::<C-A-INT> MNAS-PACKAGE/EXAMPLE::<C-B-INT> MNAS-PACKAGE/EXAMPLE:<C-C-EXP>) {1004634983}>
   #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE:M-C-EXP (MNAS-PACKAGE/EXAMPLE::<C-A-INT> MNAS-PACKAGE/EXAMPLE::<C-B-INT> MNAS-PACKAGE/EXAMPLE::<C-C-INT>) {10046349A3}>
   #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE:M-C-EXP (MNAS-PACKAGE/EXAMPLE:<C-A-EXP> MNAS-PACKAGE/EXAMPLE:<C-B-EXP> MNAS-PACKAGE/EXAMPLE:<C-C-EXP>) {10049D5BD3}>)
@end(code)"  
  (filter-methods
   (apply #'append 
          (mapcar #'closer-mop:generic-function-methods
                  (package-generics package-name
                                    :external  external
                                    :internal  internal
                                    :inherited inherited)))))

(defun package-setf-methods (package-name
                             &key
                               (external t)
                               (internal nil)
                               (inherited nil))
  "@b(Описание:) функция @b(package-setf-methods) возвращает список
setf-методов пакета @b(package-name).

 @b(Пример использования:)
@begin[lang=lisp](code)
  (package-setf-methods :mnas-package/example :internal t)
    (#<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE::M-FOO :AROUND (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> T) {10019B69A3}>
     ...
     #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE::M-FOO-SHORT (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> MNAS-PACKAGE/EXAMPLE:<C>) {1001C6CAC3}>)
@end(code)"  
  (filter-setf-methods 
   (apply #'append 
          (mapcar #'closer-mop:generic-function-methods
                  (package-setf-generics package-name
                                         :external  external
                                         :internal  internal
                                         :inherited inherited)))))
