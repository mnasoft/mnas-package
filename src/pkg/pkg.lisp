;;;; ./src/pkg/pkg.lisp

(defpackage #:mnas-package/pkg
  (:use #:cl )
  (:nicknames "MPKG/PKG")
  (:export package-methods
           package-generics
           package-functions
           package-setf-functions
           package-classes
           package-variables
           )
  (:export filter-variables
           filter-functions
           filter-setf-functions
           package-symbols-by-category
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

(in-package :mnas-package/pkg)

(defgeneric ->key (thing))

(defgeneric dependency-tree (system))

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
  
(defmethod ->key ((thing string))
  (intern (string-upcase thing) :keyword))

(defmethod ->key ((thing symbol))
  (if (keywordp thing)
      thing
      (intern (symbol-name thing) :keyword)))

(defgeneric dependencies-of (system))

(defmethod dependencies-of ((system symbol))
  (mapcar #'->key
          (slot-value
           (asdf/system:find-system system)
           'asdf/component:sideway-dependencies)))
  
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

(defmethod dependency-tree ((system symbol))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun filter-setf-functions (symbols)
  "@b(Описание:) функция filter-functions возвращает список символов,
являющихся сопряженными с setf-функциями.

 @b(Переменые:)
@begin(list) 
@item(symbols - список символов пакета.)
@end(list)
"
  (let ((rez nil))
    (mapc
     #'(lambda (symbol)
         (let ((setf-name `(setf ,symbol)))
           (when (not (null (ignore-errors (fdefinition setf-name))))
             (push symbol rez))))
     symbols)
    rez))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun package-setf-functions (package-name &key (external t) (internal nil) (inherited nil) )
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
    (map 'nil
         #'(lambda (el) (push (alexandria:ensure-function `(setf ,el)) rez))
         (filter-setf-functions
          (package-symbols-by-category
           package-name
           :external  external
           :internal  internal
           :inherited inherited)))
    rez))

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

(defun package-methods (package-name &key (external t) (internal nil) (inherited nil))
  "@b(Описание:) функция @b(package-methods) возвращает список методов
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
  (package-methods :mnas-package :internal t)
   => (#<STANDARD-METHOD MNAS-PACKAGE::DEPENDENCY-TREE (SYMBOL) {1007EE7CA3}>
       ...
       #<STANDARD-METHOD MNAS-PACKAGE:OBJ-PACKAGE-STRING (T) {1007EE79C3}>)
@end(code)
"
  (apply #'append
         (loop :for generic :in (package-generics package-name :external external :internal internal :inherited inherited)
        :collect (sb-mop:generic-function-methods generic))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



