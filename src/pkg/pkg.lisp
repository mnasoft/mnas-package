;;;; ./src/pkg/pkg.lisp

(defpackage #:mnas-package/pkg
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod ->key ((thing cons))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric dependencies-of (system))

(defmethod dependencies-of ((system symbol))
  (remove-if #'null
             (mapcar #'->key
                     (slot-value
                      (asdf/system:find-system system)
                      'asdf/component:sideway-dependencies))))
  
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
  (cond
    ((listp (first func))
     (second (first func)))
    ((null (listp (first func)))
     (first func))))

(defun who-references (var)
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
  (apply #'append
   (mapcar #'who-references
	   var-lst)))

(defun who-calls (func)
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
  (apply #'append
         (mapcar #'who-calls
	         func-lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun filter-variables (symbols)
  (let ((rez nil))
    (mapc
     #'(lambda (el)
	 (when (boundp el)
	   (push el rez)))
     symbols)
    rez))

(defun filter-functions (symbols)
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
  (let ((rez nil))
    (map nil
     #'(lambda (el) (when (macro-function el) (push el rez)))
     symbols)
    rez))

(defun filter-setf-functions (symbols)
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
  (let ((rez nil))
    (mapc
     #'(lambda (method)
         (when (eq 'standard-method (type-of method))
           (push method rez)))
     methods)
    rez))

(defun filter-setf-methods (methods)
  (let ((rez nil))
    (mapc
     #'(lambda (method)
         (when (eq 'standard-method (type-of method))
           (push method rez)))
     methods)
    rez))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun package-variables (package-name &key (external t) (internal nil) (inherited nil))
  (filter-variables
   (package-symbols-by-category
    package-name
    :external external
    :internal internal
    :inherited inherited)))

(defun package-functions (package-name &key (external t) (internal nil) (inherited nil) )
  (mapcar #'symbol-function (filter-functions
	                     (package-symbols-by-category
	                      package-name
	                      :external external
	                      :internal internal
	                      :inherited inherited))))

(defun package-macroses (package-name &key (external t) (internal nil) (inherited nil) )
  (mapcar #'macro-function
	  (filter-macroses
	   (package-symbols-by-category
	    package-name
	    :external  external
	    :internal  internal
	    :inherited inherited))))

(defun package-setf-functions (package-name &key (external t) (internal nil) (inherited nil) )
  (mapcar #'(lambda (el) (alexandria:ensure-function `(setf ,el)))
          (filter-setf-functions
           (package-symbols-by-category
            package-name
            :external  external
            :internal  internal
            :inherited inherited))))

(defun package-classes (package-name &key (external t) (internal nil) (inherited nil)
			&aux
			  (rez nil)
			  (class nil)
			  (package (find-package package-name)))
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

(defun package-generics (package-name &key (external t) (internal nil) (inherited nil))
  (mapcar #'symbol-function
          (filter-generics
	   (package-symbols-by-category
	    package-name
	    :external external
	    :internal internal
	    :inherited inherited))))

(defun package-setf-generics (package-name &key (external t) (internal nil) (inherited nil) )
  (mapcar #'(lambda (el) (alexandria:ensure-function `(setf ,el)))
          (filter-setf-generics
           (package-symbols-by-category
            package-name
            :external  external
            :internal  internal
            :inherited inherited))))

(defun package-methods (package-name &key (external t) (internal nil) (inherited nil))
  (filter-methods
   (apply #'append 
          (mapcar #'closer-mop:generic-function-methods
                  (package-generics package-name
                                    :external  external
                                    :internal  internal
                                    :inherited inherited)))))

(defun package-setf-methods (package-name &key (external t) (internal nil) (inherited nil))
  (filter-setf-methods 
   (apply #'append 
          (mapcar #'closer-mop:generic-function-methods
                  (package-setf-generics package-name
                                         :external  external
                                         :internal  internal
                                         :inherited inherited)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
