;;;; ./src/sec/sec.lisp

(defpackage :mnas-package/sec
  (:use #:cl ) 
  (:nicknames "MPKG/SEC")
  (:export *min-doc-length*)
  (:export section-variables 
           section-functions
           section-macroses
           section-setf-functions
           section-generics
           section-setf-generics
           section-methods
           section-setf-methods
           section-classes
           section-package
           section-system)
  (:export insert-codex-doc)
  (:export with-downcase
           with-package
           )
  (:documentation "Пакет @b(mnas-package/docs) содержит функции
  генерирования секций документации."))

(in-package :mnas-package/sec)

(defparameter *min-doc-length* 80)

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


(defgeneric insert-codex-doc (obj &key stream min-doc-length))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod insert-codex-doc ((symbol symbol)
                             &key (stream t) (min-doc-length *min-doc-length*))
  (when (< min-doc-length (length (documentation symbol 'variable)))
    (format stream "~%  @cl:doc(variable ~s)" (mpkg/obj:obj-name symbol))
    t))

(defmethod insert-codex-doc ((function function)
                             &key (stream t) (min-doc-length *min-doc-length*))
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
                             &key (stream t) (min-doc-length *min-doc-length*))
  (when (< min-doc-length (length (documentation generic t)))
    (multiple-value-bind (name type) (mpkg/obj:obj-name generic)
      (ecase type
        (:generic
         (format stream "~&  @cl:doc(generic ~s)" name))
        (:setf-generic
         (format stream "~&  @cl:doc(setf-generic ~s)" name))))))

(defmethod insert-codex-doc ((class class) &key (stream t) (min-doc-length *min-doc-length*))
  (when (< min-doc-length (length (documentation class t)))
    (format stream "~&  @cl:doc(class ~s)" (mpkg/obj:obj-name class))
    t))

(defmethod insert-codex-doc ((method method) &key (stream t) (min-doc-length *min-doc-length*))
  (when (< min-doc-length (length (documentation method t)))
    (let ((mqs (closer-mop::method-qualifiers method))
          (mll (closer-mop:method-lambda-list method))
          (msp (closer-mop:method-specializers method)))
      (unless (and mqs (listp mqs) (= 1 (length mqs)))
        (multiple-value-bind (name type) (mpkg/obj:obj-name method)
          (ecase type
            (:setf-method
             (format stream "~&  @cl:doc(setf-method ~s" name))
            (:method
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
                             &key (stream t) (min-doc-length *min-doc-length*))
  (when (< min-doc-length (length (documentation package t)))
    (format stream "~a" (documentation package t))
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun section-variables (package-name
                          &key
                            (stream t)
                            (external t)
                            (internal nil)
                            (inherited nil)
                            (sort t)
                            (min-doc-length *min-doc-length*)
                          &aux (package (find-package package-name)))
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
                          (min-doc-length *min-doc-length*)
                        &aux (package (find-package package-name)))
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
                               (min-doc-length *min-doc-length*)
                             &aux (package (find-package package-name)))
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
                        &key
                          (stream t)
                          (external t) (internal nil) (inherited nil)
                          (sort t) (min-doc-length *min-doc-length*)
                        &aux (package (find-package package-name)))
  (format stream "@begin(section) @title(~A)~2%"
          (mnas-package/obj:obj-name package))
  (insert-codex-doc package :stream stream :min-doc-length min-doc-length)
  (map nil
       #'(lambda (func)
           (funcall func package :stream stream
                                 :sort sort
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
                            (sort t)
                            (min-doc-length *min-doc-length*)
                          &aux (package (find-package package-name)))
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
                           (sort t)
                           (min-doc-length *min-doc-length*)
                         &aux (package (find-package package-name)))
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
                                 (sort t)
                                 (min-doc-length *min-doc-length*)
                               &aux (package (find-package package-name)))
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
                           (sort t)
                           (min-doc-length *min-doc-length*)
                         &aux (package (find-package package-name)))
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
                                (sort t)
                                (min-doc-length *min-doc-length*)
                              &aux (package (find-package package-name)))
  (declare ((or package string symbol) package-name))
  (with-package package
    (with-downcase
      (let ((setf-generics (mpkg/pkg:package-setf-generics package :external external :internal internal :inherited inherited)))
        (when (some
               #'(lambda (setf-func)
                   (when (< min-doc-length (length (documentation setf-func t))) t))
               setf-generics)
          (format stream "@begin(section)~% @title(Обобщенные setf-функции)~% @cl:with-package[name=~S]("
	          (mpkg/obj:obj-name package))
          (map nil #'(lambda (el) (insert-codex-doc el :stream stream :min-doc-length min-doc-length))
               (if sort
                   (sort setf-generics #'string< :key #'(lambda (elem) (mpkg/obj:obj-name elem)))
                   setf-generics))
          (format stream ")~%@end(section)~%"))))))

(defun section-classes (package-name
                        &key
                          (stream t)
                          (external t)
                          (internal nil)
                          (inherited nil)
                          (sort t)
                          (min-doc-length *min-doc-length*)
                        &aux (package (find-package package-name)))
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
