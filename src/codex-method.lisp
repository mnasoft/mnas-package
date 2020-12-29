;;;; ./src/codex-method.lisp

(in-package :mnas-package)

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
  (loop :for method :in (mopp:specializer-direct-methods class)
        :for gf           = (mopp:method-generic-function method)
        :for fname        = (mopp:generic-function-name gf)
        :for fname-string = (when (symbolp fname) (symbol-name fname))
        :when (and (stringp fname-string)
                   (>= (length fname-string)
                       (length prefix))
                   (string= fname-string prefix
                            :end1 (length prefix)
                            :end2 (length prefix)))
          collect gf))

#|
(require :temperature-fild/t-fild)
(find-all-generics (find-class 'mtf/t-fild:<t-fild>) "SPLOT")
(mopp:generic-function-name
 (first                                  
  (mnas-package::find-all-generics (find-class 'mtf/t-fild:<t-fild>) "SPLOT")))
|#
;;;;;;;;;;;;;;;;;;;;

(defun find-all-methods (class prefix)
  "(pprint (find-all-methods (find-class 'mtf:<sector>) \"SEC\"))"
  (loop :for method :in (mopp:specializer-direct-methods class)
        :for gf           = (mopp:method-generic-function method)
        :for fname        = (mopp:generic-function-name gf)
        :for fname-string = (when (symbolp fname) (symbol-name fname))
        :when (and (stringp fname-string)
                   (>= (length fname-string)
                       (length prefix))
                   (string= fname-string prefix
                            :end1 (length prefix)
                            :end2 (length prefix)))
          collect method))

(defun smbl-split (symbol)
  (let ((rez (mnas-string:split ":" (format nil "~S" symbol) :omit-nulls nil)))
    (when (= 3 (length rez)) (setf (second rez) "::"))
    rez))

(defun smbl-name (symbol)
  (let ((lst (smbl-split symbol)))
    (string-downcase
     (ecase (length lst)
       (1 (first lst))
       (2 (second lst))
       (3 (third  lst))))))

(defun smbl-separator-bak (symbol)
  (let ((lst (smbl-split symbol)))
    (string-downcase
     (ecase (length lst)
       (1 "")
       (2 ":")
       (3 "::")))))

(defmethod smbl-separator ((symbol symbol))
  (let ((type (nth-value 1 (find-symbol (symbol-name symbol) (symbol-package symbol)))))
         (ecase type
       (:external  "")
       (:inherited ":")
       (:internal  "::"))))

(defmethod smbl-separator ((function function))
  (let ((type (nth-value 1 (find-symbol
                            (symbol-name (function-name function))
                            (symbol-package (function-name function))))))
    (ecase type
      (:external  "")
      (:inherited ":")
      (:internal  "::"))))

(defun smbl-package-bak (symbol)
  (let ((lst (smbl-split symbol)))
    (string-downcase
     (ecase (length lst)
       (1 "")
       (2 (first lst))
       (3 (first lst))))))

(defmethod smbl-package ((symbol symbol))
  (if (eq :external (nth-value 1 (find-symbol (symbol-name '*mmm*)))) ""
      (package-name (symbol-package symbol))))

(defmethod smbl-package ((function function))
  (if (eq :external (nth-value 1 (find-symbol (symbol-name '*mmm*)))) ""
  (package-name (symbol-package symbol))))

(defun smbl-name-downcase (symbol)
  (string-downcase (smbl-name symbol)))

(defun make-doc-for-standard-method (m &key (stream t))
  (block method-name
    (let ((gfn (mopp:generic-function-name (mopp:method-generic-function m))))
      (when (eq (symbol-package gfn) *package*)
        (format stream "~&  @cl:doc(method")
        (format stream " ~s" gfn)
        (let ((mll (mopp:method-lambda-list m))
              (msp (mopp:method-specializers m)))
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
            (format stream ")")))))))

(defun make-doc-method (m &key (stream t) (min-doc-length 80))
  (let ((m-type (type-of m)))
    (case m-type
      ('standard-method
       (when (< min-doc-length (length (documentation m t)))
         (make-doc-for-standard-method m :stream stream)))
      (otherwise "uncnoun"))))

(export '(make-doc-methods))

(defun make-doc-methods (package class prefix &key (stream t) (min-doc-length 80))
  "@b(Описание:) функция @b(make-doc-methods) выводит в поток
@b(stream) раздел документации, подготовленной для вставки в 
scr-файл системы документирования codex. Этот раздел содержит 
методы класса @b(class), имена которых начинаются 
с префикса @b(prefix).
"
  (let ((print-case *print-case*)
        (pkg-old    *package*))
    (setf *print-case* :downcase
          *package* package)
    (format stream " @cl:with-package[name=~S](~%"
            (string-downcase (package-name package)))
    (block make-doc-for-methods
      (map 'nil
           #'(lambda (el)
               (make-doc-method el :stream stream :min-doc-length min-doc-length))
           (find-all-methods class prefix)))
    (format stream ")~%")
    (setf *print-case* print-case
          *package* pkg-old)))

;;;;;;;;;;

#|
(require :temperature-fild)
(with-open-file (os "~/123.scr" :direction :output :if-exists :supersede)
  (make-doc-methods (find-package :mtf/plot)  (find-class 'mtf::<t-fild>) "PLOT" :stream os)
  (make-doc-methods (find-package :mtf/splot) (find-class 'mtf::<t-fild>) "SPLOT" :stream os))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-doc-generic (g  &key (stream t)  (min-doc-length 80))
   (let ((gfn (mopp:generic-function-name g)))
     (when (and (eq (symbol-package gfn) *package*)
                (< min-doc-length (length (documentation g t))))
        (format stream "~&  @cl:doc(generic")
        (format stream " ~s)" gfn))))

(export '(make-doc-generics))

(defun make-doc-generics (package class prefix &key (stream t) (min-doc-length 80))
  "@b(Описание:) функция @b(make-doc-methods) выводит в поток
@b(stream) раздел документации, подготовленной для вставки в 
scr-файл системы документирования codex. Этот раздел содержит 
методы класса @b(class), имена которых начинаются 
с префикса @b(prefix).
"
  (let ((print-case *print-case*)
        (pkg-old    *package*))
    (setf *print-case* :downcase
          *package* package)
    (format stream " @cl:with-package[name=~S](~%"
            (string-downcase (package-name package)))
    (block make-doc-for-generics
      (map 'nil
           #'(lambda (el)
               (make-doc-generic el :stream stream :min-doc-length min-doc-length))
           (find-all-generics class prefix)))
    (format stream ")~%")
    (setf *print-case* print-case
          *package* pkg-old)))

#|
(require :temperature-fild)
(with-open-file (os "~/123.scr" :direction :output :if-exists :supersede)
  (make-doc-generics (find-package 'mtf) (find-class 'mtf/t-fild::<t-fild>) "" :stream os :min-doc-length 50))
|#
