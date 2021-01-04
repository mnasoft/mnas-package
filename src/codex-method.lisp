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

#|
(require :temperature-fild/t-fild)
(find-all-generics (find-class 'mtf/t-fild:<t-fild>) "SPLOT")
(sb-mop:generic-function-name
 (first                                  
  (mnas-package::find-all-generics (find-class 'mtf/t-fild:<t-fild>) "SPLOT")))
|#
;;;;;;;;;;;;;;;;;;;;

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
    (format stream " @cl:with-package[name=~S](~%" (mpkg/obj:obj-name package))
    (block make-doc-for-methods
      (map 'nil
           #'(lambda (el)
               (insert-codex-doc el :stream stream :min-doc-length min-doc-length))
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
    (format stream " @cl:with-package[name=~s](~%" (mpkg/obj:obj-name package))
    (block make-doc-for-generics
      (map 'nil #'(lambda (el) (insert-codex-doc el :stream stream :min-doc-length min-doc-length))
           (find-all-generics class prefix)))
    (format stream ")~%")
    (setf *print-case* print-case
          *package* pkg-old)))

#|
(require :temperature-fild)
(with-open-file (os "~/123.scr" :direction :output :if-exists :supersede)
  (make-doc-generics (find-package 'mtf) (find-class 'mtf/t-fild::<t-fild>) "" :stream os :min-doc-length 50))
|#
