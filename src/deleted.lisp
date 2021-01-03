;;;; ./src/deleted.lisp

(in-package :mnas-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'use-mnas-package)

(defun use-mnas-package ()
"@b(Описание:) use-mnas-package копирует внешние символы пакета
:mnas-package в пространство имен пакета :cl-user."
  (use-package (find-package :mnas-package) (find-package :cl-user)))

(export 'unuse-mnas-package)

(defun unuse-mnas-package ()
"@b(Описание:) unuse-mnas-package удаляет внешние символы пакета
:mnas-package из пространства имен пакета :cl-user."
  (unuse-package (find-package :mnas-package) (find-package :cl-user)))

(export 'doc-template )
(defun doc-template (&optional (pkg *package*))
"Пример использования:
@begin[lang=lisp](code)
 (doc-template)
@end(code)
"
  (let ((f-b nil)
	(b   nil))
    (map 'nil
	 #'(lambda (el)
	     (when (fboundp (read-from-string el)) (push el f-b))
	     (when (boundp  (read-from-string el)) (push el   b)))
	 (let ((lst ()))                                                     
	   (do-external-symbols (s pkg)
	     (when (eq (find-package pkg) (symbol-package s)) (push (string-downcase (symbol-name s)) lst)))
	   (sort lst #'string> )))
    (format t "@cl:with-package[name=~S](~%" (string-downcase (package-name pkg)))
    (map 'nil #'(lambda (el) (format t "@cl:doc(function ~a)~%" el) ) f-b)
    (format t ")~%~%")
    (format t "@cl:with-package[name=~S](~%" (string-downcase (package-name pkg)))
    (map 'nil #'(lambda (el) (format t "@cl:doc(variable ~a)~%" el) )   b)
    (format t ")~%~%")))

(doc-template)

(defun make-doc-generic (g  &key (stream t)  (min-doc-length 80))
   (let ((gfn (mopp:generic-function-name g)))
     (when (and (eq (symbol-package gfn) *package*)
                (< min-doc-length (length (documentation g t))))
        (format stream "~&  @cl:doc(generic")
        (format stream " ~s)" gfn))))

(defun make-doc-method (m &key (stream t) (min-doc-length 80))
  (let ((m-type (type-of m)))
    (case m-type
      ('standard-method
       (when (< min-doc-length (length (documentation m t)))
         (make-doc-for-standard-method m :stream stream)))
      (otherwise "uncnoun"))))

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

(defun method-name (method)
    "@b(Описание:) функция @b(generic-name) возвращает символ,
представляющий имя метода.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (method-name (first (mopp:generic-function-methods (first (package-generics :dxf)))))
@end(code)
"
  (mopp:generic-function-name
   (mopp:method-generic-function method)))

(defun generic-name (generic)
  "@b(Описание:) функция @b(generic-name) возвращает символ,
представляющий имя обобщенной функции.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (generic-name (first (package-generics :dxf)))
@end(code)
"
  (mopp:generic-function-name generic))

(defun function-name (function)
  "@b(Описание:) функция @b(function-name) возвращает символ,
представляющий имя функции.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (function-name #'function-name)  
 => function-name
@end(code)
"
  (nth-value 2 (function-lambda-expression function)))

(defun make-mnas-systems ()
  "Необходимо сделать описание"
  (let* ((sos (make-string-output-stream))
	 (sis (make-string-input-stream 
	       (progn
		 (when (uiop:directory-exists-p (pathname "d:/PRG/msys32/home/namatv/quicklisp/local-projects/mnas/mnas-systems"))
		   (uiop:delete-directory-tree (pathname "d:/PRG/msys32/home/namatv/quicklisp/local-projects/mnas/mnas-systems/") :validate t))
		 (sb-ext:run-program
		  (cond
		    ((uiop:os-windows-p) "d:/PRG/msys32/usr/bin/bash.exe")
		    (t "/bin/bash"))
		  '("-c" "find /home/namatv/quicklisp/local-projects/ -name '*.asd'") :output sos)
		 (get-output-stream-string sos))))
	 (asd (loop for line = (read-line sis nil nil)
		    while line
		    collect (pathname-name line))))
    (ensure-directories-exist
     (pathname
      (concatenate 'string (namestring (user-homedir-pathname)) "quicklisp/local-projects/mnas/mnas-systems/")))
    (with-open-file
	(asd-file
	 (pathname (concatenate 'string (namestring (user-homedir-pathname)) "quicklisp/local-projects/mnas/mnas-systems/" "mnas-systems.asd"))
	 :direction :output :if-exists :supersede)
      (format asd-file ";;;; mnas-systems.asd~%~%")
      (format asd-file "(defsystem #:mnas-systems~%")
      (format asd-file "  :components ((:file \"mnas-systems\"))~%")
      (format asd-file "  :depends-on (~%")
      (loop for i in asd
	    do (format asd-file "	       #:~a~%" i))
      (format asd-file "  ))"))
    (with-open-file
	(lisp-file
	 (pathname (concatenate 'string (namestring (user-homedir-pathname)) "quicklisp/local-projects/mnas/mnas-systems/" "mnas-systems.lisp"))
	 :direction :output :if-exists :supersede)
      (format lisp-file ";;;; mnas-systems.lisp~%~%")
      (format lisp-file "(defpackage #:mnas-systems)~%~%")
      (format lisp-file "(in-package #:mnas-systems)"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun smbl-name-downcase (symbol)
  (string-downcase (smbl-name symbol)))

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

(defun smbl-package-bak (symbol)
  (let ((lst (smbl-split symbol)))
    (string-downcase
     (ecase (length lst)
       (1 "")
       (2 (first lst))
       (3 (first lst))))))

(defun smbl-split (symbol)
  (let ((rez (mnas-string:split ":" (format nil "~S" symbol) :omit-nulls nil)))
    (when (= 3 (length rez)) (setf (second rez) "::"))
    rez))

(defmethod smbl-package ((symbol symbol))
  (if (eq :external (nth-value 1 (find-symbol (symbol-name '*mmm*)))) ""
      (package-name (symbol-package symbol))))

(defmethod smbl-package ((function function))
  (if (eq :external (nth-value 1 (find-symbol (symbol-name '*mmm*)))) ""
      (package-name (symbol-package (function-name function)))))

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
