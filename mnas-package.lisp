;;;; mnas-package.lisp

(in-package #:mnas-package)

(defun package-symbols (package-name &aux (lst nil) (package (find-package package-name)))
  "Выполнят поиск всех символов, определенных пакетом package-name
 Примеры использования:
 =====================
 (package-symbols 'mnas-package)
 (package-symbols :mnas-package)
 (package-symbols (find-package :mnas-package))
 (package-symbols \"MNAS-PACKAGE\")"
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
  "Выполнят поиск всех символов, определенных пакетом package-name,
которые удовлетворяют определенной категории:
 - external  t    - внешиние символы;
 - internal  t    - внутренние символы;
 - inherited nil  - заимствованные символы.
 Примеры использования:
 =====================
 (package-symbols-by-category 'mnas-package :internal nil) ;; отбор только внешних символов;
 (package-symbols-by-category :mnas-package)               ;; отбор внешних и внутренних символов;
 (package-symbols-by-category \"MNAS-PACKAGE\" 
   :internal nil 
   :inherited t) ;; отбор только внешних и заимствованных символов;
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
      (package-symbols package))
     (when external  (setf rez (union rez external-lst)))
     (when internal  (setf rez (union rez internal-lst)))
     (when inherited (setf rez (union rez inherited-lst)))
     rez)
    (t (error "~S does not designate a package" package-name))))

(defun package-filter-symbols (sym-list)
  (let ((rez nil))
    (mapc
     #'(lambda (el) (unless (fboundp el) (push el rez)))
     sym-list)
    rez))

(defun package-filter-functions (sym-list)
  (let ((rez nil))
    (mapc
     #'(lambda (el) (when (fboundp el) (push el rez)))
     sym-list)
    rez))

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

(defun who-calls (func)
  (let
      ((rez (swank/backend:who-calls func))
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

(defun make-call-praph (package-name
			&aux
			  (package (find-package package-name))
			  (pkg-functions nil))
  (declare ((or package string symbol) package-name))
  (cond
    (package
     (setf pkg-functions (package-filter-functions (package-symbols-by-category package)))
     ;;;; (package-function-symbols package)
     (mnas-graph:make-graph
      (who-calls-lst
       pkg-functions)
      :nodes (mapcar #'(lambda (el) (func-to-string el)) pkg-functions)))
    (t (error "~S does not designate a package" package-name))))

(defun package-call-graph (package-name
			   &key
			     (graphviz-prg :filter-dot))
  (when (symbolp package-name) (require package-name))
  (when (stringp package-name) (require package-name))
  (mnas-graph:view-graph (make-call-praph package-name) :graphviz-prg graphviz-prg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun package-classes
    (package-name
     &aux
       (rez nil)
       (class nil)
       (package (find-package package-name)))
  "Возвращает список классов пакета.
   Пример использования:
 (mnas-package::package-classes :mnas-package)
 (mnas-package::package-classes (find-package \"MNAS-PACKAGE\"))
 (mnas-package::package-classes (find-package :mnas-package))"
  (declare ((or package string symbol) package-name))
  (mapc 
   #'(lambda (el)
       (setf class (find-class el nil))
       (when class (push class rez)))
   (package-symbols-by-category package))
  rez)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-class-graph
    (package-name
     &aux
       (package (find-package package-name))
       (graph (make-instance 'mnas-graph:graph)))
  (declare ((or package string symbol) package-name))
  (flet ((find-subclasses (class)
	   (mapcar
	    #'(lambda (el)
		(mnas-graph:insert-to
		 (make-instance
		  'mnas-graph:edge
		  :from (make-instance 'mnas-graph:node :owner graph :name (string (class-name class)))
		  :to   (make-instance 'mnas-graph:node :owner graph :name (string (class-name el))))
		 graph))
	    (sb-mop:class-direct-subclasses class))
	   graph))
    (mapc
     #'(lambda (el)
  	 (mnas-graph:insert-to
	  (make-instance 'mnas-graph:node :owner graph :name (string (class-name el)))
	  graph)
	 (find-subclasses el))
     (package-classes package)))
  graph)

(defun package-class-graph
    (package-name
     &key
       (graphviz-prg :filter-dot))
  "Выводит визуальное представление иерархии классов (графа наследования)
   Пример использования:
 (mnas-package:mnas-package-demo-11)"
  (when (symbolp package-name) (require package-name))
  (when (stringp package-name) (require package-name))
  (mnas-graph:view-graph
   (make-class-graph package-name)
    :graphviz-prg graphviz-prg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun class-undirect-subclasses (class-01)
  "Выполняет поиск всех подклассов класса class-01 и 
возвращает список всех найденных классов.
Пример использования:
 (class-undirect-subclasses (find-class 'number))
"
  (let ((rez-classes nil)
	(l-not-obr (list class-01)))
    (flet
	((bar (class)
	   (format t "~S~%" (class-name class))
	   (setf l-not-obr (append l-not-obr (sb-mop:class-direct-subclasses class)))))
      (do ((class nil))
	  ((null l-not-obr) rez-classes)
	(setf class (pop l-not-obr))
	(push class rez-classes)
	(bar class)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;; (make-mnas-systems)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-symbol-graph
    (package-name
     &aux
       (package (find-package package-name))
       (graph (make-instance 'mnas-graph:graph)))
  (declare ((or package string symbol) package-name))
  (mapc
   #'(lambda (el)
       (mnas-graph:insert-to
	(make-instance 'mnas-graph:node :owner graph :name (string (class-name el)))
	graph)
       (find-subclasses el))
   (package-classes package))
  graph)
