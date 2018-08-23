;;;; mnas-call-graph.lisp

(in-package #:cl-user)

(defpackage #:mnas-call-graph
  (:use #:cl)
  (:export ;read-file ;defun-code ;defun-name ;defmethod-code ;defmethod-name ;def-name
   package-symbols
   package-symbols-by-category
   package-function-symbols
   defu-defm-name
   who-calls
   who-calls-lst
   make-call-praph))

(in-package #:mnas-call-graph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun package-symbols (package-name &aux (lst nil) (package (find-package package-name)))
  "Выполнят поиск всех символов, определенных пакетом package-name
 Примеры использования:
 =====================
 (package-symbols 'mnas-call-graph)
 (package-symbols :mnas-call-graph)
 (package-symbols \"MNAS-CALL-GRAPH\")"
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
 (package-symbols-by-category 'mnas-call-graph :internal nil) ;; отбор только внешних символов;
 (package-symbols-by-category :mnas-call-graph)               ;; отбор внешних и внутренних символов;
 (package-symbols-by-category \"MNAS-CALL-GRAPH\" 
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

(defun package-function-symbols
    (package-name
     &aux (lst (list))
       (package (find-package package-name)))
  "Retrieves all function symbols from a package."
  (declare ((or package string symbol) package-name))
  (the
   list
   (cond
     (package
      (do-all-symbols (symb package)
	(when (and (fboundp symb) (eql (symbol-package symb) package))
	  (push symb lst)))
      lst)
     (t (error "~S does not designate a package" package-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun defu-defm-name (func)
    (cond
      ((listp (first func))
       (second (first func)))
      ((null (listp (first func)))
       (first func))))

(defun who-calls (func)
  (let
      ((rez (swank/backend:who-calls func))
       (func-str (string-downcase (string func))))
    (mapcar
     #'(lambda (el1)
	 (list el1 func-str))
     (remove-duplicates 
      (mapcar
       #'(lambda (el)
	   (string-downcase
	    (string
	     (defu-defm-name el))))
       rez)
      :test #'equal))))

(defun who-calls-lst (func-lst)
  (apply #'append
   (mapcar #'who-calls
	   func-lst)))

(defun make-call-praph (package-name 
			&aux (package (find-package package-name)))
  (declare ((or package string symbol) package-name))
  (cond
    (package
     (mnas-graph:view-graph
      (mnas-graph:generate-graph
       (mnas-call-graph:who-calls-lst
	(package-function-symbols package)))))
    (t (error "~S does not designate a package" package-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:cl-user)

(defun package-call-graph (package-name)
  (require package-name)
  (mnas-call-graph:make-call-praph package-name))

