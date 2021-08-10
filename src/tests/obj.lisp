;;;; ./src/tests/obj.lisp
(in-package :mnas-package/tests)

(def-suite obj
  :description "Мастер-набор тестов пакета mnas-package/obj"
  :in all)

(in-suite obj)

#+nil
(defparameter pkg (find-package :mnas-package/example))

(def-test obj-name ()
  (let ((pkg (find-package :mnas-package/example)))
  (is-true (equal (multiple-value-list (mpkg/obj:obj-name (find-symbol "*V-A-EXP*" pkg)))
                  '(mnas-package/example:*v-a-exp* :SYMBOL)))
  (is-true (equal (multiple-value-list (mpkg/obj:obj-name (find-symbol "*V-B-EXP*" pkg)))
                  '(mnas-package/example:*v-b-exp* :SYMBOL)))
  (is-true (equal (multiple-value-list (mpkg/obj:obj-name (find-symbol "*V-C-EXP*" pkg))) 
                  '(mnas-package/example:*v-c-exp* :SYMBOL)))
  (is-true (equal (multiple-value-list (mpkg/obj:obj-name (find-symbol "F-A-EXP" pkg))) 
                  '(mnas-package/example:f-a-exp :SYMBOL)))
  (is-true (equal (multiple-value-list (mpkg/obj:obj-name (uiop:ensure-function 'mnas-package/example:f-a-exp)))
                  '(mnas-package/example:f-a-exp :FUNCTION)))
  (is-true (equal (multiple-value-list (mpkg/obj:obj-name (uiop/common-lisp:ensure-generic-function 'mnas-package/example:m-a-exp)))
                  '(mnas-package/example:m-a-exp :GENERIC)))
  (map nil #'(lambda (el)
               (is-true (equal (multiple-value-list (mpkg/obj:obj-name el))
                               '(mnas-package/example:m-a-exp :METHOD))))
       (closer-mop:generic-function-methods (uiop/common-lisp:ensure-generic-function 'mnas-package/example::m-a-exp)))
  (is-true (equal (multiple-value-list (mpkg/obj:obj-name (find-class 'mnas-package/example:<c-b-exp>)))
                  '(mnas-package/example:<c-b-exp> :CLASS)))))

(def-test obj-package ()
  (let ((pkg (find-package :mnas-package/example)))
    (is-true (eq (mpkg/obj:obj-package (find-symbol "*V-A-EXP*" pkg)) pkg))
    (is-true (eq (mpkg/obj:obj-package (find-symbol "*V-B-EXP*" pkg)) pkg))
    (is-true (eq (mpkg/obj:obj-package (find-symbol "*V-C-EXP*" pkg)) pkg))
    (is-true (eq (mpkg/obj:obj-package (find-symbol "F-A-EXP" pkg)) pkg)) 
    (is-true (eq (mpkg/obj:obj-package
                  (uiop:ensure-function 'mnas-package/example:f-a-exp))
                 pkg))
    (is-true (eq (mpkg/obj:obj-package
                  (uiop/common-lisp:ensure-generic-function 'mnas-package/example::m-foo))
                 pkg))
    (map nil #'(lambda (el)
                 (is-true (eq (mpkg/obj:obj-package el) pkg)))
         (closer-mop:generic-function-methods (uiop/common-lisp:ensure-generic-function 'mnas-package/example:m-a-exp)))
    (is-true (eq (mpkg/obj:obj-package (find-class 'mnas-package/example:<c-b-exp>)) pkg))))


