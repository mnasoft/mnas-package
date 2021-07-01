;;;; ./src/tests/obj.lisp
(in-package :mnas-package/tests)

(def-suite obj
  :description "Мастер-набор тестов пакета mnas-package/obj"
  :in all)

(in-suite obj)

(def-test obj-name ()
  (let ((pkg (find-package :mnas-package/example)))
  (is-true (equal (multiple-value-list (mpkg/obj:obj-name (find-symbol "*A-EXP*" pkg)))
                  '(mnas-package/example:*a-exp* :SYMBOL)))
  (is-true (equal (multiple-value-list (mpkg/obj:obj-name (find-symbol "*B-EXP*" pkg)))
                  '(mnas-package/example:*b-exp* :SYMBOL)))
  (is-true (equal (multiple-value-list (mpkg/obj:obj-name (find-symbol "*C*" pkg)))
                  '(mnas-package/example::*c* :SYMBOL)))
  (is-true (equal (multiple-value-list (mpkg/obj:obj-name (find-symbol "FOO" pkg)))
                  '(MNAS-PACKAGE/EXAMPLE::FOO :SYMBOL)))
  (is-true (equal (multiple-value-list (mpkg/obj:obj-name (uiop:ensure-function 'mnas-package/example::foo)))
                  '(MNAS-PACKAGE/EXAMPLE::FOO :FUNCTION)))
  (is-true (equal (multiple-value-list (mpkg/obj:obj-name (uiop/common-lisp:ensure-generic-function 'mnas-package/example::m-foo)))
                  '(MNAS-PACKAGE/EXAMPLE::M-FOO :GENERIC-FUNCTION)))
  (map nil #'(lambda (el)
               (is-true (equal (multiple-value-list (mpkg/obj:obj-name el))
                               '(MNAS-PACKAGE/EXAMPLE::M-FOO :METHOD))))
       (closer-mop:generic-function-methods (uiop/common-lisp:ensure-generic-function 'mnas-package/example::m-foo)))
  (is-true (equal (multiple-value-list (mpkg/obj:obj-name (find-class 'mnas-package/example:<c>)))
                  '(MNAS-PACKAGE/EXAMPLE:<C> :CLASS)))))

(def-test obj-package ()
  (let ((pkg (find-package :mnas-package/example)))
    (is-true (eq (mpkg/obj:obj-package (find-symbol "*A-EXP*" pkg)) pkg))
    (is-true (eq (mpkg/obj:obj-package (find-symbol "*B-EXP*" pkg)) pkg))
    (is-true (eq (mpkg/obj:obj-package (find-symbol "*C*" pkg)) pkg))
    (is-true (eq (mpkg/obj:obj-package (find-symbol "FOO" pkg)) pkg))
    (is-true (eq (mpkg/obj:obj-package
                  (uiop:ensure-function 'mnas-package/example::foo))
             pkg))
    (is-true (eq (mpkg/obj:obj-package
                  (uiop/common-lisp:ensure-generic-function 'mnas-package/example::m-foo))
                 pkg))
    (map nil #'(lambda (el)
                 (is-true (eq (mpkg/obj:obj-package el) pkg)))
         (closer-mop:generic-function-methods (uiop/common-lisp:ensure-generic-function 'mnas-package/example::m-foo)))
    (is-true (eq (mpkg/obj:obj-package (find-class 'mnas-package/example:<c>)) pkg))))
