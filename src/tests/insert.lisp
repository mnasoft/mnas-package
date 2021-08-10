;;;; tests/main.lisp

(in-package :mnas-package/tests)

(def-suite insert
  :description "Тестирование методов insert-codex-doc."
  :in main)

(in-suite insert)

(def-test insert ()
  (is-true
   (equal
    (let ((os (make-string-output-stream)))
      (mnas-package/sec:with-downcase
        (mnas-package/sec:with-package (find-package :mnas-package/example)
          (map nil
               #'(lambda (el)
                   (mpkg/sec:insert-codex-doc el :stream os)
                   (mpkg/sec:insert-codex-doc el :stream os :min-doc-length 20)
                   )
               (append
                (list
                 'mnas-package/example:*v-a-exp*
                 'mnas-package/example:*v-b-exp*
                 'mnas-package/example:*v-c-exp*
                 (uiop:ensure-function 'mnas-package/example:f-a-exp)
                 (uiop:ensure-function 'mnas-package/example:f-b-exp)
                 (uiop:ensure-function 'mnas-package/example:f-c-exp)
                 (uiop:ensure-function 'mnas-package/example::f-a-int)
                 (uiop:ensure-function 'mnas-package/example::f-b-int)
                 (uiop:ensure-function 'mnas-package/example::f-a-int)
                 (uiop/common-lisp:ensure-generic-function 'mnas-package/example:m-a-exp)
                 (uiop/common-lisp:ensure-generic-function 'mnas-package/example:m-b-exp)
                 (find-class 'mnas-package/example::<c-a-int>)
                 (find-class 'mnas-package/example::<c-b-int>)
                 (find-class 'mnas-package/example:<c-c-exp>)
                 (find-class 'mnas-package/example:<c-a-exp>)
                 (find-class 'mnas-package/example:<c-b-exp>)
                 (find-class 'mnas-package/example:<c-c-exp>))
                (apply #'append 
                       (mapcar
                        #'(lambda (el)
                            (closer-mop:generic-function-methods
                             (uiop/common-lisp:ensure-generic-function el)))
                        '(mnas-package/example:m-a-exp mnas-package/example::m-foo-short)))))))
      (get-output-stream-string os))
    "
  @cl:doc(variable *v-a-exp*)
  @cl:doc(variable *v-a-exp*)
  @cl:doc(variable *v-b-exp*)
  @cl:doc(variable *v-b-exp*)
  @cl:doc(variable *v-c-exp*)
  @cl:doc(variable *v-c-exp*)
  @cl:doc(function f-a-exp)
  @cl:doc(function f-a-exp)
  @cl:doc(function f-b-exp)
  @cl:doc(function f-b-exp)
  @cl:doc(function f-c-exp)
  @cl:doc(function f-c-exp)
  @cl:doc(function f-a-int)
  @cl:doc(function f-a-int)
  @cl:doc(function f-b-int)
  @cl:doc(function f-b-int)
  @cl:doc(function f-a-int)
  @cl:doc(function f-a-int)
  @cl:doc(generic m-a-exp)
  @cl:doc(generic m-a-exp)
  @cl:doc(generic m-b-exp)
  @cl:doc(generic m-b-exp)
  @cl:doc(class <c-a-int>)
  @cl:doc(class <c-a-int>)
  @cl:doc(class <c-b-int>)
  @cl:doc(class <c-b-int>)
  @cl:doc(class <c-c-exp>)
  @cl:doc(class <c-c-exp>)
  @cl:doc(class <c-a-exp>)
  @cl:doc(class <c-a-exp>)
  @cl:doc(class <c-b-exp>)
  @cl:doc(class <c-b-exp>)
  @cl:doc(class <c-c-exp>)
  @cl:doc(class <c-c-exp>)
  @cl:doc(method m-a-exp (x <c-a-int>) (y <c-b-int>) (z <c-c-exp>))
  @cl:doc(method m-a-exp (x <c-a-int>) (y <c-b-int>) (z <c-c-exp>))")))






