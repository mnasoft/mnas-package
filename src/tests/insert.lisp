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
      (mnas-package::with-downcase
        (mnas-package::with-package (find-package :mnas-package/example)
          (map nil
               #'(lambda (el)
                   (mpkg::insert-codex-doc el :stream os)
                   (mpkg::insert-codex-doc el :stream os :min-doc-length 20)
                   )
               (append
                (list
                 'mnas-package/example:*a-exp*
                 'mnas-package/example:*b-exp*
                 'mnas-package/example::*c*
                 (uiop:ensure-function 'mnas-package/example::foo)
                 (uiop:ensure-function 'mnas-package/example::bar)
                 (uiop:ensure-function 'mnas-package/example:baz)
                 (uiop:ensure-function 'mnas-package/example::foo-short)
                 (uiop:ensure-function 'mnas-package/example::bar-short)
                 (uiop:ensure-function 'mnas-package/example:baz-short)
                 (uiop/common-lisp:ensure-generic-function 'mnas-package/example:m-foo)
                 (uiop/common-lisp:ensure-generic-function 'mnas-package/example::m-foo-short)
                 (find-class 'mnas-package/example::<a>)
                 (find-class 'mnas-package/example::<b>)
                 (find-class 'mnas-package/example:<c>)
                 (find-class 'mnas-package/example::<a-short>)
                 (find-class 'mnas-package/example::<b-short>)
                 (find-class 'mnas-package/example::<c-short>))
                (apply #'append 
                       (mapcar
                        #'(lambda (el)
                            (closer-mop:generic-function-methods
                             (uiop/common-lisp:ensure-generic-function el)))
                        '(mnas-package/example:m-foo mnas-package/example::m-foo-short)))))))
      (get-output-stream-string os))
    "
  @cl:doc(variable *a-exp*)
  @cl:doc(variable *b-exp*)
  @cl:doc(variable *c*)
  @cl:doc(function foo)
  @cl:doc(function foo)
  @cl:doc(function bar)
  @cl:doc(function bar)
  @cl:doc(function baz)
  @cl:doc(function baz)
  @cl:doc(function foo-short)
  @cl:doc(function bar-short)
  @cl:doc(function baz-short)
  @cl:doc(generic m-foo)
  @cl:doc(generic m-foo)
  @cl:doc(generic m-foo-short)
  @cl:doc(class <a>)
  @cl:doc(class <a>)
  @cl:doc(class <b>)
  @cl:doc(class <b>)
  @cl:doc(class <c>)
  @cl:doc(class <c>)
  @cl:doc(class <a-short>)
  @cl:doc(class <b-short>)
  @cl:doc(class <c-short>)
  @cl:doc(method m-foo (x <a>) (y <b>) z)
  @cl:doc(method m-foo (x <a>) (y <b>) z)
  @cl:doc(method m-foo (x <a>) (y <b>) (z <c>))
  @cl:doc(method m-foo (x <a>) (y <b>) (z <c>))
  @cl:doc(method m-foo-short (x <a>) (y <b>) z)
  @cl:doc(method m-foo-short (x <a>) (y <b>) (z <c>))")))






