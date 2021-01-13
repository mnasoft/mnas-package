;;;; tests/main.lisp

(in-package :mnas-package/tests)

(def-suite main
  :description "Мастер-набор всех тестов проекта mnas-package."
  :in all)

(in-suite main)

(def-test section-variables ()
  (is-true (= 5 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::section-variables :mnas-package/example :stream os)
                    (get-output-stream-string os))))))
  (is-true (= 6 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::section-variables :mnas-package/example :internal t :stream os)
                    (get-output-stream-string os))))))
  (is-true (= 5 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::section-variables :mnas-package/example :stream os :min-doc-length 10)
                    (get-output-stream-string os))))))
  (is-true (= 7 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::section-variables :mnas-package/example :internal t :stream os :min-doc-length 10)
                    (get-output-stream-string os)))))))

(def-test section-functions ()
  (is-true (= 5 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::section-functions :mnas-package/example :stream os)
                    (get-output-stream-string os))))))
  (is-true (= 7 (length
                  (mnas-string:split
                   (format nil "~%") 
                   (let ((os (make-string-output-stream )))
                     (mpkg::section-functions
                      :mnas-package/example :internal t :stream os)
                     (get-output-stream-string os))))))
  (is-true (= 6 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::section-functions
                     :mnas-package/example :stream os :min-doc-length 10)
                    (get-output-stream-string os))))))
  (is-true (= 10 (length
                  (mnas-string:split
                   (format nil "~%") 
                   (let ((os (make-string-output-stream )))
                     (mpkg::section-functions
                      :mnas-package/example :internal t :stream os :min-doc-length 10)
                     (get-output-stream-string os)))))))

(def-test section-setf-functions ()
  (is-true (= 4 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::section-setf-functions :mnas-package/example :stream os)
                    (get-output-stream-string os))))))
  (is-true (= 6 (length
                  (mnas-string:split
                   (format nil "~%") 
                   (let ((os (make-string-output-stream )))
                     (mpkg::section-setf-functions
                      :mnas-package/example :internal t :stream os)
                     (get-output-stream-string os))))))
  (is-true (= 4 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::section-setf-functions
                     :mnas-package/example :stream os :min-doc-length 10)
                    (get-output-stream-string os))))))
  (is-true (= 6 (length
                  (mnas-string:split
                   (format nil "~%") 
                   (let ((os (make-string-output-stream )))
                     (mpkg::section-setf-functions
                      :mnas-package/example :internal t :stream os :min-doc-length 10)
                     (get-output-stream-string os)))))))

(def-test section-generics/test ()
  (is-true (= 5 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::section-generics :mnas-package/example :stream os)
                    (get-output-stream-string os))))))
  (is-true (= 5 (length
                  (mnas-string:split
                   (format nil "~%") 
                   (let ((os (make-string-output-stream )))
                     (mpkg::section-generics :mnas-package/example :internal t :stream os)
                     (get-output-stream-string os))))))
  (is-true (= 5 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::section-generics :mnas-package/example :stream os :min-doc-length 10)
                    (get-output-stream-string os))))))
  (is-true (= 6 (length
                  (mnas-string:split
                   (format nil "~%") 
                   (let ((os (make-string-output-stream )))
                     (mpkg::section-generics :mnas-package/example :internal t :stream os :min-doc-length 10)
                     (get-output-stream-string os)))))))

(def-test section-methods ()
  (is-true (= 6 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::section-methods :mnas-package/example :stream os)
                    (get-output-stream-string os))))))
  (is-true (= 6 (length
                  (mnas-string:split
                   (format nil "~%") 
                   (let ((os (make-string-output-stream )))
                     (mpkg::section-methods :mnas-package/example :internal t :stream os)
                     (get-output-stream-string os))))))
  (is-true (= 6 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::section-methods :mnas-package/example :stream os :min-doc-length 10)
                    (get-output-stream-string os))))))
  (is-true (= 8 (length
                  (mnas-string:split
                   (format nil "~%") 
                   (let ((os (make-string-output-stream )))
                     (mpkg::section-methods :mnas-package/example :internal t :stream os :min-doc-length 10)
                     (get-output-stream-string os)))))))

(def-test section-classes ()
  (is-true (= 5 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::section-classes :mnas-package/example :stream os)
                    (get-output-stream-string os))))))
  (is-true (= 7 (length
                  (mnas-string:split
                   (format nil "~%") 
                   (let ((os (make-string-output-stream )))
                     (mpkg::section-classes :mnas-package/example :internal t :stream os)
                     (get-output-stream-string os))))))
  (is-true (= 5 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::section-classes :mnas-package/example :stream os :min-doc-length 10)
                    (get-output-stream-string os))))))
  (is-true (= 10 (length
                  (mnas-string:split
                   (format nil "~%") 
                   (let ((os (make-string-output-stream )))
                     (mpkg::section-classes :mnas-package/example :internal t :stream os :min-doc-length 10)
                     (get-output-stream-string os)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                 'mnas-package/example:*a*
                 'mnas-package/example::*b*
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
                            (sb-mop:generic-function-methods
                             (uiop/common-lisp:ensure-generic-function el)))
                        '(mnas-package/example:m-foo mnas-package/example::m-foo-short)))))))
      (get-output-stream-string os))
    "
  @cl:doc(variable *a*)
  @cl:doc(variable *a*)
  @cl:doc(variable *b*)
  @cl:doc(variable *b*)
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






