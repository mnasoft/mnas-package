(in-package :mnas-package/tests)

(def-suite pkg
  :description "Мастер-набор тестов пакета mnas-package/obj"
  :in all)

(in-suite pkg)

(def-test package-by-type-length ()
  (let ((pkg (find-package :mnas-package/example)))
    (is-true (eq  3 (length (mnas-package/pkg:package-variables :mnas-package/example :internal t))))
    (is-true (eq  1 (length (mnas-package/pkg:package-variables :mnas-package/example ))))
    
    (is-true (eq  6 (length (mnas-package/pkg:package-functions :mnas-package/example :internal t))))
    (is-true (eq  2 (length (mnas-package/pkg:package-functions :mnas-package/example))))
    
    (is-true (eq  2 (length (mnas-package/pkg:package-macroses :mnas-package/example :internal t))))
    (is-true (eq  1 (length (mnas-package/pkg:package-macroses :mnas-package/example))))
    
    (is-true (eq  2 (length (mnas-package/pkg:package-setf-functions :mnas-package/example :internal t))))
    (is-true (eq  0 (length (mnas-package/pkg:package-setf-functions :mnas-package/example))))
    
    (is-true (eq  2 (length (mnas-package/pkg:package-generics :mnas-package/example :internal t))))
    (is-true (eq  1 (length (mnas-package/pkg:package-generics :mnas-package/example))))

    (is-true (eq  2 (length (mnas-package/pkg:package-setf-generics :mnas-package/example :internal t))))
    (is-true (eq  1 (length (mnas-package/pkg:package-setf-generics :mnas-package/example))))
    
    (is-true (eq 16 (length (mnas-package/pkg:package-methods :mnas-package/example :internal t))))
    (is-true (eq  8 (length (mnas-package/pkg:package-methods :mnas-package/example))))

    (is-true (eq  6 (length (mnas-package/pkg:package-classes :mnas-package/example :internal t))))
    (is-true (eq  1 (length (mnas-package/pkg:package-classes :mnas-package/example))))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(mnas-package/pkg:package-variables :mnas-package/example :internal t)
#|
'(MNAS-PACKAGE/EXAMPLE:*A*
  MNAS-PACKAGE/EXAMPLE::*C*
  MNAS-PACKAGE/EXAMPLE::*B*)
|#
(mnas-package/pkg:package-functions :mnas-package/example :internal t)
#|
'(#<FUNCTION MNAS-PACKAGE/EXAMPLE:BAZ>
  #<FUNCTION MNAS-PACKAGE/EXAMPLE:BAZ-SHORT>
  #<FUNCTION MNAS-PACKAGE/EXAMPLE::BAR>
  #<FUNCTION MNAS-PACKAGE/EXAMPLE::BAR-SHORT>
  #<FUNCTION MNAS-PACKAGE/EXAMPLE::FOO-SHORT>
  #<FUNCTION MNAS-PACKAGE/EXAMPLE::FOO>)
|#
(mnas-package/pkg:package-macroses :mnas-package/example :internal t)
#|
'(#<FUNCTION (MACRO-FUNCTION MNAS-PACKAGE/EXAMPLE:MAK-A) {52EAB01B}>
  #<FUNCTION (MACRO-FUNCTION MNAS-PACKAGE/EXAMPLE::MAK-A-SHORT) {52EB09AB}>)
|#

(mnas-package/pkg:package-setf-functions :mnas-package/example :internal t)
#|
'(#<FUNCTION (SETF MNAS-PACKAGE/EXAMPLE::SETF-FOO)>
  #<FUNCTION (SETF MNAS-PACKAGE/EXAMPLE::FOO)>)
|#

(mnas-package/pkg:package-setf-generics :mnas-package/example :internal t)
#|
'(#<STANDARD-GENERIC-FUNCTION (COMMON-LISP:SETF MNAS-PACKAGE/EXAMPLE:M-FOO) (0)>
  #<STANDARD-GENERIC-FUNCTION (COMMON-LISP:SETF MNAS-PACKAGE/EXAMPLE::SM-FOO) (0)>)
|#

(mnas-package/pkg:package-generics :mnas-package/example :internal t)
#|
'(#<STANDARD-GENERIC-FUNCTION MNAS-PACKAGE/EXAMPLE:M-FOO (8)>
  #<STANDARD-GENERIC-FUNCTION MNAS-PACKAGE/EXAMPLE::M-FOO-SHORT (8)>)
|#

(mnas-package/pkg:package-methods :mnas-package/example :internal t)
#|
'(#<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE:M-FOO :AROUND (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> T) {1006B58A13}>
  #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE:M-FOO :BEFORE (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> T) {1006B58A23}>
  #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE:M-FOO :AFTER (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> T) {1006B58A33}>
  #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE:M-FOO (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> T) {1006B58A43}>
  #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE:M-FOO :AROUND (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> MNAS-PACKAGE/EXAMPLE:<C>) {1006B59003}>
  #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE:M-FOO :BEFORE (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> MNAS-PACKAGE/EXAMPLE:<C>) {1006B59013}>
  #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE:M-FOO :AFTER (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> MNAS-PACKAGE/EXAMPLE:<C>) {1006B59023}>
  #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE:M-FOO (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> MNAS-PACKAGE/EXAMPLE:<C>) {1006B59033}>
  #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE::M-FOO-SHORT :AROUND (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> T) {1006B589D3}>
  #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE::M-FOO-SHORT :BEFORE (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> T) {1006B589E3}>
  #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE::M-FOO-SHORT :AFTER (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> T) {1006B589F3}>
  #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE::M-FOO-SHORT (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> T) {1006B58A03}>
  #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE::M-FOO-SHORT :AROUND (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> MNAS-PACKAGE/EXAMPLE:<C>) {1006B58FC3}>
  #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE::M-FOO-SHORT :BEFORE (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> MNAS-PACKAGE/EXAMPLE:<C>) {1006B58FD3}>
  #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE::M-FOO-SHORT :AFTER (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> MNAS-PACKAGE/EXAMPLE:<C>) {1006B58FE3}>
  #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE::M-FOO-SHORT (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> MNAS-PACKAGE/EXAMPLE:<C>) {1006B58FF3}>)
|#

(mnas-package/pkg:package-classes :mnas-package/example :internal t)
#|
'(#<STANDARD-CLASS MNAS-PACKAGE/EXAMPLE:<C>> ;
  #<STANDARD-CLASS MNAS-PACKAGE/EXAMPLE::<A>> ;
  #<STANDARD-CLASS MNAS-PACKAGE/EXAMPLE::<B>> ;
  #<STANDARD-CLASS MNAS-PACKAGE/EXAMPLE::<B-SHORT>> ;
  #<STANDARD-CLASS MNAS-PACKAGE/EXAMPLE::<C-SHORT>> ;
  #<STANDARD-CLASS MNAS-PACKAGE/EXAMPLE::<A-SHORT>>) ;
|#
