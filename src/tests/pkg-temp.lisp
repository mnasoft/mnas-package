(in-package :mnas-package/tests)

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
'(#<STANDARD-GENERIC-FUNCTION (COMMON-LISP:SETF MNAS-PACKAGE/EXAMPLE:M-FOO) (4)>
  #<STANDARD-GENERIC-FUNCTION (COMMON-LISP:SETF MNAS-PACKAGE/EXAMPLE::<B>-B) (1)>
  #<STANDARD-GENERIC-FUNCTION (COMMON-LISP:SETF MNAS-PACKAGE/EXAMPLE::<A>-A) (1)>)
|#

(mnas-package/pkg:package-generics :mnas-package/example :internal t)
#|
'(#<STANDARD-GENERIC-FUNCTION MNAS-PACKAGE/EXAMPLE:M-FOO (8)>
  #<STANDARD-GENERIC-FUNCTION MNAS-PACKAGE/EXAMPLE::M-FOO-SHORT (8)>)
|#

(mnas-package/pkg:package-methods :mnas-package/example :internal t)
#|
'(#<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE::M-FOO-SHORT (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> MNAS-PACKAGE/EXAMPLE:<C>) {1001D1B523}>
  #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE::M-FOO-SHORT :AFTER (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> MNAS-PACKAGE/EXAMPLE:<C>) {1001D1B513}>
  #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE::M-FOO-SHORT :BEFORE (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> MNAS-PACKAGE/EXAMPLE:<C>) {1001D1B503}>
  #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE::M-FOO-SHORT :AROUND (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> MNAS-PACKAGE/EXAMPLE:<C>) {1001D1B4F3}>
  #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE::M-FOO-SHORT (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> T) {10019E2653}>
  #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE::M-FOO-SHORT :AFTER (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> T) {10019E2643}>
  #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE::M-FOO-SHORT :BEFORE (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> T) {10019E2633}>
  #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE::M-FOO-SHORT :AROUND (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> T) {10019E2623}>
  #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE:M-FOO (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> MNAS-PACKAGE/EXAMPLE:<C>) {1001D1B563}>
  #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE:M-FOO :AFTER (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> MNAS-PACKAGE/EXAMPLE:<C>) {1001D1B553}>
  #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE:M-FOO :BEFORE (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> MNAS-PACKAGE/EXAMPLE:<C>) {1001D1B543}>
  #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE:M-FOO :AROUND (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> MNAS-PACKAGE/EXAMPLE:<C>) {1001D1B533}>
  #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE:M-FOO (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> T) {10019E2693}>
  #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE:M-FOO :AFTER (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> T) {10019E2683}>
  #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE:M-FOO :BEFORE (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> T) {10019E2673}>
  #<STANDARD-METHOD MNAS-PACKAGE/EXAMPLE:M-FOO :AROUND (MNAS-PACKAGE/EXAMPLE::<A> MNAS-PACKAGE/EXAMPLE::<B> T) {10019E2663}>)
|#

(mnas-package/pkg:package-setf-methods :mnas-package/example :internal t)
#|
'(#<STANDARD-METHOD (COMMON-LISP:SETF MNAS-PACKAGE/EXAMPLE:M-FOO) (NUMBER MNAS-PACKAGE/EXAMPLE::<B>) {100805D593}>
  #<STANDARD-METHOD (COMMON-LISP:SETF MNAS-PACKAGE/EXAMPLE:M-FOO) (T MNAS-PACKAGE/EXAMPLE::<B>) {100805D363}>
  #<STANDARD-METHOD (COMMON-LISP:SETF MNAS-PACKAGE/EXAMPLE:M-FOO) (NUMBER MNAS-PACKAGE/EXAMPLE::<A>) {100805D5A3}>
  #<STANDARD-METHOD (COMMON-LISP:SETF MNAS-PACKAGE/EXAMPLE:M-FOO) (T MNAS-PACKAGE/EXAMPLE::<A>) {100805D373}>
  #<SB-MOP:STANDARD-WRITER-METHOD (COMMON-LISP:SETF MNAS-PACKAGE/EXAMPLE::<B>-B), slot:MNAS-PACKAGE/EXAMPLE::B, (T MNAS-PACKAGE/EXAMPLE::<B>) {100805D383}>
  #<SB-MOP:STANDARD-WRITER-METHOD (COMMON-LISP:SETF MNAS-PACKAGE/EXAMPLE::<A>-A), slot:MNAS-PACKAGE/EXAMPLE::A, (T MNAS-PACKAGE/EXAMPLE::<A>) {100805D393}>)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(map 'nil #'(lambda (el)
             (mnas-package::insert-codex-doc el))
     (mnas-package/pkg:package-setf-generics :mnas-package/example :internal t))
