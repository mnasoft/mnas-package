;;;; ./tests/make-call-graph.lisp

(length
 (mnas-string:split (format nil "~%") 
                    (let ((os (make-string-output-stream )))
                      (mpkg::make-codex-section-methods :mnas-package/example :internal t :stream os)
                      (get-output-stream-string os))))


; @begin(section)
;  @title(Методы)
;  @cl:with-package[name="MNAS-PACKAGE/EXAMPLE"](
;   @cl:doc(method m-foo :around (x <a>) (y <b>) z)
;   @cl:doc(method m-foo :before (x <a>) (y <b>) z)
;   @cl:doc(method m-foo :after (x <a>) (y <b>) z)
;   @cl:doc(method m-foo (x <a>) (y <b>) z)
;   @cl:doc(method m-foo :around (x <a>) (y <b>) (z <c>))
;   @cl:doc(method m-foo :before (x <a>) (y <b>) (z <c>))
;   @cl:doc(method m-foo :after (x <a>) (y <b>) (z <c>))
;   @cl:doc(method m-foo (x <a>) (y <b>) (z <c>)))
; @end(section)
;  => :UPCASE

(mpkg::make-codex-section-methods :mnas-package/example :internal t)

(mpkg::make-codex-documentation :mnas-package/example :internal t)

(mpkg::make-codex-section-variables :mnas-package/example :internal t)

(in-package :mnas-package/example)

(mpkg::insert-codex-doc (nth 1 (mpkg/pkg:package-classes :mnas-package/example :external t :internal t)))

 @begin(section)
  @title(Методы)
  @cl:with-package[name="MNAS-PACKAGE/EXAMPLE"](
   @cl:doc(method m-foo :around (x <a>) (y <b>) z)
   @cl:doc(method m-foo :before (x <a>) (y <b>) z)
   @cl:doc(method m-foo :after (x <a>) (y <b>) z)
   @cl:doc(method m-foo (x <a>) (y <b>) z)
   @cl:doc(method m-foo :around (x <a>) (y <b>) (z <c>))
   @cl:doc(method m-foo :before (x <a>) (y <b>) (z <c>))
   @cl:doc(method m-foo :after (x <a>) (y <b>) (z <c>))
   @cl:doc(method m-foo (x <a>) (y <b>) (z <c>)))
 @end(section)

