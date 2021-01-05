;;;; ./tests/make-call-graph.lisp
                        
(mpkg::make-codex-section-methods :mnas-package/example :internal t)

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

