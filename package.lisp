;;;; package.lisp

(defpackage #:mnas-package
  (:use #:cl )  ;;;; #:mnas-package/make-graph
  (:nicknames "MPKG")
  (:intern insert-codex-doc)
  (:export class-undirect-subclasses)
  (:export make-codex-documentation)
  (:intern make-codex-section-package ;; Информация о пакете  пока не реализована
           make-codex-section-system ;; Информация о системе пока не реализована
           make-codex-section-variables 
           make-codex-section-functions
           make-codex-section-generics
           make-codex-section-methods
           make-codex-section-classes)
  (:export make-codex-graphs)
  (:export view-call-graph
	   view-system-graph
           view-class-graph
           view-symbol-graph)
  (:export make-doc-generics
           make-doc-methods)
  (:documentation
   "
 Система mnas-package предназначена для извлечения информации из asdf-систем.

 Извлеченная информация представляется в виде графов.

 Система позволяет построить следующие графы:
@begin(list)
 @item(зависимостей систем @image[src=./system-graph-mnas-package.gv.png]())
 @item(вызовов функций     @image[src=./call-graph-mnas-package.gv.png]())
 @item(использования символов функциями @image[src=./symbol-graph-mnas-package.gv.png]())
 @item(наследования классов  @image[src=./class-graph-mnas-package.gv.png]())
@end(list)"
   ))

;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
