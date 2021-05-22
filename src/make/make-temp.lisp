;;;; ./src/make/make-temp.lisp

(in-package :mnas-package/make)

(defun generic-graph (generic
                      &aux
                        (package *package*)
		        (graph (make-instance 'mnas-graph:<graph>)))
  "@b(Описание:) функция @b(generic-graph) возвращает граф параметров
 обобщенной функций. 

 Данный граф должен быть трехуровневым:
@begin(list)
 @item(первый уровень - обобщенная функция;)
 @item(второй - номер по порядку для обязательного параметра и его имя; )
 @item(третий - тип обязательного параметра.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)

@end(code)
  "
  generic
  graph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *g* (first (mnas-package/pkg:package-generics :mnas-package/example)))

(defparameter *graph* (make-instance 'mnas-graph:<graph>))

(mnas-graph:
*g*  ; => #<STANDARD-GENERIC-FUNCTION MNAS-PACKAGE/EXAMPLE:M-C-EXP (1)>

(mnas-package/obj:obj-name
 (first (closer-mop:method-specializers
 (first (closer-mop:generic-function-methods *g*)))))

(defclass <obj-node> (mnas-graph:<node>)
  ((obj :accessor <obj-node>-obj :initarg :obj :initform nil :documentation "Объект"))
  (:documentation "@b(Описание:) класс @b(<obj-node>) представляет с
  присоединенным к ней объектом."))

(defmethod print-object ((obj-node <obj-node>) s)
  (format s "COOOL=~S" (<obj-node>-obj obj-node)))

(defparameter *obj* (make-instance
                     '<obj-node>
                     :obj "123"
                     :owner *graph*))

(first (closer-mop:method-specializers
                                  (first (closer-mop:generic-function-methods *g*))))
(setf (<obj-node>-obj *obj*)
      (first (closer-mop:method-specializers
                                  (first (closer-mop:generic-function-methods *g*)))))


(mnas-graph:insert-to
)
*graph*
