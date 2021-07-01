;;;; temp.lisp

(in-package :mnas-package)

(defun class-undirect-subclasses (class-01)
"@b(Описание:) функция @b(class-undirect-subclasses)
 выполняет поиск всех подклассов класса class-01 и 
 возвращает список всех найденных классов.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (progn
  (require :dxf)
  (class-undirect-subclasses (find-class 'dxf::object))
  (class-undirect-subclasses (find-class 'number)))
@end(code)
"
  (let ((rez-classes nil)
	(l-not-obr (list class-01)))
    (flet
	((bar (class)
	   (setf l-not-obr (append l-not-obr (closer-mop:class-direct-subclasses class)))))
      (do ((class nil))
	  ((null l-not-obr) rez-classes)
	(setf class (pop l-not-obr))
	(push class rez-classes)
	(bar class)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;










