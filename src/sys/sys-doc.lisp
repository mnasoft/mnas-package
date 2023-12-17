;;;; ./mnas-package/src/sys/sys-doc.lisp

(in-package :mnas-package/sys)

(defmacro make-doc (obj-name obj-type doc-string)
  `(setf (documentation ,obj-name ,obj-type)
         ,doc-string))

(defun find-slot (slot-name class)
  (find slot-name (closer-mop:class-direct-slots (find-class class))
        :key #'closer-mop:slot-definition-name))

(make-doc
  #'MNAS-PACKAGE/SYS:VERSION 'function
  "@b(Описание:) функция @b(system-version) возвращает версию системы,
заданной аргументом @b(system-designator).")
