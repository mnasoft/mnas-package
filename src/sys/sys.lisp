;;;; ./src/core/core.lisp

(defpackage #:mnas-package/sys
  (:use #:cl )
  (:nicknames "MPKG/SYS")
  (:export version)
  (:documentation " Пакет mnas-package/sys предназначена для извлечения
информации из asdf-систем."))

(in-package #:mnas-package/sys)

(defun version (system-designator)
  "@b(Описание:) функция @b(system-version) возвращает версию системы
@b(system-designator)"
  (let ((system (asdf:find-system system-designator nil)))
    (when (and system (slot-boundp system 'asdf:version))
      (asdf:component-version system))))
