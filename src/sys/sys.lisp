;;;; ./src/core/core.lisp

(defpackage :mnas-package/sys
  (:use #:cl )
  (:nicknames "MPKG/SYS")
  (:export version)
  (:documentation "@b(Описание:) Пакет @b(mnas-package/sys)
  предназначен для извлечения информации из asdf-систем."))

(in-package :mnas-package/sys)

(defun version (system-designator)
  (let ((system (asdf:find-system system-designator nil)))
    (when (and system (slot-boundp system 'asdf:version))
      (asdf:component-version system))))
