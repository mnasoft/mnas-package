;;;; demos.lisp

(in-package #:mnas-package)

(export 'lll)
(defparameter lll '(1 2 3))

(defun mnas-package-demo-1 ()
  (require :mnas-string)
  (package-symbols-by-category :mnas-package :internal nil))

(defun mnas-package-demo-2 ()
  (require :mnas-string)
  (package-symbols-by-category :mnas-package))

(defun mnas-package-demo-3 ()
  (make-call-praph :mnas-package))

(defun mnas-package-demo-10 ()
  (require :mnas-string)
  (make-call-praph :mnas-string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn 
  (defclass mnas-package-a1 ()      ())
  (defclass mnas-package-a2 (mnas-package-a1)    ())
  (defclass mnas-package-a3 (mnas-package-a2)    ())
  (defclass mnas-package-a4 (mnas-package-a3)    ())

  (defclass mnas-package-b1 ()      ())
  (defclass mnas-package-b2 (mnas-package-b1)    ())
  (defclass mnas-package-b3 (mnas-package-b2)    ())
  (defclass mnas-package-b4 (mnas-package-b3)    ())

  (defclass mnas-package-c1 (mnas-package-a3 mnas-package-b3) ()))

(defun mnas-package-demo-11 ()
  (package-class-graph :mnas-package)
  (package-call-graph :mnas-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mnas-package-demo ()
  (mnas-package-demo-1)
  (mnas-package-demo-2)
  (mnas-package-demo-3)
  (mnas-package-demo-10)
  (mnas-package-demo-11)
  )

(progn 
  (format t "~%~%~%")
  (format t "mnas-package DEMOS~%")
  (format t "==================~%")
  (format t "(mnas-package:mnas-package-demo-1)~%")
  (format t "(mnas-package:mnas-package-demo-2)~%")
  (format t "(mnas-package:mnas-package-demo-3)~%")
  (format t "(mnas-package:mnas-package-demo-10)~%")
  (format t "(mnas-package:mnas-package-demo-11)"))
