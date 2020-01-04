;;;; demos.lisp

(in-package #:mnas-package)

(annot:enable-annot-syntax)

(defparameter *sample-var-1* '(1 2 3))
(defparameter *sample-var-2* ())
(defparameter *sample-var-3* "3")

@export
(defun mnas-package-demo-1 ()
  *sample-var-1*
  (package-symbols-by-category :mnas-package :internal nil))

@export
(defun mnas-package-demo-2 ()
  *sample-var-2*
  (package-symbols-by-category :mnas-package))

@export
(defun mnas-package-demo-3 ()
  *sample-var-3*
  (make-call-praph :mnas-package))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
(defun mnas-package-demo ()
  (mnas-package-demo-1)
  (mnas-package-demo-2)
  (mnas-package-demo-3)
  (package-call-graph   :mnas-package)
  (package-class-graph  :mnas-package)
  (package-symbol-graph :mnas-package))

(progn 
  (format t "~%~%~%")
  (format t "mnas-package DEMOS~%")
  (format t "==================~%")
  (format t "(mnas-package:mnas-package-demo)~%")
  (format t "(mnas-package:mnas-package-demo-1)~%")
  (format t "(mnas-package:mnas-package-demo-2)~%")
  (format t "(mnas-package:mnas-package-demo-3)~%"))
