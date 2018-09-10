;;;; demos.lisp

(in-package #:mnas-package)

(export 'demo-1)
(defun demo-1 ()
  (require :mnas-string)
  (package-symbols-by-category :mnas-package :internal nil))

(export 'demo-2)
(defun demo-2 ()
  (require :mnas-string)
  (package-symbols-by-category :mnas-package))

(export 'demo-3)
(defun demo-3 ()
  (make-call-praph :mnas-package))

(export 'demo-10)
(defun demo-10 ()
  (require :mnas-string)
  (make-call-praph :mnas-string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn 
  (defclass a1 ()      ())
  (defclass a2 (a1)    ())
  (defclass a3 (a2)    ())
  (defclass a4 (a3)    ())

  (defclass b1 ()      ())
  (defclass b2 (b1)    ())
  (defclass b3 (b2)    ())
  (defclass b4 (b3)    ())

  (defclass c1 (a3 b3) ()))

(export 'demo-11)
(defun demo-11 ()
  (make-call-praph  :mnas-package)
  (make-class-graph :mnas-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn 
  (format t "~%~%~%")
  (format t "mnas-package DEMOS~%")
  (format t "==================~%")
  (format t "(mnas-package:demo-1)~%")
  (format t "(mnas-package:demo-2)~%")
  (format t "(mnas-package:demo-3)~%")
  (format t "(mnas-package:demo-10)~%")
  (format t "(mnas-package:demo-11)"))
