;;;; ./src/docs/docs.lisp

(defpackage #:mnas-package/docs
  (:use #:cl ) ;; :mnas-package/pkg
  (:nicknames "MPKG/DOCS")
  (:export make-all)
  (:documentation "Пакет @b(mnas-package/docs) содержит функции
  генерирования и публикации документации.
"))

(in-package :mnas-package/docs)

(defun make-document ()
    (loop
      :for i :in
      '((:mnas-package      :mnas-package)
        (:mnas-package/sys  nil)
        (:mnas-package/view nil)
        (:mnas-package/make nil)
        (:mnas-package/pkg  nil)
        (:mnas-package/obj  nil)
        (:mnas-package/example nil)
        )
      :do (apply #'mnas-package:document i)))


(defun make-graphs ()
  (loop
    :for i :in
    '(:mnas-package     
      :mnas-package/sys 
      :mnas-package/view  
      :mnas-package/make  
      :mnas-package/pkg   
      :mnas-package/obj   
      )
    :do (mnas-package:make-codex-graphs i i)))

(defun make-all ()
  (make-document)
  (make-graphs)
  (codex:document :mnas-package)
  (make-graphs))
