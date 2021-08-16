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
      '((:mnas-package         :mnas-package)
        (:mnas-package/sys     nil)
        (:mnas-package/view    nil)
        (:mnas-package/make    nil)
        (:mnas-package/pkg     nil)
        (:mnas-package/obj     nil)
        (:mnas-package/sec     nil :internal t)
        (:mnas-package/example nil :internal t)
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
      :mnas-package/sec
      )
    :do (mnas-package:make-codex-graphs i i)))

(defun make-all (&aux
                   (of (if (find (uiop:hostname)
                                 mnas-package:*intranet-hosts*
                                 :test #'string=)
                           '(:type :multi-html :template :gamma)
                           '(:type :multi-html :template :minima))))
  (mnas-package:make-html-path :mnas-package)
  (make-document)
  (make-graphs)
  (mnas-package:make-mainfest-lisp
   '(:mnas-package)
   "Mnas-Package"
   '("Nick Matvyeyev")
   (mnas-package:find-sources "mnas-package")
   :output-format of)
  (codex:document :mnas-package)
  (make-graphs)
  (mnas-package:copy-doc->public-html "mnas-package")
  (mnas-package:rsync-doc "mnas-package"))

#+nil
(make-all)
