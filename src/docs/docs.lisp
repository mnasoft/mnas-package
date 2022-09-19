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
      '((:mnas-package         :mnas-package :internal t)
        (:mnas-package/sys     nil           :internal t)
        (:mnas-package/view    nil           :internal t)
        (:mnas-package/make    nil           :internal t)
        (:mnas-package/pkg     nil           :internal t)
        (:mnas-package/obj     nil           :internal t)
        (:mnas-package/sec     nil           :internal t)
        (:mnas-package/example nil           :internal t)
        )
      :do (apply #'mnas-package:document i)))

(defun make-graphs ()
  (loop
    :for j :from 1
    :for i :in
    '(:mnas-package     
      :mnas-package/sys 
      :mnas-package/view  
      :mnas-package/make  
      :mnas-package/pkg   
      :mnas-package/obj
      :mnas-package/sec
      )
    :do (progn
          (mnas-package:make-codex-graphs i i)
          (format t "~A ~A~%" j i))))

(defun make-all (&aux
                   (of (if (find (uiop:hostname)
                                 mnas-package:*intranet-hosts*
                                 :test #'string= :key #'first)
                           '(:type :multi-html :template :gamma)
                           '(:type :multi-html :template :minima))))
  (mnas-package:make-html-path :mnas-package)
  (make-document)
  (mnas-package:make-mainfest-lisp '(:mnas-package)
                                   "Mnas-Package"
                                   '("Mykola Matvyeyev")
                                   (mnas-package:find-sources "mnas-package")
                                   :output-format of)
  (codex:document :mnas-package)
  (make-graphs)
  (mnas-package:copy-doc->public-html "mnas-package")
  (mnas-package:rsync-doc "mnas-package")
  :make-all-finish)

#+nil
(make-all)
