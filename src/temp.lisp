;;;; temp.lisp

(in-package #:mnas-package)


(package-system-graph
 :mnas-package
 :fname "package-system-graph"
 :fpath "/home/namatv/quicklisp/local-projects/mnas/mnas-package/docs/"
 :out-type "png"
 :viewer nil)

(package-call-graph
 :mnas-package
 :fname "package-call-graph"
 :fpath "/home/namatv/quicklisp/local-projects/mnas/mnas-package/docs/"
 :out-type "png"
 :viewer nil)

(package-class-graph
 :mnas-package
 :fname "package-class-graph"
 :fpath "/home/namatv/quicklisp/local-projects/mnas/mnas-package/docs/"
 :out-type "png"
 :viewer nil)

(package-symbol-graph
 :mnas-graph
 :fname "package-symbol-graph"
 :fpath "/home/namatv/quicklisp/local-projects/mnas/mnas-package/docs/"
 :out-type "png"
 :viewer nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-all-methods (class prefix)
"(pprint (find-all-methods (find-class 'mtf:<sector>) \"SEC\"))"
  (loop for method in (sb-mop:specializer-direct-methods class)
        for gf           = (sb-mop:method-generic-function method)
        for fname        = (sb-mop:generic-function-name gf)
        for fname-string = (when (symbolp fname) (symbol-name fname))
        when (and (stringp fname-string)
                  (>= (length fname-string)
                      (length prefix))
                  (string= fname-string prefix
                           :end1 (length prefix)
                           :end2 (length prefix)))
        collect method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(find-all-methods class prefix)

(require :temperature-fild/splot)
		  
(codex-documentation-docs :mnas-package)
