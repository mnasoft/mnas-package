;;;; codex.lisp

(in-package :mnas-package)

(annot:enable-annot-syntax)

(defun codex-documentation-html (system-designator package-designator)
    (let ((system (asdf:find-system system-designator)))
      (concatenate 'string
		   (namestring (asdf:system-source-directory system))
		   "docs/build/"
		   (string-downcase (package-name (find-package package-designator)))
		   "/html")))

(export 'make-codex-documentation )
(defun make-codex-documentation (system-designator package-designator)
  (let ((pkg  package-designator)
	(sys  system-designator)
	(fpath (codex-documentation-html system-designator package-designator)))
    (break ":100000000000")
    (codex:document sys)
    (break ":200000000000")
    (mnas-package:view-call-graph   pkg :out-type "png" :viewer nil :fpath fpath :fname "call-graph" :system-name sys)
    (break ":3")
    (mnas-package:view-system-graph sys :out-type "png" :viewer nil :fpath fpath :fname "system-graph")
    (break ":4")
    (mnas-package:view-class-graph  pkg :out-type "png" :viewer nil :fpath fpath :fname "class-graph")
    (break ":5")    
    (mnas-package:view-symbol-graph pkg :out-type "png" :viewer nil :fpath fpath :fname "symbol-graph"))
      (break ":6"))
