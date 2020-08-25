;;;; cl-user-import-symbols.lisp

(in-package :mnas-package)

(annot:enable-annot-syntax)

@export
@annot.doc:doc
"@b(Описание:) use-mnas-package копирует внешние символы пакета
:mnas-package в пространство имен пакета :cl-user."
(defun use-mnas-package ()
  (use-package (find-package :mnas-package) (find-package :cl-user)))

@export
@annot.doc:doc
"@b(Описание:) unuse-mnas-package удаляет внешние символы пакета
:mnas-package из пространства имен пакета :cl-user."
(defun unuse-mnas-package ()
  (unuse-package (find-package :mnas-package) (find-package :cl-user)))


(defun codex-documentation-html (system-designator package-designator)
    (let ((system (asdf:find-system system-designator)))
      (concatenate 'string
		   (namestring (asdf:system-source-directory system))
		   "docs/build/"
		   (string-downcase (package-name (find-package package-designator)))
		   "/html")))

(defun make-codex-documentation (system-designator package-designator)
    (let ((pkg  package-designator)
	  (fpath (codex-documentation-html system-designator package-designator)))
      (codex:document pkg)
      (mnas-package:view-call-graph   pkg :out-type "png" :viewer nil :fpath fpath :fname "call-graph")
      (mnas-package:view-system-graph pkg :out-type "png" :viewer nil :fpath fpath :fname "system-graph")
      (mnas-package:view-class-graph  pkg :out-type "png" :viewer nil :fpath fpath :fname "class-graph")
      (mnas-package:view-symbol-graph pkg :out-type "png" :viewer nil :fpath fpath :fname "symbol-graph")))
