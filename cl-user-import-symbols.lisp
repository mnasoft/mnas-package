;;;; cl-user-import-symbols.lisp

(in-package :mnas-package)

(annot:enable-annot-syntax)

@export
@annot.doc:doc
"DOC use-mnas-package"
(defun use-mnas-package ()
  (use-package (find-package :mnas-package) (find-package :cl-user)))

@export
@annot.doc:doc
"DOC unuse-mnas-package"
(defun unuse-mnas-package ()
  (unuse-package (find-package :mnas-package) (find-package :cl-user)))
