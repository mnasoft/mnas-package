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
