;;;; cl-user-import-symbols.lisp

(in-package :mnas-package)

(export 'use-mnas-package)
(defun use-mnas-package ()
"@b(Описание:) use-mnas-package копирует внешние символы пакета
:mnas-package в пространство имен пакета :cl-user."
  (use-package (find-package :mnas-package) (find-package :cl-user)))

(export 'unuse-mnas-package)

(defun unuse-mnas-package ()
"@b(Описание:) unuse-mnas-package удаляет внешние символы пакета
:mnas-package из пространства имен пакета :cl-user."
  (unuse-package (find-package :mnas-package) (find-package :cl-user)))
