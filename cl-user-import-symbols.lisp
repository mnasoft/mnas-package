;;;; cl-user-import-symbols.lisp

(in-package :mnas-package)

(defun use-mnas-package ()
  "DOC use-mnas-package"
  (use-package (find-package :mnas-package) (find-package :cl-user)))

(defun unuse-mnas-package ()
  "DOC unuse-mnas-package"
  (unuse-package (find-package :mnas-package) (find-package :cl-user)))
