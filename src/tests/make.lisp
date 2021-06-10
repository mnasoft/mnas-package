;;;; ./src/tests/make.lisp
(in-package :mnas-package/tests)

(def-suite make
  :description "Мастер-набор тестов пакета mnas-package/obj"
  :in all)

(in-suite make)

(def-test make ()
  (let ((g (mnas-package/make:call-graph :mnas-package/example)))
    (is-true (eq 6 (hash-table-count (mnas-graph:<graph>-nodes g))))
    (is-true (eq 4 (hash-table-count (mnas-graph:<graph>-edges g))))
    ))
