;;;; tests/package.lisp

(defpackage #:mnas-package/tests
  (:use #:cl #:fiveam)
  (:export #:run-tests))

(in-package :mnas-package/tests)

(defun run-tests () (run! 'all-tests))

(def-suite all-tests
  :description "Мастер-набор всех тестов проекта mnas-package.")

(in-suite all-tests)

(def-suite mnas-package-tests
  :description "Мастер-набор всех тестов проекта mnas-package."
  :in all-tests)

(in-suite mnas-package-tests)

(def-test mnas-package-tests/functions ()
  "Проверка размеров матрицы."
  (is-true (= 3
              (hash-table-count
               (mnas-graph:graph-nodes
                (mnas-package/make:call-graph :mnas-package-tests/functions)))))
  (is-true (= 2
              (hash-table-count
               (mnas-graph:graph-edges
                (mnas-package/make:call-graph :mnas-package-tests/functions))))))

(maphash #'(lambda (key value)
             (format t "key=~S value=~S~%" (type-of key) (type-of value)))
         (mnas-graph:graph-nodes (mnas-package/make:call-graph :mnas-package-tests/functions)))


(mnas-graph:find-node
 (mnas-package/make:call-graph :mnas-package-tests/functions)
 "bar")


(run-tests)
