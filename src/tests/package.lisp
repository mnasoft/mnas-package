;;;; tests/package.lisp

(defpackage :mnas-package/tests
  (:use #:cl #:fiveam)
  (:export #:run-tests))

(in-package :mnas-package/tests)

(defun run-tests () (run! 'all))

;;;;(run-tests)
