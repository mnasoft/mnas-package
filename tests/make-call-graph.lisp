;;;; ./tests/make-call-graph.lisp

(defpackage #:mnas-package-tests/functions (:use #:cl))

(in-package :mnas-package-tests/functions)

(defun foo () t)

(defun bar () (foo))

(defun baz () (bar))
