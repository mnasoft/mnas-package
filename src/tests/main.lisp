;;;; tests/package.lisp

(defpackage #:mnas-package/tests
  (:use #:cl #:fiveam)
  (:export #:run-tests))

(in-package :mnas-package/tests)

(defun run-tests () (run! 'all-tests))

(def-suite all-tests
  :description "Мастер-набор всех тестов проекта mnas-package.")

(in-suite all-tests)

(def-suite mnas-package/tests
  :description "Мастер-набор всех тестов проекта mnas-package."
  :in all-tests)

(in-suite mnas-package/tests)

(def-test make-codex-section-variables/test ()
  (is-true (= 5 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::make-codex-section-variables :mnas-package/example :stream os)
                    (get-output-stream-string os))))))
  (is-true (= 6 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::make-codex-section-variables :mnas-package/example :internal t :stream os)
                    (get-output-stream-string os))))))
  (is-true (= 5 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::make-codex-section-variables :mnas-package/example :stream os :min-doc-length 10)
                    (get-output-stream-string os))))))
  (is-true (= 7 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::make-codex-section-variables :mnas-package/example :internal t :stream os :min-doc-length 10)
                    (get-output-stream-string os)))))))

(def-test make-codex-section-functions/test ()
  (is-true (= 5 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::make-codex-section-functions :mnas-package/example :stream os)
                    (get-output-stream-string os))))))
  (is-true (= 7 (length
                  (mnas-string:split
                   (format nil "~%") 
                   (let ((os (make-string-output-stream )))
                     (mpkg::make-codex-section-functions
                      :mnas-package/example :internal t :stream os)
                     (get-output-stream-string os))))))
  (is-true (= 6 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::make-codex-section-functions
                     :mnas-package/example :stream os :min-doc-length 10)
                    (get-output-stream-string os))))))
  (is-true (= 10 (length
                  (mnas-string:split
                   (format nil "~%") 
                   (let ((os (make-string-output-stream )))
                     (mpkg::make-codex-section-functions
                      :mnas-package/example :internal t :stream os :min-doc-length 10)
                     (get-output-stream-string os)))))))

(def-test make-codex-section-setf-functions/test ()
  (is-true (= 4 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::make-codex-section-setf-functions :mnas-package/example :stream os)
                    (get-output-stream-string os))))))
  (is-true (= 6 (length
                  (mnas-string:split
                   (format nil "~%") 
                   (let ((os (make-string-output-stream )))
                     (mpkg::make-codex-section-setf-functions
                      :mnas-package/example :internal t :stream os)
                     (get-output-stream-string os))))))
  (is-true (= 4 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::make-codex-section-setf-functions
                     :mnas-package/example :stream os :min-doc-length 10)
                    (get-output-stream-string os))))))
  (is-true (= 6 (length
                  (mnas-string:split
                   (format nil "~%") 
                   (let ((os (make-string-output-stream )))
                     (mpkg::make-codex-section-setf-functions
                      :mnas-package/example :internal t :stream os :min-doc-length 10)
                     (get-output-stream-string os)))))))

(def-test make-codex-section-generics/test ()
  (is-true (= 4 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::make-codex-section-generics :mnas-package/example :stream os)
                    (get-output-stream-string os))))))
  (is-true (= 5 (length
                  (mnas-string:split
                   (format nil "~%") 
                   (let ((os (make-string-output-stream )))
                     (mpkg::make-codex-section-generics :mnas-package/example :internal t :stream os)
                     (get-output-stream-string os))))))
  (is-true (= 4 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::make-codex-section-generics :mnas-package/example :stream os :min-doc-length 10)
                    (get-output-stream-string os))))))
  (is-true (= 6 (length
                  (mnas-string:split
                   (format nil "~%") 
                   (let ((os (make-string-output-stream )))
                     (mpkg::make-codex-section-generics :mnas-package/example :internal t :stream os :min-doc-length 10)
                     (get-output-stream-string os)))))))

(def-test make-codex-section-methods/test ()
  (is-true (= 4 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::make-codex-section-methods :mnas-package/example :stream os)
                    (get-output-stream-string os))))))
  (is-true (= 12 (length
                  (mnas-string:split
                   (format nil "~%") 
                   (let ((os (make-string-output-stream )))
                     (mpkg::make-codex-section-methods :mnas-package/example :internal t :stream os)
                     (get-output-stream-string os))))))
  (is-true (= 4 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::make-codex-section-methods :mnas-package/example :stream os :min-doc-length 10)
                    (get-output-stream-string os))))))
  (is-true (= 20 (length
                  (mnas-string:split
                   (format nil "~%") 
                   (let ((os (make-string-output-stream )))
                     (mpkg::make-codex-section-methods :mnas-package/example :internal t :stream os :min-doc-length 10)
                     (get-output-stream-string os)))))))

(def-test make-codex-section-classes/test ()
  (is-true (= 5 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::make-codex-section-classes :mnas-package/example :stream os)
                    (get-output-stream-string os))))))
  (is-true (= 7 (length
                  (mnas-string:split
                   (format nil "~%") 
                   (let ((os (make-string-output-stream )))
                     (mpkg::make-codex-section-classes :mnas-package/example :internal t :stream os)
                     (get-output-stream-string os))))))
  (is-true (= 5 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::make-codex-section-classes :mnas-package/example :stream os :min-doc-length 10)
                    (get-output-stream-string os))))))
  (is-true (= 10 (length
                  (mnas-string:split
                   (format nil "~%") 
                   (let ((os (make-string-output-stream )))
                     (mpkg::make-codex-section-classes :mnas-package/example :internal t :stream os :min-doc-length 10)
                     (get-output-stream-string os)))))))

(def-suite mnas-package/grsph/tests
  :description "Мастер-набор всех тестов проекта mnas-package."
  :in all-tests)

(in-suite mnas-package/grsph/tests)

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

(run-tests)
