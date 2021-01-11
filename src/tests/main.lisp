;;;; tests/main.lisp

(in-package :mnas-package/tests)

(def-suite main
  :description "Мастер-набор всех тестов проекта mnas-package."
  :in all)

(in-suite main)

(def-test make-codex-section-variables ()
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

(def-test make-codex-section-functions ()
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

(def-test make-codex-section-setf-functions ()
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

(def-test make-codex-section-methods ()
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

(def-test make-codex-section-classes ()
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

#|
(def-suite mnas-package/graph
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
|#
