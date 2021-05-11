;;;; tests/main.lisp

(in-package :mnas-package/tests)

(def-suite main
  :description "Мастер-набор всех тестов проекта mnas-package."
  :in all)

(in-suite main)

(def-test section-variables ()
  (is-true (= 6 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::section-variables :mnas-package/example :stream os)
                    (get-output-stream-string os))))))
  (is-true (= 6 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::section-variables :mnas-package/example :internal t :stream os)
                    (get-output-stream-string os))))))
  (is-true (= 6 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::section-variables :mnas-package/example :stream os :min-doc-length 10)
                    (get-output-stream-string os))))))
  (is-true (= 7 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::section-variables :mnas-package/example :internal t :stream os :min-doc-length 10)
                    (get-output-stream-string os)))))))

(def-test section-functions ()
  (is-true (= 7 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::section-functions :mnas-package/example :stream os)
                    (get-output-stream-string os))))))
  (is-true (= 7 (length
                  (mnas-string:split
                   (format nil "~%") 
                   (let ((os (make-string-output-stream )))
                     (mpkg::section-functions
                      :mnas-package/example :internal t :stream os)
                     (get-output-stream-string os))))))
  (is-true (= 8 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::section-functions
                     :mnas-package/example :stream os :min-doc-length 10)
                    (get-output-stream-string os))))))
  (is-true (= 10 (length
                  (mnas-string:split
                   (format nil "~%") 
                   (let ((os (make-string-output-stream )))
                     (mpkg::section-functions
                      :mnas-package/example :internal t :stream os :min-doc-length 10)
                     (get-output-stream-string os)))))))

(def-test section-setf-functions ()
  (is-true (= 5 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::section-setf-functions :mnas-package/example :stream os)
                    (get-output-stream-string os))))))
  (is-true (= 5 (length
                  (mnas-string:split
                   (format nil "~%") 
                   (let ((os (make-string-output-stream )))
                     (mpkg::section-setf-functions
                      :mnas-package/example :internal t :stream os)
                     (get-output-stream-string os))))))
  (is-true (= 5 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::section-setf-functions
                     :mnas-package/example :stream os :min-doc-length 10)
                    (get-output-stream-string os))))))
  (is-true (= 5 (length
                  (mnas-string:split
                   (format nil "~%") 
                   (let ((os (make-string-output-stream )))
                     (mpkg::section-setf-functions
                      :mnas-package/example :internal t :stream os :min-doc-length 10)
                     (get-output-stream-string os)))))))

(def-test section-generics/test ()
  (is-true (= 5 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::section-generics :mnas-package/example :stream os)
                    (get-output-stream-string os))))))
  (is-true (= 5 (length
                  (mnas-string:split
                   (format nil "~%") 
                   (let ((os (make-string-output-stream )))
                     (mpkg::section-generics :mnas-package/example :internal t :stream os)
                     (get-output-stream-string os))))))
  (is-true (= 5 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::section-generics :mnas-package/example :stream os :min-doc-length 10)
                    (get-output-stream-string os))))))
  (is-true (= 6 (length
                  (mnas-string:split
                   (format nil "~%") 
                   (let ((os (make-string-output-stream )))
                     (mpkg::section-generics :mnas-package/example :internal t :stream os :min-doc-length 10)
                     (get-output-stream-string os)))))))

(def-test section-methods ()
  (is-true (= 6 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::section-methods :mnas-package/example :stream os)
                    (get-output-stream-string os))))))
  (is-true (= 6 (length
                  (mnas-string:split
                   (format nil "~%") 
                   (let ((os (make-string-output-stream )))
                     (mpkg::section-methods :mnas-package/example :internal t :stream os)
                     (get-output-stream-string os))))))
  (is-true (= 6 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::section-methods :mnas-package/example :stream os :min-doc-length 10)
                    (get-output-stream-string os))))))
  (is-true (= 8 (length
                  (mnas-string:split
                   (format nil "~%") 
                   (let ((os (make-string-output-stream )))
                     (mpkg::section-methods :mnas-package/example :internal t :stream os :min-doc-length 10)
                     (get-output-stream-string os)))))))

(def-test section-classes ()
  (is-true (= 5 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::section-classes :mnas-package/example :stream os)
                    (get-output-stream-string os))))))
  (is-true (= 7 (length
                  (mnas-string:split
                   (format nil "~%") 
                   (let ((os (make-string-output-stream )))
                     (mpkg::section-classes :mnas-package/example :internal t :stream os)
                     (get-output-stream-string os))))))
  (is-true (= 5 (length
                 (mnas-string:split
                  (format nil "~%") 
                  (let ((os (make-string-output-stream )))
                    (mpkg::section-classes :mnas-package/example :stream os :min-doc-length 10)
                    (get-output-stream-string os))))))
  (is-true (= 10 (length
                  (mnas-string:split
                   (format nil "~%") 
                   (let ((os (make-string-output-stream )))
                     (mpkg::section-classes :mnas-package/example :internal t :stream os :min-doc-length 10)
                     (get-output-stream-string os)))))))
