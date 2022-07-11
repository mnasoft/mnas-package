 ; => 8 (4 bits, #x8, #o10, #b1000)
;;; 

(in-package :mnas-package/tests)

(def-suite main
  :description "Мастер-набор всех тестов проекта mnas-package."
  :in all)

(in-suite main)

(def-test section-variables ()
  (let ((pkg (find-package :mnas-package/example)))
    (is-true (= 7 (length
                   (mnas-string:split
                    (format nil "~%") 
                    (let ((os (make-string-output-stream )))
                      (mpkg/sec:section-variables pkg :stream os)
                      (get-output-stream-string os))))))
    (is-true (= 7 (length
                   (mnas-string:split
                    (format nil "~%") 
                    (let ((os (make-string-output-stream )))
                      (mpkg/sec:section-variables pkg :stream os :min-doc-length 10)
                      (get-output-stream-string os))))))
    (is-true (= 10 (length
                    (mnas-string:split
                     (format nil "~%") 
                     (let ((os (make-string-output-stream )))
                       (mpkg/sec:section-variables pkg :internal t :stream os :min-doc-length 10)
                       (get-output-stream-string os))))))))

(def-test section-functions ()
  (let ((pkg (find-package :mnas-package/example)))
    (is-true (= 7 (length
                   (mnas-string:split
                    (format nil "~%") 
                    (let ((os (make-string-output-stream )))
                      (mpkg/sec:section-functions pkg :stream os)
                      (get-output-stream-string os))))))
    (is-true (= 10 (length
                    (mnas-string:split
                     (format nil "~%") 
                     (let ((os (make-string-output-stream )))
                       (mpkg/sec:section-functions pkg :internal t :stream os)
                       (get-output-stream-string os))))))
    (is-true (= 7 (length
                   (mnas-string:split
                    (format nil "~%") 
                    (let ((os (make-string-output-stream )))
                      (mpkg/sec:section-functions pkg :stream os :min-doc-length 10)
                      (get-output-stream-string os))))))
    (is-true (= 10 (length
                    (mnas-string:split
                     (format nil "~%") 
                     (let ((os (make-string-output-stream )))
                       (mpkg/sec:section-functions
                        pkg :internal t :stream os :min-doc-length 10)
                       (get-output-stream-string os))))))))

(def-test section-setf-functions ()
  (let ((pkg (find-package :mnas-package/example)))
    (is-true (= 7 (length
                   (mnas-string:split
                    (format nil "~%") 
                    (let ((os (make-string-output-stream )))
                      (mpkg/sec:section-setf-functions pkg :stream os)
                      (get-output-stream-string os))))))
    (is-true (= 7 (length
                   (mnas-string:split
                    (format nil "~%") 
                    (let ((os (make-string-output-stream )))
                      (mpkg/sec:section-setf-functions
                       pkg :internal t :stream os)
                      (get-output-stream-string os))))))
    (is-true (= 7 (length
                   (mnas-string:split
                    (format nil "~%") 
                    (let ((os (make-string-output-stream )))
                      (mpkg/sec:section-setf-functions
                       pkg :stream os :min-doc-length 10)
                      (get-output-stream-string os))))))
    (is-true (= 7 (length
                   (mnas-string:split
                    (format nil "~%") 
                    (let ((os (make-string-output-stream )))
                      (mpkg/sec:section-setf-functions
                       pkg :internal t :stream os :min-doc-length 10)
                      (get-output-stream-string os))))))))

(def-test section-generics/test ()
  (let ((pkg (find-package :mnas-package/example)))
    (is-true (= 7 (length
                   (mnas-string:split
                    (format nil "~%") 
                    (let ((os (make-string-output-stream )))
                      (mpkg/sec::section-generics pkg :stream os)
                      (get-output-stream-string os))))))
    (is-true (= 10 (length
                    (mnas-string:split
                     (format nil "~%") 
                     (let ((os (make-string-output-stream )))
                       (mpkg/sec::section-generics pkg :internal t :stream os)
                       (get-output-stream-string os))))))
    (is-true (= 7 (length
                   (mnas-string:split
                    (format nil "~%") 
                    (let ((os (make-string-output-stream )))
                      (mpkg/sec::section-generics pkg :stream os :min-doc-length 10)
                      (get-output-stream-string os))))))
    (is-true (= 10 (length
                    (mnas-string:split
                     (format nil "~%") 
                     (let ((os (make-string-output-stream )))
                       (mpkg/sec::section-generics pkg :internal t :stream os :min-doc-length 10)
                       (get-output-stream-string os))))))))

(def-test section-methods ()
  (let ((pkg (find-package :mnas-package/example)))
    (is-true (= 8 (length
                   (mnas-string:split
                    (format nil "~%") 
                    (let ((os (make-string-output-stream )))
                      (mpkg/sec:section-methods pkg :stream os)
                      (get-output-stream-string os))))))
    (is-true (= 8 (length
                   (mnas-string:split
                    (format nil "~%") 
                    (let ((os (make-string-output-stream )))
                      (mpkg/sec:section-methods pkg :internal t :stream os)
                      (get-output-stream-string os))))))
    (is-true (= 8 (length
                   (mnas-string:split
                    (format nil "~%") 
                    (let ((os (make-string-output-stream )))
                      (mpkg/sec:section-methods pkg :stream os :min-doc-length 10)
                      (get-output-stream-string os))))))
    (is-true (= 8 (length
                   (mnas-string:split
                    (format nil "~%") 
                    (let ((os (make-string-output-stream )))
                      (mpkg/sec:section-methods pkg :internal t :stream os :min-doc-length 10)
                      (get-output-stream-string os))))))))

(def-test section-classes ()
  (let ((pkg (find-package :mnas-package/example)))
    (is-true (= 7 (length
                   (mnas-string:split
                    (format nil "~%") 
                    (let ((os (make-string-output-stream )))
                      (mpkg/sec:section-classes pkg :stream os)
                      (get-output-stream-string os))))))
    (is-true (= 10 (length
                    (mnas-string:split
                     (format nil "~%") 
                     (let ((os (make-string-output-stream )))
                       (mpkg/sec:section-classes pkg :internal t :stream os)
                       (get-output-stream-string os))))))
    (is-true (= 7 (length
                   (mnas-string:split
                    (format nil "~%") 
                    (let ((os (make-string-output-stream )))
                      (mpkg/sec:section-classes pkg :stream os :min-doc-length 10)
                      (get-output-stream-string os))))))
    (is-true (= 10 (length
                    (mnas-string:split
                     (format nil "~%") 
                     (let ((os (make-string-output-stream )))
                       (mpkg/sec:section-classes pkg :internal t :stream os :min-doc-length 10)
                       (get-output-stream-string os))))))))
