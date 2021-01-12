(in-package :mnas-package/tests)

(def-suite pkg
  :description "Мастер-набор тестов пакета mnas-package/obj"
  :in all)

(in-suite pkg)

(def-test package-by-type-length ()
  (let ((pkg (find-package :mnas-package/example)))
    (is-true (eq  3 (length (mnas-package/pkg:package-variables pkg :internal t))))
    (is-true (eq  1 (length (mnas-package/pkg:package-variables :mnas-package/example ))))
    
    (is-true (eq  6 (length (mnas-package/pkg:package-functions pkg :internal t))))
    (is-true (eq  2 (length (mnas-package/pkg:package-functions :mnas-package/example))))
    
    (is-true (eq  2 (length (mnas-package/pkg:package-macroses pkg :internal t))))
    (is-true (eq  1 (length (mnas-package/pkg:package-macroses :mnas-package/example))))
    
    (is-true (eq  2 (length (mnas-package/pkg:package-setf-functions pkg :internal t))))
    (is-true (eq  0 (length (mnas-package/pkg:package-setf-functions :mnas-package/example))))
    
    (is-true (eq  2 (length (mnas-package/pkg:package-generics pkg :internal t))))
    (is-true (eq  1 (length (mnas-package/pkg:package-generics :mnas-package/example))))
    
    (is-true (eq 16 (length (mnas-package/pkg:package-methods pkg :internal t))))
    (is-true (eq  8 (length (mnas-package/pkg:package-methods :mnas-package/example))))

    (is-true (eq  6 (length (mnas-package/pkg:package-classes :mnas-package/example :internal t))))
    (is-true (eq  1 (length (mnas-package/pkg:package-classes :mnas-package/example))))
    ))

