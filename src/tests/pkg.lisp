(in-package :mnas-package/tests)

(def-suite pkg
  :description "Мастер-набор тестов пакета mnas-package/obj"
  :in all)

(in-suite pkg)

(def-test package-by-type-length ()
  (let ((pkg (find-package :mnas-package/example)))
    (is-true (eq  6 (length (mnas-package/pkg:package-variables pkg :internal t))))
    (is-true (eq  3 (length (mnas-package/pkg:package-variables pkg ))))
    
    (is-true (eq  6 (length (mnas-package/pkg:package-functions pkg :internal t))))
    (is-true (eq  3 (length (mnas-package/pkg:package-functions pkg))))
    
    (is-true (eq  6 (length (mnas-package/pkg:package-macroses pkg :internal t))))
    (is-true (eq  3 (length (mnas-package/pkg:package-macroses pkg))))
    
    (is-true (eq  3 (length (mnas-package/pkg:package-setf-functions pkg :internal t))))
    (is-true (eq  3 (length (mnas-package/pkg:package-setf-functions pkg))))
    
    (is-true (eq  11 (length (mnas-package/pkg:package-generics :mnas-package/example :internal t))))
    (is-true (eq  3 (length (mnas-package/pkg:package-generics pkg))))
    
    (is-true (eq  6 (length (mnas-package/pkg:package-setf-generics pkg :internal t))))
    (is-true (eq  3 (length (mnas-package/pkg:package-setf-generics pkg))))
    
    (is-true (eq  4 (length (mnas-package/pkg:package-methods pkg :internal t))))
    (is-true (eq  4 (length (mnas-package/pkg:package-methods pkg))))

    (is-true (eq  6 (length (mnas-package/pkg:package-classes pkg :internal t))))
    (is-true (eq  3 (length (mnas-package/pkg:package-classes pkg))))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-test package-variables ()
  "(mnas-package/pkg:package-variables :mnas-package/example :internal t) "
  (let ((pkg (find-package :mnas-package/example))
        (vars '(MNAS-PACKAGE/EXAMPLE:*V-A-EXP*
                MNAS-PACKAGE/EXAMPLE:*V-C-EXP*
                MNAS-PACKAGE/EXAMPLE:*V-B-EXP*
                MNAS-PACKAGE/EXAMPLE::*V-A-INT*
                MNAS-PACKAGE/EXAMPLE::*V-C-INT*
                MNAS-PACKAGE/EXAMPLE::*V-B-INT*)))
        (loop :for i :in (mnas-package/pkg:package-variables pkg :internal t)
          :do
             (is-true (find i vars :test #'equal)))))

(def-test package-functions ()
  "(mnas-package/pkg:package-functions :mnas-package/example :internal t)
 "
  (let ((pkg (find-package :mnas-package/example))
        (funcs (list #'MNAS-PACKAGE/EXAMPLE:F-A-EXP
                     #'MNAS-PACKAGE/EXAMPLE:F-B-EXP
                     #'MNAS-PACKAGE/EXAMPLE:F-C-EXP
                     #'MNAS-PACKAGE/EXAMPLE::F-C-INT
                     #'MNAS-PACKAGE/EXAMPLE::F-B-INT
                     #'MNAS-PACKAGE/EXAMPLE::F-A-INT)))
    (loop :for i :in  (mnas-package/pkg:package-functions pkg :internal t)
          :do
             (is-true (find i funcs :test #'equal)))))

(def-test package-macroses ()
  "(mnas-package/pkg:package-macroses :mnas-package/example :internal t)
 "
  (let ((pkg (find-package :mnas-package/example))
        (maks (list (MACRO-FUNCTION 'MNAS-PACKAGE/EXAMPLE:K-B-EXP)
                    (MACRO-FUNCTION 'MNAS-PACKAGE/EXAMPLE:K-C-EXP)
                    (MACRO-FUNCTION 'MNAS-PACKAGE/EXAMPLE:K-A-EXP)
                    (MACRO-FUNCTION 'MNAS-PACKAGE/EXAMPLE::K-A-INT)
                    (MACRO-FUNCTION 'MNAS-PACKAGE/EXAMPLE::K-B-INT)
                    (MACRO-FUNCTION 'MNAS-PACKAGE/EXAMPLE::K-C-INT))))
    (loop :for i :in  (mnas-package/pkg:package-macroses pkg :internal t)
          :do
             (is-true (find i maks :test #'equal)))))

(def-test package-setf-functions ()
  "(mnas-package/pkg:package-setf-functions :mnas-package/example :internal t)
 "
  (let ((pkg (find-package :mnas-package/example))
        (f-setf (list
                 #'(SETF MNAS-PACKAGE/EXAMPLE:F-B-EXP)
                 #'(SETF MNAS-PACKAGE/EXAMPLE:F-A-EXP)
                 #'(SETF MNAS-PACKAGE/EXAMPLE:F-C-EXP))))
        (loop :for i :in  (mnas-package/pkg:package-setf-functions pkg :internal t)
          :do
             (is-true (find i f-setf :test #'equal)))))

(def-test package-generics ()
  "(mnas-package/pkg:package-generics :mnas-package/example :internal t)
 "
  (let ((pkg (find-package :mnas-package/example))
        (gens (list #'MNAS-PACKAGE/EXAMPLE:M-C-EXP
                      #'MNAS-PACKAGE/EXAMPLE:M-B-EXP
                      #'MNAS-PACKAGE/EXAMPLE:M-A-EXP
                      #'MNAS-PACKAGE/EXAMPLE::M-FOO
                      #'MNAS-PACKAGE/EXAMPLE::<C-C-INT>-C
                      #'MNAS-PACKAGE/EXAMPLE::M-B-INT
                      #'MNAS-PACKAGE/EXAMPLE::M-A-INT
                      #'MNAS-PACKAGE/EXAMPLE::M-FOO-SHORT
                      #'MNAS-PACKAGE/EXAMPLE::<C-B-INT>-B
                      #'MNAS-PACKAGE/EXAMPLE::M-C-INT
                      #'MNAS-PACKAGE/EXAMPLE::<C-A-INT>-A)))
        (loop :for i :in (mnas-package/pkg:package-generics pkg :internal t)
          :do
             (is-true (find i gens :test #'equal)))))

(def-test package-setf-generics ()
  "(mnas-package/pkg:package-setf-generics :mnas-package/example :internal t)
 "
  (let ((pkg (find-package :mnas-package/example))
        (g-setf (list #'(SETF MNAS-PACKAGE/EXAMPLE:M-C-EXP)
                    #'(SETF MNAS-PACKAGE/EXAMPLE:M-B-EXP)
                    #'(SETF MNAS-PACKAGE/EXAMPLE:M-A-EXP)
                    #'(SETF MNAS-PACKAGE/EXAMPLE::<C-C-INT>-C)
                    #'(SETF MNAS-PACKAGE/EXAMPLE::<C-B-INT>-B)
                    #'(SETF MNAS-PACKAGE/EXAMPLE::<C-A-INT>-A))))
       
        (loop :for i :in (mnas-package/pkg:package-setf-generics pkg :internal t)
          :do
             (is-true (find i g-setf :test #'equal)))))


(def-test package-methods ()
  "(mnas-package/pkg:package-methods :mnas-package/example :internal t)
 "
  (let ((pkg (find-package :mnas-package/example))
        (mets (list
               (find-method #'MNAS-PACKAGE/EXAMPLE:M-A-EXP '()
                            (mapcar #'find-class '(MNAS-PACKAGE/EXAMPLE::<C-A-INT>
                                                   MNAS-PACKAGE/EXAMPLE::<C-B-INT>
                                                   MNAS-PACKAGE/EXAMPLE:<C-C-EXP>)))
               (find-method #'MNAS-PACKAGE/EXAMPLE:M-B-EXP '()
                            (mapcar #'find-class '(MNAS-PACKAGE/EXAMPLE::<C-A-INT>
                                                   MNAS-PACKAGE/EXAMPLE::<C-B-INT>
                                                   MNAS-PACKAGE/EXAMPLE:<C-C-EXP>)))
               (find-method #'MNAS-PACKAGE/EXAMPLE:M-C-EXP '()
                            (mapcar #'find-class '(MNAS-PACKAGE/EXAMPLE::<C-A-INT>
                                                   MNAS-PACKAGE/EXAMPLE::<C-B-INT>
                                                   MNAS-PACKAGE/EXAMPLE::<C-C-INT>)))
               (find-method #'MNAS-PACKAGE/EXAMPLE:M-C-EXP '()
                            (mapcar #'find-class '(MNAS-PACKAGE/EXAMPLE:<C-A-EXP>
                                                   MNAS-PACKAGE/EXAMPLE:<C-B-EXP>
                                                   MNAS-PACKAGE/EXAMPLE:<C-C-EXP>))))))
       
        (loop :for i :in (mnas-package/pkg:package-methods pkg :internal t)
          :do
             (is-true (find i mets :test #'equal)))))

(def-test package-classes ()
  "(mnas-package/pkg:package-classes :mnas-package/example :internal t)
 "
  (let ((pkg (find-package :mnas-package/example))
        (mets (list
               (find-class 'MNAS-PACKAGE/EXAMPLE:<C-A-EXP>)
               (find-class 'MNAS-PACKAGE/EXAMPLE:<C-C-EXP>)
               (find-class 'MNAS-PACKAGE/EXAMPLE:<C-B-EXP>)
               (find-class 'MNAS-PACKAGE/EXAMPLE::<C-C-INT>)
               (find-class 'MNAS-PACKAGE/EXAMPLE::<C-A-INT>)
               (find-class 'MNAS-PACKAGE/EXAMPLE::<C-B-INT>))))
       
    (loop :for i :in (mnas-package/pkg:package-classes pkg :internal t)
          :do
             (is-true (find i mets :test #'equal)))))

"
 Running test suite PKG
  Running test PACKAGE-BY-TYPE-LENGTH ........f.......
  Running test PACKAGE-VARIABLES ......
  Running test PACKAGE-FUNCTIONS ......
  Running test PACKAGE-MACROSES ......
  Running test PACKAGE-SETF-FUNCTIONS ...
  Running test PACKAGE-GENERICS X
  Running test PACKAGE-SETF-GENERICS ......
  Running test PACKAGE-METHODS ....
  Running test PACKAGE-CLASSES ......
 Running test MAKE .....
 Running test suite MAIN
  Running test SECTION-VARIABLES ...
  Running test SECTION-FUNCTIONS ....
  Running test SECTION-SETF-FUNCTIONS ....
  Running test SECTION-GENERICS/TEST ....
  Running test SECTION-METHODS .....
  Running test SECTION-CLASSES ....
  Running test INSERT .
 Did 96 checks.
    Pass: 94 (97%)
    Skip: 0 ( 0%)
    Fail: 2 ( 2%)

 Failure Details:
 --------------------------------
 PACKAGE-GENERICS in PKG [(mnas-package/pkg:package-generics :mnas-package/example :internal t)
 ]: 
      Unexpected Error: #<UNDEFINED-FUNCTION M-FOO-SHORT {1004AEC4D3}>
The function MNAS-PACKAGE/EXAMPLE::M-FOO-SHORT is undefined..
 --------------------------------
 --------------------------------
 PACKAGE-BY-TYPE-LENGTH in PKG []: 
      (EQ 11 (LENGTH (MNAS-PACKAGE/PKG:PACKAGE-GENERICS PKG :INTERNAL T))) did not return a true value
 --------------------------------
"
