;;;; temp.lisp

(in-package #:mnas-package)

(defgeneric ->key (thing))

(defmethod ->key ((thing string))
  (intern (string-upcase thing) :keyword))

(defmethod ->key ((thing symbol))
  (if (keywordp thing)
      thing
      (intern (symbol-name thing) :keyword)))

;;;; (defmethod ->key ((thing cons)) (second thing))

;;;; (defmethod ->key ((thing string)) thing)

(defgeneric dependencies-of (system))
(defmethod dependencies-of ((system symbol))
  (mapcar #'->key (slot-value (asdf/system:find-system system) 'asdf/component:sideway-dependencies)))

(defun ordered-dep-tree (dep-tree)
  (let ((res))
    (labels ((in-res? (dep-name) (member dep-name res))
             (insert-pass (remaining)
                (loop for (dep . sub-deps) in remaining
                      for unmet-sub-deps = (remove-if #'in-res? sub-deps)
                      if (null unmet-sub-deps) do (push dep res)
                      else collect (cons dep unmet-sub-deps) into next-rems
                      finally (return next-rems))))
      (loop for (dep . callers) in dep-tree for deps-of = (dependencies-of dep)
            if (null deps-of) do (push dep res)
            else collect (cons dep deps-of) into non-zeros
            finally (loop while non-zeros
                          do (setf non-zeros (insert-pass non-zeros)))))
      (reverse res)))

(defgeneric dependency-tree (system))
(defmethod dependency-tree ((system symbol))
  (let ((res (make-hash-table)))
    (labels ((rec (sys) 
               (loop with deps = (dependencies-of sys)
                  for dep in deps for dep-k = (->key dep)
                  unless (gethash dep-k res) do (rec dep)
                  do (pushnew (->key sys) (gethash dep-k res)))))
      (rec system))
    (ordered-dep-tree (alexandria:hash-table-alist res))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-system-graph (system)
  (mnas-graph:make-graph 
   (mapcar
    #'(lambda (el)
	(mapcar #'symbol-name el))
    (reduce
     #'(lambda (x y)
	 (append x (mapcar #'(lambda (el) (list y el)) (dependencies-of y))))
     (append (list system) (dependency-tree system))
     :initial-value (make-list 0)))))

(defun package-system-graph (system)
  (mnas-graph:view-graph (make-system-graph system)))

(package-system-graph :clim)
