;;;; temp.lisp

(in-package :mnas-package)

(defun class-undirect-subclasses (class-01)
"@b(Описание:) функция @b(class-undirect-subclasses)
 выполняет поиск всех подклассов класса class-01 и 
 возвращает список всех найденных классов.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (progn
  (require :dxf)
  (class-undirect-subclasses (find-class 'dxf::object))
  (class-undirect-subclasses (find-class 'number)))
@end(code)
"
  (let ((rez-classes nil)
	(l-not-obr (list class-01)))
    (flet
	((bar (class)
	   (setf l-not-obr (append l-not-obr (closer-mop:class-direct-subclasses class)))))
      (do ((class nil))
	  ((null l-not-obr) rez-classes)
	(setf class (pop l-not-obr))
	(push class rez-classes)
	(bar class)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remove-msys-prefix (path)
  (let ((msystem-prefix (uiop:getenv "MSYSTEM_PREFIX")
                        ))
    (if msystem-prefix
        (mk-pathname
         (nthcdr   
          (1- (length (mnas-string:split "/" msystem-prefix)))
          (mnas-string:split "/" path)))
        path)))


(let ((msystem-prefix "D:/home/namatv/PRG/mingw64/mingw64/"
              
                      ))
  (if msystem-prefix
      (mk-pathname
       (nthcdr   
        (1- (length (mnas-string:split "/" msystem-prefix)))
        (mnas-string:split "/" path)))
      path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *system->packages* (make-hash-table :test 'equal))

(defun track-system-packages (system thunk)
  "Track packages created while loading SYSTEM."
  (let ((before (copy-list (list-all-packages))))
    (funcall thunk)
    (let ((after (list-all-packages)))
      (setf (gethash (asdf:component-name system) *system->packages*)
            (set-difference after before)))))

(defun packages-by-subsystem (system-name)
  "Return an alist (subsystem-name . packages) for SYSTEM-NAME."
  (clrhash *system->packages*)
  (let ((system (asdf:find-system system-name)))
    (labels ((wrap (component thunk)
               (track-system-packages component thunk)))
      (let ((asdf:*around-compile-hook* #'wrap))
        (asdf:load-system system)))
    ;; Преобразуем хеш-таблицу в alist
    (loop for k being the hash-keys of *system->packages*
          using (hash-value v)
          collect (cons k v))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *system->packages* (make-hash-table :test 'equal))

(defmethod asdf:perform :around ((op asdf:load-op) (c asdf:cl-source-file))
  (let* ((system (asdf:component-system c))
         (sys-name (asdf:component-name system))
         (before (copy-list (list-all-packages))))
    (prog1
        (call-next-method)
      (let ((after (list-all-packages)))
        (setf (gethash sys-name *system->packages*)
              (nconc (gethash sys-name *system->packages*)
                     (set-difference after before)))))))

(defun packages-by-system (system-name)
  (clrhash *system->packages*)
  (asdf:load-system system-name)
  ;; Преобразуем в alist
  (loop for k being the hash-keys of *system->packages*
          using (hash-value v)
        collect (cons k (remove-duplicates v))))

(packages-by-system :math)

(list-all-packages)

(ql:quickload "asdf-viz")

(asdf-viz:visualize-asdf-hierarchy "123.png")
