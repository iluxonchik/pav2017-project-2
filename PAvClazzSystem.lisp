;; TABLE TO HOLD ALL META CLASS OBJECTS
(let ((all-classes (make-hash-table)))

    (defun get-class (name)
        (let ((class-object (gethash name all-classes nil)))
                class-object))
    (defun (setf get-class) (new-value name)
        (setf (gethash name all-classes) new-value))
    (defun get-class-with-error (name)
        (let ((class-object (gethash name all-classes nil)))
                (if (null class-object)
                    (error "Class ~S does not exist" name)
                    class-object)))
)
;; META CLASS DEFINITION

;;; GETTERS
(defun class-direct-slots (class)
    (let* ((class-object (get-class-with-error class))
            (direct-slots (gethash 'direct-slots class-object)))
        direct-slots))

(defun class-all-slots (class)
    (let* ((class-object (get-class-with-error class))
            (all-slots (gethash 'all-slots class-object)))
        all-slots))

(defun class-direct-superclasses (class)
    (let* ((class-object (get-class-with-error class))
            (direct-superclasses (gethash 'direct-superclasses class-object)))
            direct-superclasses))

(defun class-direct-subclasses (class)
    (let* ((class-object (get-class-with-error class))
            (direct-subclasses (gethash 'direct-subclasses class-object)))
        direct-subclasses))

(defun class-cpl (class)
    (let* ((class-object (get-class-with-error class))
            (cpl (gethash 'cpl class-object)))
        cpl))

;;; FINISH GETTERS

;;; SETTERS
(defun (setf class-direct-subclasses) (new-value class)
    (let ((class-object (get-class-with-error class)))
        (setf (gethash 'direct-subclasses class-object) new-value)))

(defun add-direct-subclass (class new-direct-subclass)
    (let ((current-direct-subclasses (class-direct-subclasses class)))
        (setf (class-direct-subclasses class) (nconc current-direct-subclasses (list new-direct-subclass)))))
;;; FINISH SETTERS

(defun make-class-def (name &key direct-slots all-slots direct-superclasses direct-subclasses cpl)
    (if (get-class name)
        (error "Class with name ~S already exists" name)
       (let ((class (make-hash-table)))
         (setf (gethash 'name class) name)
         (setf (gethash 'direct-slots class) direct-slots)
         (setf (gethash 'all-slots class) all-slots)
         (setf (gethash 'direct-superclasses class) direct-superclasses)
         (setf (gethash 'direct-subclasses class) direct-subclasses)
         (setf (gethash 'cpl class) cpl)
         (setf (get-class name) class))))

(make-class-def 'standard-pav-class)

;; FINISH META CLASS DEFINITION

;; (MACRO DEF-CLASS) HELPER FUNCTIONS

(defun canonize-class-slots (direct-slots class-precedance-list)
    direct-slots)

(defun canonize-direct-superclasses (super-classes)
    (if (null (first super-classes))
        super-classes
        (progn (mapcar #'(lambda (class-symbol) (get-class-with-error class-symbol)) super-classes)
            super-classes)))

(defun class-precedance-list (class direct-superclasses)
    ;empty method right now waiting for ILLYA
    direct-superclasses)

(defun !suffix-for-def-class (symbol suffix)
  (intern (concatenate 'string (string symbol) "-" (string suffix))))

(defun !create-getter-for-class (class-name slot-name)
    `(defun ,(!suffix-for-def-class class-name slot-name) (object)
        (let ((value (gethash ',slot-name object)))
            ;need to check is object is of the class which this getter is made for
            value)))

(defun !create-setter-for-class (class-name slot-name)
    `(defun (setf ,(!suffix-for-def-class class-name slot-name)) (new-value object)
        (setf (gethash ',slot-name object) new-value)))

;; FINISH (MACRO DEF-CLASS) HELPER FUNCTIONS

(defmacro def-class (class &rest slots)
      (let* ((class-definition (if (listp class) class (list class 'standard-pav-class)))
        (name (first class-definition))
        (super-classes (cdr class-definition)))
        `(progn (make-class-def ',name
                    :direct-superclasses ',(canonize-direct-superclasses super-classes)
                    :direct-slots ',slots
                    :all-slots ',(canonize-class-slots slots super-classes)
                    :cpl ',(class-precedance-list name super-classes))
                ;(mapcar #'(lambda (x) (add-direct-subclass x ',name)) ',super-classes)
                (dolist (class ',super-classes) (add-direct-subclass class  ',name))
                (defun ,(!suffix-for-def-class 'make name) (&key ,@slots)
                          (let ((class (make-hash-table)))
                            ,@(mapcar #'(lambda (x) `(setf (gethash ',x class) ,x)) slots)
                          class))
                ,@(mapcar #'(lambda (x) (!create-getter-for-class name x)) slots)
                ,@(mapcar #'(lambda (x) (!create-setter-for-class name x)) slots)
                )
        ) )


;;;FINISH PAva CLASS SYSTEM

(defun list-empty (lst)
    (eql (length lst) 0)
)

(defun class-precedance-list (cls direct-superclasses)
   (let* (
          (cpl_res (list cls))
          (stack direct-superclasses)
          (curr_node nil)
          (curr_node_cpl nil)
         )
         (loop while (not (list-empty stack))
              do (setf curr_node (car stack))
                 (setf cpl_res (append cpl_res (list curr_node)))
                 (setf stack (cdr stack))

                 (if curr_node
                   (progn (setf curr_node_cpl (class-direct-superclasses curr_node))
                   (setf stack (append curr_node_cpl stack))
                  )
                 )
         )
    (setf cpl_res (remove-duplicates cpl_res))
    (print cpl_res)
     cpl_res
     )
)

(defun cpl (cls ds)
  (class-precedance-list cls ds))


;;Some tests
(def-class person name)
(macroexpand '(def-class (student person) name hello))

(def-class (student person) id)

(defvar a)
(setf a (make-student :id "ist177900"))
(student-id a)
(setf (student-id a) "ist177966")
(student-id a)

(def-class h)
(def-class f)
(def-class (a h f))
(def-class (b a))
(def-class (c a))
(def-class (d b c))
