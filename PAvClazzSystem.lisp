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
(defvar **reserved-slots** '(standard-pav-class meta-object-name))
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

(defun canonize-class-slots (direct-slots class-precedence-list)
    (labels ((illegal-slot-name (slot reason)
                              (error "The slot ~a cannot be defined. The slot ~a" slot reason))
           (check-slot (slot evaluated-slots)
                        (cond ((not (symbolp slot))(illegal-slot-name slot "is not a symbol."))
                          ((keywordp slot)(illegal-slot-name slot "is a keyword."))
                          ((member slot **reserved-slots**)(illegal-slot-name slot "is a reserved keyword."))
                          ((member slot evaluated-slots)(illegal-slot-name slot (format nil "is already defined in a CLASS.")))
             )))
          (let ((all-slots (apply #'append (loop for class in (cdr class-precedence-list)
                                                                     collect (class-direct-slots class)))))
            (loop for slot in direct-slots
                do (progn (check-slot slot all-slots)
                          (setf all-slots (append all-slots (list slot)))))
            all-slots)))



(defun canonize-direct-superclasses (super-classes)
    (if (null (first super-classes))
        super-classes
        (progn (mapcar #'(lambda (class-symbol) (get-class-with-error class-symbol)) super-classes)
            super-classes)))

(defun list-empty (lst)
    (eql (length lst) 0)
)

(defun class-precedence-list (cls direct-superclasses)
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
     cpl_res
     )
)

(defun !suffix-for-def-class (list-of-symbols)
  (intern (format nil "~{~a~}" list-of-symbols)))

(defun !create-getter-for-class (class-name slot-name)
    `(defun ,(!suffix-for-def-class (list class-name '- slot-name)) (object)
        (let ((value (gethash ',slot-name object)))
            ;need to check is object is of the class which this getter is made for
            value)))

(defun !create-setter-for-class (class-name slot-name)
    `(defun (setf ,(!suffix-for-def-class (list class-name '- slot-name))) (new-value object)
        (setf (gethash ',slot-name object) new-value)))

;; FINISH (MACRO DEF-CLASS) HELPER FUNCTIONS

(defmacro def-class (class &rest slots)
      (let* ((class-definition (if (listp class) class (list class 'standard-pav-class)))
        (name (first class-definition))
        (super-classes (cdr class-definition))
        (cpl (class-precedence-list name super-classes))
        (all-slots (canonize-class-slots slots cpl)))
        `(progn (make-class-def ',name
                    :direct-superclasses ',(canonize-direct-superclasses super-classes)
                    :direct-slots ',slots
                    :all-slots ',all-slots
                    :cpl ',cpl)
                (mapc #'(lambda (x) (add-direct-subclass x ',name)) ',super-classes)
                (defun ,(!suffix-for-def-class (list 'make '- name)) (&key ,@all-slots)
                          (let ((class (make-hash-table)))
                            (setf (gethash 'meta-object-name class) ',name)
                            ,@(mapcar #'(lambda (x) `(setf (gethash ',x class) ,x)) all-slots)
                          class))
                (defun ,(!suffix-for-def-class (list name '?)) (object)
                    (let ((meta-object (gethash 'meta-object-name object)))
                  (not (null (member ',name (class-cpl meta-object))))))
                ,@(mapcar #'(lambda (x) (!create-getter-for-class name x)) all-slots)
                ,@(mapcar #'(lambda (x) (!create-setter-for-class name x)) all-slots)
                (format nil "<DEF-CLASS ~{~a ~}>" '(The class ,name with slots (,@slots) and superclasses (,@super-classes) has been defined.)))
        ) )


;;;FINISH PAva CLASS SYSTEM


;;Some tests
; (def-class person name)
; (def-class (student person) id)
;
; (defvar a)
; (setf a (make-student :id "ist177900"))
; (student-id a)
; (setf (student-id a) "ist177966")
; (student-id a)
;
; (def-class h)
; (def-class f)
; (def-class (a h f))
; (def-class (b a))
; (def-class (c a))
; (def-class (d b c))
