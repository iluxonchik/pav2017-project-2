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

(defun class-direct-slots (class)
	(let* ((class-object (get-class-with-error class))
			(direct-slots (gethash 'direct-slots class-object)))
		direct-slots))

(defun class-all-slots (class)
	(let* ((class-object (get-class-with-error class))
			(all-slots (gethash 'all-slots class-object)))
		all-slots))

(defun canonize-class-slots (direct-slots class-precedance-list)
	direct-slots)

(defun canonize-direct-superclasses (super-classes)
	(if (null (first super-classes))
		super-classes
		(progn (mapcar #'(lambda (class-symbol) (get-class-with-error class-symbol)) super-classes)
			super-classes)))

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

(defmacro def-class (class &rest slots)
	  (let* ((class-definition (if (listp class) class (list class nil)))
	  	(name (first class-definition))
	  	(super-classes (cdr class-definition)))
	   	`(progn (make-class-def ',name 
	   				:direct-superclasses ',(canonize-direct-superclasses super-classes)
	   				:direct-slots ',slots
	   				:all-slots ',(canonize-class-slots slots super-classes))
	   			(defun ,(!suffix-for-def-class 'make name) (&key ,@slots)
			       		  (let ((class (make-hash-table)))
			       		  	,@(mapcar #'(lambda (x) `(setf (gethash ',x class) ,x)) slots)
			       		  class))
	   			,@(mapcar #'(lambda (x) (!create-getter-for-class name x)) slots)
	   			,@(mapcar #'(lambda (x) (!create-setter-for-class name x)) slots)
				)
		) )

;;Some tests
(def-class person name)
(macroexpand '(def-class (student person) name hello))

(def-class (student person) id)

(defvar a)
(setf a (make-student :id "ist177900"))
(student-id a)
(setf (student-id a) "ist177966")
(student-id a)