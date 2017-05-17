(let ((all-classes (make-hash-table)))

	(defun get-class (name)
		(let ((class-object (gethash name all-classes nil)))
				class-object))
	(defun (setf get-class) (new-value name)
		(setf (gethash name all-classes) new-value))
)



(defun !suffix-for-def-class (symbol suffix)
  (intern (concatenate 'string (string symbol) "-" (string suffix))))

(defun !create-getter-for-class (class-name slot-name)
	`(defun ,(!suffix-for-def-class class-name slot-name) (object)
		(let ((value (gethash ',slot-name object)))
			;(if ((eq ,class-name (gethash 'META-CLASS object))) value (error "This is not a object of class ~S" ',class-name))
			value)))

(defun !create-setter-for-class (class-name slot-name)
	`(defun (setf ,(!suffix-for-def-class class-name slot-name)) (new-value object)
		(setf (gethash ',slot-name object) new-value)))

(defun make-meta-object (name &key direct-slots all-slots direct-superclasses direct-subclasses cpl)
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

(defmacro def-class (class &rest slots)
	  (let* ((class-definition (if (listp class) class (list class nil)))
	  	(name (first class-definition))
	  	(super-classes (cdr class-definition)))
	   	`(progn (make-meta-object ',name :direct-superclasses ',super-classes :direct-slots ',slots)
	   			(defun ,(!suffix-for-def-class 'make name) (&key ,@slots)
			       		  (let ((class (make-hash-table)))
			       		  	,@(mapcar #'(lambda (x) `(setf (gethash ',x class) ,x)) slots)
			       		  class))
	   			,@(mapcar #'(lambda (x) (!create-getter-for-class name x)) slots)
	   			,@(mapcar #'(lambda (x) (!create-setter-for-class name x)) slots)
				)
		) )

(macroexpand '(def-class (student person) name hello))

(def-class (student person) name hello)

(defvar a)
(setf a (make-student :name "luis"))
(student-name a)
(setf (student-name a) "catarina")
(student-name a)