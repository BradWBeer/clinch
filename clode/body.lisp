
(in-package #:clode)

(defclass physics-body (refcount)
  ((pointer :initform nil
	    :initarg :pointer
	    :reader pointer)
   (mass :initform nil
	 :initarg :mass
	 :reader mass)))

(defmethod initialize-instance :after ((this physics-body) &key world)
  
  (if world 
      (unless (slot-value this 'pointer)
	(setf (slot-value this 'pointer) (body-create world))

	(when (mass this)
	  (body-set-mass (pointer this) (pointer (mass this)))
	  (ref (mass this))))
      
      (error "a physics-body requires a world!")))


(defmethod unload ((this physics-body) &key)
  (when (mass this)
    (unref (mass this)))

  (when (pointer this)
    (body-destroy (pointer this))
    (setf (slot-value this 'pointer) nil)))



(defmethod set-transform ((body physics-body) (matrix SIMPLE-ARRAY))
  (with-foreign-object (rot 'dReal 12)
    (loop for i from 0 to 11
       do (setf (mem-aref rot 'dreal i) 0))

    (loop
       for pos in '(0 4 8 1 5 9 2 6 10)
       for i from 0 to 11
	 
       do (setf (mem-aref rot 'dreal i) (elt matrix pos)))

    (body-set-rotation (pointer body) rot))
  
  (with-foreign-object (pos 'dreal 4)
    (loop for i from 0 to 3 do (setf (mem-aref pos 'dreal i) 0))

    (body-set-position (pointer body)
		       (elt matrix 3)
		       (elt matrix 7)
		       (elt matrix 11))))
