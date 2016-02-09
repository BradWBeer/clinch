
(in-package #:clode)
 
(defclass physics-box (physics-object)
  ((x :initform 1
      :initarg :x
      :reader x)
   (y :initform 1
      :initarg :y
      :reader y)
   (z :initform 1
      :initarg :z
      :reader z)))


(defmethod initialize-instance :after ((this physics-box) &key total-mass density)

  (setf (slot-value this 'geometry) (ode:create-box (pspace this) (x this) (y this) (z this)))

  (when (and (not (body this))
	     (or total-mass density))
    
    (let ((m (make-instance 'physics-mass)))
      
      (cond (density    (clode:mass-set-box (pointer m) density (x this) (y this) (z this)))
	    (total-mass (clode:mass-set-box-total (pointer m) total-mass (x this) (y this) (z this)))
	    (t (clode:mass-set-box (pointer m) 1 (x this) (y this) (z this))))
      (setf (slot-value this 'body) (make-instance 'physics-body :world (world this) :mass m))))
  
  (when (body this)
    (clode:geom-set-body (geometry this) (pointer (body this)))))
