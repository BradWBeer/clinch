
(in-package #:clode)

(defclass physics-cylinder (physics-object)
  ((radius :initform 1
	   :initarg :radius
	   :reader radius)
   (len    :initform 1
	   :initarg :length
	   :reader len)))

(defmethod initialize-instance :after ((this physics-cylinder) &key total-mass density)

  (setf (slot-value this 'geometry) (create-cylinder (pspace this) (radius this) (len this)))

  (when (and (not (body this))
	     (or total-mass density))
    
    (let ((m (make-instance 'physics-mass)))
      
      (cond (density    (mass-set-cylinder (pointer m) density 1 (radius this) (len this)))
	    (total-mass (mass-set-cylinder-total (pointer m) density 1 (radius this) (len this)))
	    (t (mass-set-cylinder (pointer m) 1 1 (radius this) (len this))))
      (setf (slot-value this 'body) (make-instance 'physics-body :world (world this) :mass m))))
  
  (when (body this)
    (geom-set-body (geometry this) (pointer (body this)))))
