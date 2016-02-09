
(in-package #:clode)


(defclass physics-sphere (physics-object)
  ((radius :initform 1
	   :initarg :radius
	   :reader radius)))


(defmethod initialize-instance :after ((this physics-sphere) &key total-mass density)

  (setf (slot-value this 'geometry) (create-sphere (pspace this) (radius this)))

  (when (and (not (body this))
	     (or total-mass density))
    
    (let ((m (make-instance 'physics-mass)))
      
      (cond (density    (mass-set-sphere (pointer m) density (radius this)))
	    (total-mass (mass-set-sphere-total (pointer m) total-mass (radius this)))
	    (t (mass-set-sphere (pointer m) 1 (radius this))))
      (setf (slot-value this 'body) (make-instance 'physics-body :world (world this) :mass m))))
  
  (when (body this)
    (geom-set-body (geometry this) (pointer (body this)))))
