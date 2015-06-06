
(in-package #:clode)

(defclass physics-ray (physics-object)
  ())

(defmethod initialize-instance :after ((this physics-ray) &key length total-mass density )

  (unless length
    (error "Rays require a length!"))

  (setf (slot-value this 'geometry) (ode:create-ray (pspace this) length))
  
  (when (body this)
    (clode:geom-set-body (geometry this) (pointer (body this)))))

(defmethod ray-length ((this physics-ray) &key)
  (geom-ray-get-length (geometry this)))

(defmethod (setf transform)  ((length number) (this physics-ray))
  (geom-ray-set-length (geometry this) length))

(defmethod close-callback ((this physics-ray) (that physics-object))

  (let* ((o1 (geometry this))
  	 (o2 (geometry that))
  	 (b1 (geom-get-body o1))
  	 (b2 (geom-get-body o2)))
    
    (with-foreign-object (contact '(:struct dContact) *physics-max-contacts*)
      (let ((gg (foreign-slot-pointer contact '(:struct ode::dContact) 'ode::geom)))
	
	
	(let ((num-contacts (collide o1 o2 *physics-max-contacts* gg (foreign-type-size '(:struct ode::dContact)))))
	  (unless (zerop num-contacts)
	    
	    (let ((distance (abs (foreign-slot-value gg '(:struct ode::dContactGeom) 'ode::depth))))

	      ;;(format t "CLODE:ray-callback b1 = ~A~%" distance)

	      (let* ((vel (body-get-linear-vel b1))
		     (len (ray-length this))
		     (x  (+ (* .90 (aref vel 1))
			    (* 1/2 (abs (- (abs distance) len))))))
		;; (+ (* 
		;; 	(abs (* 1/25 (max 0 (- distance 1.5))))))))
		
		;;(format t "~A vs ~A~%" vel x)
		
		(clode:body-set-linear-vel b1 (aref vel 0) x (aref vel 2))))))))))


(defmethod close-callback ((this physics-object) (that physics-ray))
  (close-callback that this))

(defmethod Ray-Get ((this physics-ray))

  (cffi:with-foreign-objects ((start 'dVector3)
			      (dir   'dVector3))

    (Geom-Ray-Get (geometry this) start dir)
    (values (cffi:convert-from-foreign start 'dVector3)
	    (cffi:convert-from-foreign dir 'dVector3))))

