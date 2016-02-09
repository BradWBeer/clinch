(in-package #:clode)


(defclass physics-spring (physics-ray)
  ((springiness :initform 1
		:initarg :springiness
		:accessor springiness)
   (damping :initform .1
	    :initarg :damping
	    :accessor damping)))
	    




(defmethod close-callback ((this physics-spring) (that physics-object))

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
	      (multiple-value-bind (start dir) (Ray-Get this)
		;;(format t "spring dir: ~A start: ~A~%" dir start)


	      (let* ((vel (body-get-linear-vel b1))
		     (len (ray-length this))
		     (x  (+ (* (- 1 (damping this)) (aref vel 1))
			    (* (springiness this) (abs (- (abs distance) len))))))
		     ;; (normal-dir (sb-cga:dot-product (apply #'clinch:make-vector
		     ;; 					    (subseq (map 'list (lambda (x)
		     ;; 								 (coerce x 'single-float))
		     ;; 							 dir) 0 3))
		     ;; 				     (apply #'clinch:make-vector
		     ;; 					    (subseq (map 'list (lambda (x)
		     ;; 								 (coerce x 'single-float))
		     ;; 							 vel) 0 3)))))

		(clode:body-set-linear-vel b1 (aref vel 0) x (aref vel 2)))))))))))


(defmethod close-callback ((this physics-object) (that physics-ray))
  (close-callback that this))
