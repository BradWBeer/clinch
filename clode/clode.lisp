;;;; clode.lisp

(in-package #:clode)

;;; "clode" goes here. Hacks and glory await!



(defvar *physics-world*)
(defvar *physics-space*)
(defvar *physics-contact-group*)
(defvar *physics-max-contacts* 25)  
(defvar *physics-geometry-hash*)

(defun n->sf (x)
  (coerce x 'single-float))

;; (defmethod get-transform (position rotation)
;;   (sb-cga:matrix (n->sf (elt rotation 0)) (n->sf (elt rotation 1)) (n->sf (elt rotation 2))  (n->sf (elt position 0)) 
;; 		 (n->sf (elt rotation 4)) (n->sf (elt rotation 5)) (n->sf (elt rotation 6))  (n->sf (elt position 1))
;; 		 (n->sf (elt rotation 8)) (n->sf (elt rotation 9)) (n->sf (elt rotation 10)) (n->sf (elt position 2))
;; 		 (n->sf 0)                (n->sf 0)                (n->sf 0)                 (n->sf 1)))

(defun coerce-floats (val)
  (sb-cga:vec (coerce (aref val 0) 'single-float)
	      (coerce (aref val 1) 'single-float)
	      (coerce (aref val 2) 'single-float)))

(setf mode-options '(ode::Mu2	  
		     ode::Axis-Dep 
		     ode::FDir1	  
		     ode::Bounce  
		     ode::Soft-ERP 
		     ode::Soft-CFM 
		     ode::Motion1 
		     ode::Motion2 
		     ode::MotionN 
		     ode::Slip1	  
		     ode::Slip2	  
		     ode::Rolling 
		     ode::Approx0   
		     ode::Approx1-1 
		     ode::Approx1-2 
		     ode::Approx1-N 
		     ode::Approx1))

(defclass physics-mass (refcount)
  ((pointer :initform (foreign-alloc 'dmass)
	    :initarg :pointer
	    :reader pointer)))

(defmethod unload ((this physics-mass) &key)
  (foreign-free (pointer this))
  (setf (slot-value this 'pointer) nil))


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


(defclass physics-object (refcount)
  ((body :initform nil
	 :initarg :body
	 :reader body)
   (geometry :initform nil
	     :initarg :geometry
	     :reader geometry)
   (world :initform *physics-world*
	  :initarg :world
	  :reader world)
   (space :initform *physics-space*
	  :initarg :space
	  :reader pspace)
   (surface-mode :initform '(:bounce :soft-CFM)
		 :initarg :mode
		 :accessor surface-mode
		 :type :int)
   (surface-mu :INITFORM 1d100 :INITARG :mu :ACCESSOR surface-mu)
   (surface-mu2 :INITFORM 0 :INITARG :mu2 :ACCESSOR surface-mu2)
   (surface-rho :INITFORM .1d0 :INITARG :rho :ACCESSOR surface-rho)
   (surface-rho2 :INITFORM 0 :INITARG :rho2 :ACCESSOR surface-rho2)
   (surface-rhon :INITFORM 0 :INITARG :rhon :ACCESSOR surface-rhon)
   (surface-bounce :INITFORM 0 :INITARG :bounce :ACCESSOR surface-bounce)
   (surface-bounce-vel :INITFORM 0 :INITARG :bounce-vel :ACCESSOR
		       surface-bounce-vel)
   (surface-soft-erp :INITFORM 0 :INITARG :soft-erp :ACCESSOR
		     surface-soft-erp)
   (surface-soft-cfm :INITFORM 0 :INITARG :soft-cfm :ACCESSOR
		     surface-soft-cfm)
   (surface-motion1 :INITFORM 0 :INITARG :motion1 :ACCESSOR
		    surface-motion1)
   (surface-motion2 :INITFORM 0 :INITARG :motion2 :ACCESSOR
		    surface-motion2)
   (surface-motionn :INITFORM 0 :INITARG :motionn :ACCESSOR
		    surface-motionn)
   (surface-slip1 :INITFORM 0 :INITARG :slip1 :ACCESSOR surface-slip1)
   (surface-slip2 :INITFORM 0 :INITARG :slip2 :ACCESSOR surface-slip2)))


(defmethod initialize-instance :around ((this physics-object) &key position rotation matrix offset-matrix offset-position offset-rotation)

  (call-next-method)

  (when (body this)
    (ref (body this)))

  (unless (typep this 'physics-plane)
    (if matrix
	(if (body this)
	    (set-transform (body this) matrix)
	    (set-transform this matrix))
	(progn
	  (when position
	    (if (body this)
		(body-set-position (pointer (body this))
				   (first (or position 0))
				   (second (or position 0))
				   (third (or position 0)))
		(geom-set-position (geometry this)
				   (first (or position 0))
				   (second (or position 0))
				   (third (or position 0)))))
	  (when rotation (error "setting rotation is not yet implemented!"))))

    (when (body this)
      (cond (offset-matrix (set-offset-transform this offset-matrix))
	    (offset-position (geom-set-offset-position (geometry this)
						       (first offset-position)
						       (second offset-position)
						       (third offset-position)))
	    (offset-rotation (error "setting rotation is not yet implemented!"))
	    (t t))))





  (setf (gethash (pointer-address (geometry this)) *physics-geometry-hash*) this))


(defmethod unload ((this physics-object) &key)
  (when (body this)
    (unref (body this))

    (setf (slot-value this 'body) nil))

  (when (geometry this)
    (when (gethash (pointer-address (geometry this)) *physics-geometry-hash*)
      (remhash (pointer-address (geometry this)) *physics-geometry-hash*))

    (Geom-Destroy (geometry this))
    (setf (slot-value this 'geometry) nil)))

(defmethod get-transform ((geom physics-object))
  (let ((position (clode:geom-get-position (geometry geom)))
	(rotation (clode:geom-get-rotation (geometry geom))))
    (sb-cga:matrix (n->sf (elt rotation 0)) (n->sf (elt rotation 1)) (n->sf (elt rotation 2))  (n->sf (elt position 0)) 
		   (n->sf (elt rotation 4)) (n->sf (elt rotation 5)) (n->sf (elt rotation 6))  (n->sf (elt position 1))
		   (n->sf (elt rotation 8)) (n->sf (elt rotation 9)) (n->sf (elt rotation 10)) (n->sf (elt position 2))
		   (n->sf 0)                (n->sf 0)                (n->sf 0)                 (n->sf 1))))


(defmethod set-transform ((geom physics-object) (matrix SIMPLE-ARRAY))
  (with-foreign-object (rot 'dReal 12)
    (loop for i from 0 to 11
       do (setf (mem-aref rot 'dreal i) 0))

    (loop
       for pos in '(0 4 8 1 5 9 2 6 10)
       for i from 0 to 11
	 
       do (setf (mem-aref rot 'dreal i) (elt matrix pos)))

    (geom-set-rotation (geometry geom) rot))
  
  (with-foreign-object (pos 'dreal 4)
    (loop for i from 0 to 3 do (setf (mem-aref pos 'dreal i) 0))

    (geom-set-position (geometry geom)
		       (elt matrix 3)
		       (elt matrix 7)
		       (elt matrix 11))))


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


(defmethod set-offset-transform ((geom physics-object) (matrix SIMPLE-ARRAY))
  (with-foreign-object (rot 'dReal 12)
    (loop for i from 0 to 11
       do (setf (mem-aref rot 'dreal i) 0))

    (loop
       for pos in '(0 4 8 1 5 9 2 6 10)
       for i from 0 to 11
	 
       do (setf (mem-aref rot 'dreal i) (elt matrix pos)))

    (geom-set-rotation (geometry geom) rot))
  
  (with-foreign-object (pos 'dreal 4)
    (loop for i from 0 to 3 do (setf (mem-aref pos 'dreal i) 0))

    (geom-set-position (geometry geom)
		       (elt matrix 3)
		       (elt matrix 7)
		       (elt matrix 11))))



(defmethod set-position ((this physics-object) x y z)
  (with-slots ((body body)) this
    (if body 
      (body-set-position (pointer body) x y z)
      (geom-set-position (geometry this) x y z))))

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


(defclass physics-capsule (physics-object)
  ((radius :initform 1
	   :initarg :radius
	   :reader radius)
   (len    :initform 1
	   :initarg :length
	   :reader len)))


(defmethod initialize-instance :after ((this physics-capsule) &key total-mass density)

  (setf (slot-value this 'geometry) (create-capsule (pspace this) (radius this) (len this)))

  (when (and (not (body this))
	     (or total-mass density))
    
    (let ((m (make-instance 'physics-mass)))
      
      (cond (density    (mass-set-capsule (pointer m) density 1 (radius this) (len this)))
	    (total-mass (mass-set-capsule-total (pointer m) density 1 (radius this) (len this)))
	    (t (mass-set-capsule (pointer m) 1 1 (radius this) (len this))))
      (setf (slot-value this 'body) (make-instance 'physics-body :world (world this) :mass m))))
  
  (when (body this)
    (geom-set-body (geometry this) (pointer (body this)))))

 
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



(defclass physics-plane (physics-object)
  ((normal :initform '(0 1 0)
	   :initarg :normal
	   :reader normal)
   (offset :initform 0
	   :initarg :offset
	   :reader offset)))

(defmethod initialize-instance :after ((this physics-plane) &key (normal '(0 1 0)) (position 0))

  (when (body this)
    (error "physics-planes do not have physics-body objects"))

  (setf (slot-value this 'geometry) (create-plane (pspace this)
						  (first normal)
						  (second normal)
						  (third normal)
						  position)))


(defclass physics-ray (physics-object)
  ())

(defmethod initialize-instance :after ((this physics-ray) &key length total-mass density )

  (unless length
    (error "Rays require a length!"))

  (setf (slot-value this 'geometry) (ode:create-ray (pspace this) length))
  
  (when (body this)
    (clode:geom-set-body (geometry this) (pointer (body this)))))


(defmacro combine-surface-properties (surface val1 val2 property)
  (let ((v1 (gensym))
	(v2 (gensym)))
    
    `(setf (foreign-slot-value ,surface '(:struct ode::dSurfaceParameters) ,property)
	   (let ((,v1 ,val1)
		 (,v2 ,val2))
	     (cond ((and ,v1 ,v2) (/ (+ ,v1 ,v2) 2))
		   ((and (null ,v1) ,v2) ,v2)
		   ((and (null ,v2) ,v1) ,v1)
		   (t 0))))))

(defmethod combine-physics-objects (surface (this physics-object) (that physics-object))

  (setf (cffi:foreign-slot-value surface '(:struct ode:dSurfaceParameters) 'ode::mode)
	(cffi:foreign-bitfield-value 'ode::Contact-Enum
				     (union (surface-mode this) (surface-mode that))))
  
  (combine-surface-properties surface
			      (surface-mu this)
			      (surface-mu that)
			      'ode::mu)
  
  (when (or (member :bounce (surface-mode this))
	    (member :bounce (surface-mode that)))

    (combine-surface-properties surface
				(surface-bounce this)
				(surface-bounce that)
				'ode::bounce)

    (combine-surface-properties surface
				(surface-bounce-vel this)
				(surface-bounce-vel that)
				'ode::bounce-vel))

  (when (or (member :soft-cfm (surface-mode this))
  	    (member :soft-cfm (surface-mode that)))
    (combine-surface-properties surface 
  				(surface-soft-cfm this)
  				(surface-soft-cfm that)
  				'ode::soft-cfm))


  (when (or (member :mu2 (surface-mode this))
  	    (member :mu2 (surface-mode that)))
    (combine-surface-properties surface
  				(surface-mu2 this)
  				(surface-mu2 that)
  				'ode::mu2))
  
  (when (or (member :rolling (surface-mode this))
  	    (member :rolling (surface-mode that)))

    (combine-surface-properties surface
    				(surface-rho this)
    				(surface-rho that)
    				'ode::rho)
    
    (combine-surface-properties surface
    				(surface-rho2 this)
    				(surface-rho2 that)
    				'ode::rho2)
    
    (combine-surface-properties surface
    				(surface-rhoN this)
    				(surface-rhoN that)
    				'ode::rhoN)))



(defmethod remove-vector (v1 v2)
  (let* ((fv1 (COERCE-FLOATS v1))
	 (fv2 (COERCE-FLOATS v2))
	 (n (sb-cga:normalize fv2))
	 (dot (sb-cga:dot-product fv1 n)))
    (if (> dot 0)
	(sb-cga:vec- fv1 (sb-cga:vec* n dot))
	v1)))
	         

(defmethod close-callback ((this physics-object) (that physics-object))

  (let* ((o1 (geometry this))
	 (o2 (geometry that))
	 (b1 (geom-get-body o1))
	 (b2 (geom-get-body o2)))
    
    (with-foreign-object (contact '(:struct dContact) *physics-max-contacts*)
      
      (let* ((surface (foreign-slot-pointer contact '(:struct ode::dContact) 'ode::surface))
	     (gg (foreign-slot-pointer contact '(:struct ode::dContact) 'ode::geom)))
	
	(combine-physics-objects surface this that)
	
	(let ((num-contacts (collide o1 o2 *physics-max-contacts* gg (foreign-type-size '(:struct ode::dContact)))))
	  (unless (zerop num-contacts)

	    (when (or (and (not (cffi:null-pointer-p b1)) (not (zerop (clode::body-is-kinematic b1))))
		      (and (not (cffi:null-pointer-p b2)) (not (zerop (clode::body-is-kinematic b2)))))
	    
	      (when (cffi:null-pointer-p b1)
		(let ((vel (remove-vector (clode:body-get-linear-vel b2)
					  (foreign-slot-value gg '(:struct ode::dContactGeom) 'ode::normal))))

		  (print vel)
		  (clode:body-set-linear-vel b2 (aref vel 0) (aref vel 1) (aref vel 2))))

	      (when (cffi:null-pointer-p b2)
		(let ((vel (remove-vector (clode:body-get-linear-vel b1)
					  (foreign-slot-value gg '(:struct ode::dContactGeom) 'ode::normal))))

		  (print vel)
		  (clode:body-set-linear-vel b1 (aref vel 0) (aref vel 1) (aref vel 2)))))


	    (loop for x from 0 to (1- num-contacts)
	       do (joint-attach
		   (joint-create-contact *physics-world* *physics-contact-group* (cffi:mem-aptr contact '(:struct clode::dContact) x))
		   b1 b2))))))))



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
		     (x  (+ (* .90 (aref vel 1))
			    (* 1 (abs (- (abs distance) 3))))))
		;; (+ (* 
		;; 	(abs (* 1/25 (max 0 (- distance 1.5))))))))
		
		;;(format t "~A vs ~A~%" vel x)
		
		(clode:body-set-linear-vel b1 (aref vel 0) x (aref vel 2))))))))))

  
(defmethod close-callback ((this physics-object) (that physics-ray))
  (close-callback that this))


(defun physics-near-handler (data o1 o2)

  (unless (cffi:pointer-eq o1 o2)
    
    (let* ((lisp-object1 (gethash (pointer-address o1) *physics-geometry-hash*))
	   (lisp-object2 (gethash (pointer-address o2) *physics-geometry-hash*)))

      (when (and lisp-object1 lisp-object2)
	
	(close-callback lisp-object1 lisp-object2)))))



(defun physics-init (&key (quad-tree t) (origin '(0 0 0)) (extents '(10 10 10)) (depth 10))  
  
  (init-ode)	      
  
  
  (setf *physics-world*         (world-create)
	*physics-space*         (if quad-tree
				    (quad-tree-space-create (null-pointer)
							    (make-array 4 :initial-contents (append origin (list 0)))
							    (make-array 4 :initial-contents (append extents (list 0)))
							    depth)
				    (hash-space-create (null-pointer)))
	*physics-contact-group* (joint-group-create 0))
  
  (world-set-gravity *physics-world* 0 -6 0)
  (world-set-cfm *physics-world* 1e-5)
  (world-set-damping *physics-world* .001 .001)
  (world-set-linear-damping-threshold *physics-world* 0.00001)
  (world-set-angular-damping-threshold *physics-world* .005)
  (clode:world-set-auto-disable-flag  *physics-world* 1)
  
  (setf *physics-geometry-hash* (make-hash-table :test 'eql)))

(defun physics-step (step handler)

  (space-collide *physics-space* (null-pointer) handler)
  (world-quick-step *physics-world* step)
  (joint-group-empty *physics-contact-group*))

(defun physics-uninit ()
  (joint-group-destroy *physics-contact-group*)      
  (world-destroy *physics-world*)
  (space-destroy *physics-space*)

  (close-ode)

  (setf *physics-world* nil
	*physics-space* nil
	*physics-contact-group* nil))




