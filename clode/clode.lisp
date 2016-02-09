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




(defmethod remove-vector (v1 v2)

  (let* ((fv1 (COERCE-FLOATS v1))
	 (fv2 (COERCE-FLOATS v2))
	 ;(n (sb-cga:normalize fv2))
	 (dot (sb-cga:dot-product fv1 fv2))
	 (ret (if (> dot 0)
		  (sb-cga:vec- fv1 (sb-cga:vec* fv2 dot))
		  v1)))

    ret))






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




