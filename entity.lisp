;;;; entity.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defclass entity ()
  ((shader
    :initform nil
    :initarg :shader
    :reader shader)
   (indexes
    :initform nil
    :initarg :indexes
    :reader indexes)
   (render-values
    :initform nil
    :initarg :values
    :reader render-values))
  (:documentation "Renders a mesh with a shader with attributes and uniforms."))


(defmethod initialize-instance :after ((this entity) &key (compile t) parent (strict-index nil))
  "Creates an entity.
    :parent adds itself to the given parent. The entity doesn't keep track of its parent."
  (when parent (add-child parent this)))

(defmethod (setf shader) (new-value (this entity))
  "Sets the shader to use."
  (sdl2:in-main-thread ()
    (setf (slot-value this 'shader) new-value)))

(defmethod (setf indexes) (new-value (this entity))
  "Sets the index array."
  (sdl2:in-main-thread ()
    (setf (slot-value this 'indexes) new-value)))

(defmethod (setf render-values) (new-value (this entity))
  "Sets all the render values. Be sure the use the correct format."
  (sdl2:in-main-thread ()
    (setf (slot-value this 'render-values) new-value)))
		       
(defun render-value-location (values key)
  "Returns the 'location' of the key."
  (loop
     for i in values 
     for x from 0
     if (equal key (second i))
     do (return x)))

(defun assoc-on-second (item lst) 
  (or (when (equal item (cadar lst))
	(car lst))
      (assoc-on-second item (cdr lst))))

(defmethod render-value ((this entity) name)
  "Retuns the value of the render attribute or uniform."
  (third 
   (assoc-on-second name (clinch::render-values this))))

(defmethod (setf render-value) (new-value (this entity) name)
  "Sets the value of a single render attribute or uniform."
  (let ((ret
	 (with-accessors ((lst render-values)) this
	   (let ((loc (render-value-location lst name)))
	     (if loc
		 (sdl2:in-main-thread ()
		 (setf (third (nth loc lst)) new-value)))))))
    ret))

(defmethod get-primitive ((this entity) name)
  (let* ((buff      (get-render-value this name))
	 (stride    (stride buff))
	 (icount    (vertex-count (indexes this)))
	 (itype     (qtype (indexes this)))
	 (btype     (clinch:qtype buff))
	 (iret      (make-array (/ icount 3)))
	 (bret      (make-array (/ icount 3))))

    (clinch:with-mapped-buffer (iptr (indexes this) :read-only)
      (clinch:with-mapped-buffer (bptr buff :read-only)
	
	(dotimes (i (/ icount 3))
	  (let ((iarr1 (make-array 3 :element-type 'integer))
		(barr1 (make-array 3)))
	    
	    (dotimes (j 3)
	      (setf (elt iarr1 j) (cffi:mem-aref iptr itype (+ (* i 3) j)))

	      (let ((barr2 (make-array stride :element-type 'single-float)))
		(dotimes (k stride)
		  (setf (elt barr2 k)
			(cffi:mem-aref bptr btype (+ k (* (elt iarr1 j) stride)))))

		(setf (elt barr1 j) barr2)))
	    
	    (setf (elt iret i) iarr1)
	    (setf (elt bret i) barr1)))))

    (values bret iret)))


(defmethod triangle-intersection? ((this entity) start dir &key (vertex-name :vertices)) ;; !!!
  "Returns distance u, v coordinates and index of the closest triangle (if any) touched by the given the ray origin and direction and the name of the vertex buffer attribute."
  (labels ((rec (primitives i distance u v index)
	     (multiple-value-bind (new-distance new-u new-v)
		 
		 (ray-triangle-intersect? start dir (first (car primitives)) (second (car primitives)) (third (car primitives)))
	       (when (and new-distance
			  (or (null distance)
			      (< new-distance distance)))
		 (setf distance new-distance
		       u new-u
		       v new-v
		       index i)))
	     (if (cdr primitives)
		 (rec (cdr primitives) (1+ i) distance u v index)
		 (values distance u v index))))
    (rec (get-primitive this vertex-name) 0 nil nil nil nil)))



(defmethod draw ((this entity) &key parent projection)
  "Draws the object. Should be removed and put into render.";; !!!!
  (with-accessors ((shader shader)) this
    (when shader 
      (let ((current-shader (if (typep shader 'function)
				(funcall shader)
				shader)))
	(use-shader current-shader)
	
	(loop
	   with tex-unit = 0
	   for (atr-or-uni name value) in (render-values this)
	   if (typep value 'function) do (setf value (funcall value))
	   collect (progn
		     (cond ((and (eql atr-or-uni :uniform)
				 (typep value 'texture)) (prog1 (bind-sampler value current-shader name tex-unit) (incf tex-unit)))
			   ((eql atr-or-uni :uniform)
			    
			    (attach-uniform current-shader name (cond ((eql value :projection) projection)
								      ((eql value :Model)      (or parent (m4:identity)))
								      ((eql value :model-1) (typecase parent
											      (node (inverse parent))
											      (array (m4:affine-inverse parent))
											      (t (m4:identity))))
								      ((eql value :projection-1) (m4:affine-inverse projection))
								      ((eql value :normal) (typecase parent
											     (node
											      (m4:to-matrix3 
											       (m4:transpose
												(m4:affine-inverse (transform parent)))))
											     (array (m4:to-matrix3 
												     (m4:transpose
												      (m4:affine-inverse parent))))
											     (t (m4:identity))))
								      (t value))))
			   
			   ((and (eql atr-or-uni :attribute)
				 (typep value 'buffer)) 
			    (bind-buffer-to-attribute-array value current-shader name))
			   ((eql atr-or-uni :attribute) (if (atom value)
							    (bind-static-values-to-attribute current-shader name value)
							    (bind-static-values-to-attribute current-shader name value)))))))))

  
  (draw-with-index-buffer (indexes this)))

(defmethod update ((this entity) &key parent matrix force)
  )

(defmethod render ((this entity) &key parent projection)
  "Renders the entity (mesh).
    :parent Sets the parent for the :model"
  (draw this :parent parent :projection projection))

(defmethod ray-entity-intersect? ((this clinch:entity) transform start end &optional (primitive :vertices))

  (multiple-value-bind (points index) (clinch::get-primitive this primitive)
    (let ((transformed-points (map 'list (lambda (x)
					   (map 'list (lambda (p) 
							(clinch:transform-point p transform)) x)) points)))
      (loop
	 with dist 
	 with u 
	 with v
	 with point
	 with point-number
	 for p from 0 to (1- (length transformed-points))
	 do (let ((pseq (elt transformed-points p)))
	      (multiple-value-bind (new-dist new-u new-v)
		  (clinch::ray-triangle-intersect? start end (elt pseq 0) (elt pseq 1) (elt pseq 2))
		
		(when (and new-dist
			   (or (null dist)
			       (> dist new-dist)))
		  (setf dist         new-dist
			u            new-u
			v            new-v
			point-number p)
		  (when index
		    (setf point (elt index p))))))
	 finally (return (when dist (values dist u v point point-number)))))))

(defmethod unload ((this entity) &key)
  "Release entity resources. Actually, there are none. It should just clear out it's values." ;; !!!!
  )

