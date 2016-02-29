;;;; entity.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defclass entity ()
  ((shader-program
    :initform nil
    :initarg :shader-program
    :reader shader-program)
   (indexes
    :initform nil
    :initarg :indexes
    :reader indexes)
   (mode :initform :triangles
	 :initarg :mode
	 :reader mode)
   (render-values
    :initform nil
    :initarg :values
    :reader render-values)
   (uniforms 
    :initform nil
    :initarg :uniforms 
    :reader uniforms)
   (attributes
    :initform nil
    :initarg :attributes
    :reader attributes)
   (enabled
    :accessor enabled
    :initform t
    :initarg :enabled))
  (:documentation "Renders a mesh with a shader-program with attributes and uniforms."))


(defmethod initialize-instance :after ((this entity) &key (compile t) parent (strict-index nil))
  "Creates an entity.
    :parent adds itself to the given parent. The entity doesn't keep track of its parent."
  (when parent (add-child parent this)))

(defmethod (setf shader-program) (new-value (this entity))
  "Sets the shader-program to use."
  (sdl2:in-main-thread ()
    (setf (slot-value this 'shader-program) new-value)))

(defmethod (setf indexes) (new-value (this entity))
  "Sets the index array."
  (sdl2:in-main-thread ()
    (setf (slot-value this 'indexes) new-value)))

(defmethod attribute ((this entity) name)
  "Returns an attribute by name. Should work with numbers and strings."
  (cdr (assoc name (attributes this) :test #'equal)))

(defmethod (setf attribute) (new-value (this entity) name)
  "Sets an attribute's value. If the name doesn't exist, it's added. If the new value is nil, the entry is deleted."
  (with-slots ((attr attributes)) this

    (if (null new-value)
	(setf attr (remove-if (lambda (x) (equal (car x) name)) attr))
	(let ((item (assoc name attr :test #'equal)))
	  (if item
	      (setf (cdr item) new-value)
	      (setf attr (acons name new-value attr))))))
  new-value)

(defmethod uniform ((this entity) name)
  "Returns a uniform by name. Should work with numbers and strings."
  (cdr (assoc name (uniforms this) :test #'equal)))

(defmethod (setf uniform) (new-value (this entity) name)
  "Sets a uniform's value. If the name doesn't exist, it's added. If the new value is nil, the entry is deleted."
  (with-slots ((uni uniforms)) this

    (if (null new-value)
	(setf uni (remove-if (lambda (x) (equal (car x) name)) uni))
	(let ((item (assoc name uni :test #'equal)))
	  (if item
	      (setf (cdr item) new-value)
	      (setf uni (acons name new-value uni))))))
  new-value)

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

(defmethod draw ((this entity) &key parent projection)
  "Draws the object. Should be removed and put into render.";; !!!!
  (with-accessors ((shader-program shader-program)) this
    (when shader-program 
      (let ((current-shader-program (if (typep shader-program 'function)
				(funcall shader-program)
				shader-program)))
	(use-shader-program current-shader-program)
	
	;; first attach attributes...
	(loop for (name . value) in (attributes this)
	   do (progn
		(when (typep value 'function)
		  (setf value (funcall value)))
		(cond ((typep value 'buffer)
		       (bind-buffer-to-attribute-array value current-shader-program name))
		      (t (bind-static-values-to-attribute current-shader-program name value)))))

	(loop
	   with tex-unit = 0
	   for (name . value) in (uniforms this)
	   do (progn
		(when (typep value 'function)
		  (setf value (funcall value)))
		(cond ((typep value 'texture) (prog1 (bind-sampler value current-shader-program name tex-unit) (incf tex-unit)))
		      (t (attach-uniform current-shader-program name (cond ((eql value :projection) projection)
									   ((eql value :Model)      (or parent (m4:identity)))
									   ((eql value :model-1) (typecase parent
												   (node (inverse parent))
												   (array (m4:affine-inverse parent))
												   (t (m4:identity))))
									   ((eql value :projection-1) (m4:affine-inverse projection))
									   ((eql value :normal) (typecase parent
												  (node
												   (m4:to-mat3 
												    (m4:transpose
												     (m4:affine-inverse (transform parent)))))
												  (array (m4:to-mat3 
													  (m4:transpose
													   (m4:affine-inverse parent))))
												  (t (m3:identity))))
									   (t value))))))))))
  (if (indexes this)
      (draw-with-index-buffer (indexes this) :mode (mode this))
      (gl:draw-arrays (mode this) 0 (find-first-vertex-length this))))

(defmethod update ((this entity) &key parent matrix force)
  )

(defmethod render ((this entity) &key parent projection)
  "Renders the entity (mesh).
    :parent Sets the parent for the :model"
  (when (enabled this)
    (draw this :parent parent :projection projection)))

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

