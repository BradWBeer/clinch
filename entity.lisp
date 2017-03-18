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
   (mode 
    :initform :triangles
    :initarg :mode
    :reader mode)
   (uniforms 
    :initform nil
    :initarg :uniforms 
    :reader uniforms)
   (attributes
    :initform nil
    :initarg :attributes
    :reader attributes)
   (bones
    :initform nil
    :initarg :bones
    :accessor bones)
   (enabled
    :accessor enabled
    :initform t
    :initarg :enabled))
  (:documentation "Renders a mesh with a shader-program with attributes and uniforms."))

(defmethod print-object ((this entity) s)
  "Print function for node."
  (format s "#<ENTITY>"))


(defmethod initialize-instance :after ((this entity) &key (compile t) parent (strict-index nil))
  "Creates an entity.
    :parent adds itself to the given parent. The entity doesn't keep track of its parent. (t means the root node)
    :indexes sets the index buffer. Required.
    :mode sets what kind of object (triangle, square, etc) will be drawn. Only triangles are tested.
    :uniforms sets the uniform values as an alist.
    :attributes sets the attribute values as an alist.
    :shader-program sets the shader program.
    :enabled sets if this entity will render."
  (when parent (if (eq parent t)
		   (add-child *node* this)
		   (add-child parent this))))

(defmethod (setf shader-program) (new-value (this entity))
  "Sets the shader-program to use."
  (! (setf (slot-value this 'shader-program) new-value)))

(defmethod (setf indexes) (new-value (this entity))
  "Sets the index array."
  (! (setf (slot-value this 'indexes) new-value)))

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

(defmacro with-attributes (args this &body body)
  `(symbol-macrolet 
       ,(loop for (var attr) in args
	   collect `(,var (attribute ,this ,attr)))
     ,@body))

(defmacro with-uniforms (args this &body body)
  `(symbol-macrolet 
       ,(loop for (var attr) in args
	   collect `(,var (uniform ,this ,attr)))
     ,@body))

(defun convert-non-buffer (this value &key projection parent)
  (cond ((eql value :projection) (or projection (m4:identity)))
	((eql value :Model)      (or parent (m4:identity)))
	((eql value :model-1) (typecase parent
				(node (inverse parent))
				(array (m4:affine-inverse parent))
				(t (m4:identity))))
	((eql value :projection-1) (m4:affine-inverse projection))
	((eql value :skeleton) (bone-buffer (bones this)))
	((eql value :bone-ids)  (bone-id-buffer (bones this)))
	((eql value :bone-weights) (weights-buffer (bones this)))
	((eql value :normal) (typecase parent
			       (node
				(m4:to-mat3 
				 (m4:transpose
				  (m4:affine-inverse (transform parent)))))
			       (array (m4:to-mat3 
				       (m4:transpose
					(m4:affine-inverse parent))))
			       (t (m3:identity))))
	(t value)))

(defmethod render ((this entity) &key parent (projection *projection*))
  "Renders the entity (mesh).
    :parent Sets the parent for the :model"
 
  (when (enabled this)
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
		  (when (typep value 'animator)
		    (setf value (render value)))
		  
		  (cond ((typep value 'buffer)
			 (bind-buffer-to-attribute-array value current-shader-program name))
			(t (bind-static-values-to-attribute 
			    current-shader-program 
			    name 
			    (convert-non-buffer this value :projection projection :parent parent))))))
	  (loop
	     with tex-unit = 0
	     for (name . value) in (uniforms this)
	     do (progn
		  (when (typep value 'function)
		    (setf value (funcall value)))
		  (when (typep value 'animator)
		    (setf value (render value)))
		  (cond ((typep value 'texture) (prog1 (bind-sampler value current-shader-program name tex-unit) (incf tex-unit)))
			(t (attach-uniform current-shader-program name 
					   (convert-non-buffer this value :projection projection :parent parent)))))))))

    (draw-with-index-buffer (indexes this) :mode (mode this))))

(defmethod update ((this entity) &key parent matrix force)
  "Dummy method when updating nodes.")


(defmethod ray-entity-intersect? ((this clinch:entity) transform start end &optional (primitive :vertices))

  (multiple-value-bind (points index) (clinch::get-primitive this primitive)
    (let ((transformed-points (map 'list (lambda (x)
					   (map 'list (lambda (p) 
							(clinch:transform-point p transform)) x)) points)))
      (print transformed-points)
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

(defmethod unload ((this entity) &key all)
  "Release entity resources. If :all t, then the index buffer and all uniforms and attributes are unloaded."
  (setf (enabled this) nil)
  (when all
    (when (indexes this) (unload (indexes this)))
    (loop for (n . v) in (uniforms this)
       do (unload v))
    (loop for (n . v) in (attributes this)
       do (unload v))))
  
  

