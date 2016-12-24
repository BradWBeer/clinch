;;;; node.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defparameter *root* nil
  "The default root node.")

(defparameter v0 (v! 0 0 0))
(defparameter vi (v! 1 1 1))

(defclass node ()
  ((name :accessor name
	 :initform nil
	 :initarg :name)
   (trans :reader translation
	  :initform v0
	  :initarg  :translation)
   (rot :reader rotation
	:initform (q:identity)
	:initarg :rotation)
   (scale    :reader scaling
	     :initform vi
	     :initarg  :scale)
   (t-matrix :initform nil)
   (r-matrix :initform nil)
   (s-matrix :initform nil)
   (transform :initform nil)
   (current-transform
    :initform nil
    :reader current-transform)
   (enabled
    :accessor enabled
    :initform t
    :initarg :enabled)
   (children :accessor children
	     :initform nil
	     :initarg  :children))
  (:documentation "A node class for creating hierarchies of objects. It caches calculations for speed. Not enough in itself, and is not required by Clinch."))

(defgeneric !reset (this)
  (:documentation "Resets a node to default. Position 0, rotation 0 and scale of 1. 
Shortcut is !0."))
(defgeneric !reset-translation (this)
  (:documentation "Resets a node's translation to 0,0,0.
Shortcut is !t0"))
(defgeneric !reset-rotation (this)
  (:documentation "Resets a node's rotation to 1,0,0,0.
Shortcut is !r0"))
(defgeneric !reset-scaling (this)
  (:documentation "Resets a node's scaling to 1,1,1.
Shortcut is !s0"))

(clone-function !reset !0)
(clone-function !reset-translation !t0)
(clone-function !reset-rotation !r0)
(clone-function !reset-scaling !s0)

(defgeneric (setf translation) (val this))
(defgeneric (setf rotation) (val this))
(defgeneric (setf scaling) (val this))

(defgeneric translation-matrix (this &key)
  (:documentation "Get the translation matrix."))
(defgeneric rotation-matrix (this &key)
  (:documentation "Get the rotation matrix."))
(defgeneric scale-matrix (this &key)
  (:documentation "Get the scale matrix."))
(defgeneric transform (this &key)
  (:documentation "get the entire transform."))
(defgeneric translate (this trans &key)
  (:documentation "Translate node.
Shortcut is !t."))
(defgeneric rotate (this rot &key)
  (:documentation "Rotate node.
Shortcut is !r."))
(defgeneric scale (this scale &key)
  (:documentation "Scale node.
Shortcut is !s."))

(defgeneric n* (this that &key)
  (:documentation "Multiplies nodes and matrix transforms."))

(defmethod initialize-instance :after ((this node) &key translation rotation scale copy matrix (parent *root*))
  "Creates a node with optional translation (vector3), rotation (quaterion) and scaling (vector3). Can also use another node or matrix to set its value. If a node and another value is give, the other value is used starting with the matrix."

  (if matrix
      (multiple-value-bind (tr r s) (decompose-transform matrix)
	(setf (translation this) tr
	      (rotation this)    r
	      (scaling this)     s))
      (setf (translation this) (or translation
				   (and copy (copy-seq (translation copy)))
				   v0)
	    (rotation this) (or rotation
				(and copy (copy-seq (rotation copy)))
				(q:identity))
	    (scaling this) (or scale
			       (and copy (copy-seq (scaling copy)))
			       vi))))	


(defmethod !reset ((this node))
  "Resets the node."
  (setf (translation this) v0)
  (setf (rotation this)    (q:identity))
  (setf (scaling this)     vi)
  (setf (slot-value this 't-matrix) nil)
  (setf (slot-value this 'r-matrix) nil)
  (setf (slot-value this 's-matrix) nil)
  (setf (slot-value this 's-matrix) nil)
  (setf (slot-value this 'transform) nil))

(defmethod !reset-translation ((this node))
  (setf (slot-value this 't-matrix) nil
	(translation this) v0))

(defmethod !reset-rotation ((this node))
  (setf (slot-value this 'r-matrix) nil
	(rotation this) (q:identity)))

(defmethod !reset-scaling ((this node))
  (setf (slot-value this 's-matrix) nil
	(scaling this) vi))


(defmethod print-object ((this node) s)
  "Print function for node."
  (with-accessors ((name name)) this
    (format s "#<NODE ~A children: ~A ~A>"
	    (cond
	      ((stringp name) (concatenate 'string "\"" name "\""))
	      (name name)
	      (t "<unnamed>"))
	    (length (children this)) (transform this))))

(defmethod changed? ((this node))
  "Has this node changed and not updated?"
  (null (slot-value this 'transform)))

(defmethod (setf changed?) (val (this node))
  "Set this node to update later."
  (if val
      (setf (slot-value this 'transform) nil)
      (transform this)))

(defmethod add-child ((this node) child &key)
  "Add a child. Children must implement update and render."
  (with-accessors ((children children)) this
    (unless (member child children)
      (setf children
	    (cons child children)))))

(defmethod remove-child ((this node) child &key)
  "Removes a child. Does not unref removed children."
  (with-accessors ((children children)) this
    
    (when (member child children)
      
      (setf children
	    (remove child children)))))

(defmethod render ((this node) &key parent (projection *projection*))
  "Render child objects. You don't need to build your application with nodes/render. This is just here to help."
  (when (enabled this)
    (let ((current-transform
	   (cond ((typep parent 'node) (m:* (transform parent) (transform this)))
		 (parent (m:*  parent (transform this)))
		 (t (transform this)))))
      
      ;; for animation this needs to update the nodes first. Then render.
      ;; This will get better when I move to an aspect oriented system.
      (loop for i in (children this)
	 unless (typep i 'entity)
	 do (render i :parent current-transform :projection projection))
      ;; Now render...
      (loop for i in (children this)
      	 if (typep i 'entity)
      	 do (render i :parent current-transform :projection projection)))
    ))

(defmethod render ((this list) &key parent (projection *projection*))
  "Render a list of rendables."
  (when (enabled this)

    (let ((current-transform
	   (cond ((and parent (typep parent 'node)) (m:* (transform this) (transform parent)))
		 ((and parent (arrayp parent)) (m:* (transform this) parent)))))
      
      (loop for i in this
	 do (progn 
	      (when (arrayp i) (setf current-transform (m:* i current-transform)))
	      (render i :parent current-transform :projection projection))))))

(defmethod update ((this node) &key parent (projection *projection*))
  "Render child objects. You don't need to build your application with nodes/render. This is just here to help."
  (when (enabled this)
    (let ((current-transform
	   (cond ((typep parent 'node) (m:* (transform parent) (transform this)))
		 (parent (m:*  parent (transform this)))
		 (t (transform this)))))
      (setf (slot-value this 'current-transform) current-transform)
      ;; for animation this needs to update the nodes first. Then render.
      ;; This will get better when I move to an aspect oriented system.
      (loop for i in (children this)
	 when (typep i 'node)
	 do (update i :parent current-transform :projection projection)))))

;;!!!!!!! TODO: Add render for function type. Might be useful for animation and more.

(defmethod (setf translation) (trans (this node))
  "Sets the translation vector."
  (with-slots ((current trans)) this
    (if (v3:= trans current) current
	(progn
	  (setf (slot-value this 't-matrix) nil
		current trans)))))

(defmethod translation-matrix ((this node) &key)
  "Gets the translation matrix."
  (or (slot-value this 't-matrix)
      (setf (slot-value this 't-matrix)
	    (m4:translation (translation this)))))


(defmethod (setf rotation) (rot (this node))
  "Sets the rotation quaterion."
  (with-slots ((current rot)) this
    (if (v4:= rot current) current
	(progn
	  (setf (slot-value this 'r-matrix) nil
		current rot)))))

(defmethod rotation-matrix ((this node) &key)
  "Gets the rotation matrix."
  (or (slot-value this 'r-matrix)
      (setf (slot-value this 'r-matrix)
	    (q:to-mat4 
	     (rotation this)))))

(defmethod (setf scaling) (scale (this node))
  "Sets the scaling vector."
  (with-slots ((current scale)) this
    (if (v3:= scale current) current
	(progn
	  (setf (slot-value this 's-matrix) nil
		current scale)))))

(defmethod scale-matrix ((this node) &key)
  "Gets the scaling matrix."
  (or (slot-value this 's-matrix)
      (setf (slot-value this 's-matrix)
	    (m4:scale (scaling this)))))

(defmethod (setf transform) (transform (this node))
  (multiple-value-bind (tr r s) (decompose-transform transform)
    (setf (translation this) tr
	  (rotation this)    r
	  (scaling this)     s)
    transform))


(defmethod transform ((this node) &key)
  "Gets the transform matrix."
  (with-slots ((scale s-matrix)
	       (trans t-matrix)
	       (rot   r-matrix)
	       (transform transform)) this

    (if (and scale rot trans transform)
	transform
	(setf (slot-value this 'transform)
	      (reduce #'m:* (list (or trans (translation-matrix this))
				  (or rot   (rotation-matrix this))
				  (or scale (scale-matrix this))))))))

(defmethod rotate ((this node) rot &key (modify t))
  "Rotate the node. Takes a quaterion."
  (if modify 
      (setf (rotation this) 
	    (q:* rot (rotation this)))
      (q:* rot (rotation this))))

(defmacro !r (this w x y z &optional in-place)
  (let ((tthis (gensym))
	(v (gensym)))
    
    `(let ((,tthis ,this)
	   (,v (q:from-axis-angle (v! ,x ,y ,z) (d->r ,w))))
       (if ,in-place
	   (setf (rotation ,tthis) ,v)
	   (rotate ,tthis ,v)))))

(defmethod translate ((this node) trans &key (modify t))
  "Translate the node. Takes a vector3."
  (if modify 
      (setf (translation this) 
	    (v:+ trans (translation this)))
      (v:+ trans (translation this))))

(defmacro !t (this x y z &optional in-place)
  (let ((tthis (gensym))
	(v (gensym)))
    
    `(let ((,tthis ,this)
	   (,v (v! ,x ,y, z)))
       (if ,in-place
	   (setf (translation ,tthis) ,v)
	   (translate ,tthis ,v)))))

(defmethod scale ((this node) size &key (modify t))
  "Scales a node. Takes a vector3."
  (if modify 
      (setf (scaling this) 
	    (v:* size (scaling this)))
      (v:* size (scaling this))))

(defmacro !s (this x y z &optional in-place)
  (let ((tthis (gensym))
	(v (gensym)))
    
    `(let ((,tthis ,this)
	   (,v (v! ,x ,y, z)))
       (if ,in-place
	   (setf (scaling ,tthis) ,v)
	   (scale ,tthis ,v)))))


(defmethod n* ((this node) (that node) &key new-node)
  "Multiplies a node? I'm not sure if this works." ;;!!!!
  (let* ((matrix (normalize-for-3d
		  (m:* (transform this)
		       (transform that)))))
    (if new-node
	(make-instance 'node :matrix matrix)
	matrix)))

(defmethod n* ((this vector) (that node) &key new-node)
  "Multiplies a node? I'm not sure if this works." ;;!!!!
  (let* ((matrix (normalize-for-3d
		  (m:* this
		       (transform that)))))
    (if new-node
	(make-instance 'node :matrix matrix)
	matrix)))

(defmethod n* ((this node) (that vector) &key new-node)
  "Multiplies a node? I'm not sure if this works." ;;!!!!
  (let* ((matrix (normalize-for-3d (m:* (transform this)
					that))))
    (if new-node
	(make-instance 'node :matrix matrix)
	matrix)))

(defmethod n* ((this vector) (that vector) &key new-node)
  "Multiplies a node? I'm not sure if this works." ;;!!!!
  (let* ((matrix (normalize-for-3D (m4:* this that))))
    (if new-node
	(make-instance 'node :matrix matrix)
	matrix)))

(defmethod traverse-node ((this node))
  "Not yet implemented."
  (loop for c in (children this)
     when c
     append (cond 
	      ((typep c 'node) (traverse-node c))
	      (t (list c)))))

(defmethod walk-node-tree (this f &key test non-node)
  (when (or (null test)
	    (funcall test this))
    (funcall f this)))


(defmethod walk-node-tree ((this node) f &key test non-node) 
  "Render child objects. You don't need to build your application with nodes/render. This is just here to help."
  (when (enabled this)
    (when (or (null test)
	      (funcall test this))
      (funcall f this))
    
    (loop for i in (children this)
       if (or non-node (typep i 'node))
       do (walk-node-tree i f :test test :non-node non-node))))

(defmethod node-top-map-string ((this node) &optional parent)
  (let ((children 
	 (loop for n in (children this)
	    when (typep n 'node)
	    append (node-top-map-string n this))))
    (if (and parent (name parent))
	(cons (list (name this) (name parent))
	      children)
	children)))

(defmethod node-top-map ((this node) &optional parent)
  (let ((children 
	 (loop for n in (children this)
	    when (typep n 'node)
	    append (node-top-map n this))))
    (if (and parent parent)
	(cons (list this parent)
	      children )
	children)))
