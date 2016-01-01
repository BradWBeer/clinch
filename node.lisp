;;;; node.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defclass node (element refcount)
  ((transform
    :accessor transform
    :initform  (sb-cga:identity-matrix)
    :initarg  :transform)
   (current-transform
    :accessor current-transform
    :initform nil)
   (enabled
    :accessor enabled
    :initform t
    :initarg :enabled))
  (:documentation "A node class for creating hierarchies of objects. It caches calculations for speed. Not enough in itself, and not required."))

(defmethod initialize-instance :after ((this node) &key parent)

  (loop for i in (children this)
       do (when (typep i 'refcount)
	    (ref i))))

(defmethod print-object ((this node) s)
  "Print function for node."
  (format s "#<NODE children: ~A ~%~A>" (length (children this)) (transform this)))

(defmethod changed? ((this node))
  "Has this node changed and not updated?"
  (null (slot-value this 'current-transform)))


(defmethod (setf changed?) (val (this node))
  "Set this node to update later."
  (setf (slot-value this 'current-transform) (if val nil t)))

(defmethod add-child ((this node) child &key)
  "Add a child. Children must implement update and render."
  (with-accessors ((children children)) this
    (unless (member child children)
      
      (when (typep child 'refcount)
	(ref child))      

      (setf children
	    (cons child children)))))

(defmethod remove-child ((this node) child &key)
  "Removes a child."
  (with-accessors ((children children)) this
    
    (when (member child children)

      (when (typep child 'refcount)
	(unref child))
      
      (setf children
	    (remove child children)))))


(defmethod update ((this node) &key parent force)
  "Update this and child nodes if changed."
  (when (or force (changed? this))
    (setf (current-transform this)
	  (if parent
	      (sb-cga:matrix* (current-transform parent) (transform this))
	      (transform this)))
    (setf force t))
  
  (loop for child in (children this)
     do (update child :parent this :force force))
  
  (current-transform this))

(defmethod render ((this node) &key parent projection)
  "Render child objects. You don't need to build your application with nodes/render. This is just here to help."
  (when (enabled this)
    (when (once this)
      (funcall (once this) this)
      (setf (once this) nil))

    (when (before-render this)
      (let ((*parent* this))
	(funcall (before-render this) this)))

    (when (changed? this)
      (update this :parent parent))
    
    (gl:matrix-mode :modelview)
    
    (load-matrix this)
    
    (loop for i in (children this)
       do (render i :parent this :projection projection))

    (when (after-render this)
      (let ((*parent* this))
	(funcall (after-render this) this)))))

(defmethod render ((this list) &key parent projection)
  "Render a list of rendables."
  (when (enabled this)
    (load-matrix this)
    
    (loop for i in this
       do (render i :parent parent :projection projection))))


(defmethod (setf transform)  ((other-node array) (this node))
  "Inherited function for setting changed?"
  (setf (slot-value this 'transform)
	other-node)
  (setf (changed? this) t)
  (transform this))

(defmethod set-identity-transform ((this node) &key)
  "Inherited function for setting changed?"
  (setf (transform this) (sb-cga:identity-matrix)))

(defmethod m* ((this node) (that node) &optional (in-place t))
  "Inherited function for setting changed?"
  (if in-place
      (setf (transform this) (sb-cga:matrix* (transform that) (transform this)))
      (sb-cga:matrix* (transform that) (transform this))))

(defmethod transpose ((this node) &optional (in-place t))
  "Inherited function for setting changed?"
  (if in-place
      (setf (transform this) (sb-cga:transpose-matrix (transform this)))
      (sb-cga:transpose-matrix (transform this))))


(defmethod invert ((this node) &optional (in-place t))
  "Inherited function for setting changed?"
  (if in-place
      (setf (transform this) (sb-cga:inverse-matrix (transform this)))
      (sb-cga:inverse-matrix (transform this))))


(defmethod scale ((this node) x y z &optional (in-place t))
  "Inherited function for setting changed?"
  (if in-place
      (setf (transform this)
	    (sb-cga:matrix*
	     (sb-cga:scale* (float x)
			    (float y)
			    (float z))
	     (transform this)))
      (sb-cga:matrix* (sb-cga:scale* (float x)
				     (float y)
				     (float z))
		      (transform this))))

(defmethod translate ((this node) x y z &optional (in-place t))
  "Inherited function for setting changed?"
  (if in-place
      (setf (transform this) (sb-cga:matrix* (sb-cga:translate* (float x)
								(float y)
								(float z))
					     (transform this)))
      (sb-cga:matrix* (sb-cga:translate* (float x)
					 (float y)
					 (float z))
		      (transform this))))

(defmethod rotate ((this node) rad x y z &optional (in-place t))
  "Inherited function for setting changed?"
  (if in-place
      (setf (transform this)
	    (sb-cga:matrix* (sb-cga:rotate-around
			     (make-vector (float x)
					  (float y)
					  (float z)) (float rad))
			    (transform this)))
      (sb-cga:matrix* (sb-cga:rotate-around
		       (make-vector (float x)
				    (float y)
				    (float z)) (float rad))
		      (transform this))))


(defmethod load-matrix ((this node) &key)

  (gl:load-matrix (or (current-transform this)
		      (transform this))))

(defmethod unload ((this node) &key)
  "Release node resources."
  (setf (enabled this) nil)
  (loop for i in (children this)
       do (when (typep i 'refcount)
	    (unref i))))

(defmacro node (&body args)

  (multiple-value-bind (keys children) (split-keywords args)
    
    `(let ((*parent* (make-instance 'node ,@keys :parent *parent*)))
       ,@children
       *parent*)))


