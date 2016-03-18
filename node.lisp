;;;; node.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defclass node ()
  ((name :accessor name
	 :initform nil
	 :initarg :name)
   (trans :reader translation
	  :initform (v! 0 0 0)
	  :initarg  :translation)
   (rot :reader rotation
	:initform (q:identity)
	:initarg :rotation)
   (scale    :reader scaling
	     :initform (v! 1 1 1)
	     :initarg  :scale)
   (t-matrix :initform nil)
   (r-matrix :initform nil)
   (s-matrix :initform nil)
   (transform :initform nil)
   (current-transform :initform nil
		      :reader current-transform)
   (enabled
    :accessor enabled
    :initform t
    :initarg :enabled)
   (children :accessor children
	     :initform nil
	     :initarg  :children))
  (:documentation "A node class for creating hierarchies of objects. It caches calculations for speed. Not enough in itself, and is not required by Clinch."))

(defgeneric !reset (this))
(defgeneric (setf rotation) (val this))
(defgeneric (setf scaling) (val this))
(defgeneric (setf translation) (val this))
(defgeneric translation-matrix (this &key))
(defgeneric rotation-matrix (this &key))
(defgeneric scale-matrix (this &key))
(defgeneric transform (this &key))
(defgeneric translate (this trans &key))
(defgeneric rotate (this rot &key))
(defgeneric scale (this scale &key))
(defgeneric n* (this that &key))

(defmethod initialize-instance :after ((this node) &key translation rotation scale copy matrix)
  "Creats a node with optional translation (vector3), rotation (quaterion) and translation (vector3). Can also use another node or matrix to set its values. If a node and another value is give, the other value is used starting with the matrix."
  ;;; add decompose-transform here !!!
  (setf (translation this) (or translation
			       (and copy (copy-seq (translation copy)))
			       (v! 0 0 0)))
  (setf (rotation this) (or rotation
			    (and copy (copy-seq (rotation copy)))
			    (v! 1 0 0 0)))
  (setf (scaling this) (or scale
			 (and copy (copy-seq (scaling copy)))
			 (v! 1 1 1))))

(defmethod !reset ((this node))
  "Resets the node."
  (setf (translation this) (v! 0 0 0))
  (setf (rotation this)    (v! 1 0 0 0))
  (setf (scaling this)     (v! 1 1 1))
  (setf (slot-value this 't-matrix) nil)
  (setf (slot-value this 'r-matrix) nil)
  (setf (slot-value this 's-matrix) nil)
  (setf (slot-value this 's-matrix) nil)
  (setf (slot-value this 'transform) nil))

(defmethod !0 ((this node))
  (!reset this))
  
(defmethod print-object ((this node) s)
  "Print function for node."
  (format s "#<NODE children: ~A ~%~A>" (length (children this)) (transform this)))

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
  "Removes a child."
  (with-accessors ((children children)) this
    
    (when (member child children)
     
      (setf children
	    (remove child children)))))

(defmethod render ((this node) &key parent projection)
  "Render child objects. You don't need to build your application with nodes/render. This is just here to help."
  (when (enabled this)

    ;; (if parent 
    ;;   (print (type-of parent))
    ;;   (print "NO PARENT!"))

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
	 do (render i :parent current-transform :projection projection)))))

(defmethod render ((this list) &key parent projection)
  "Render a list of rendables."
  (when (enabled this)

    (let ((current-transform
	   (cond ((and parent (typep parent 'node)) (m:* (transform this) (transform parent)))
		 ((and parent (arrayp parent)) (m:* (transform this) parent)))))
      
      (loop for i in this
	 do (progn 
	      (when (arrayp i) (setf current-transform (m:* i current-transform)))
	      (render i :parent current-transform :projection projection))))))

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

(defmethod transform ((this node) &key)
  "Gets the transform matrix."
  (with-slots ((scale s-matrix)
	       (trans t-matrix)
	       (rot   r-matrix)
	       (transform transform)) this

    (if (and scale rot trans transform)
	transform
	(setf transform
	      (reduce #'m:* (list (or trans (translation-matrix this))
				    (or rot   (rotation-matrix this))
				    (or scale (scale-matrix this))))))))

(defmethod rotate ((this node) rot &key (modify t))
  "Rotate the node. Takes a quaterion."
  (if modify 
      (setf (rotation this) 
	    (q:* rot (rotation this)))
      (q:* rot (rotation this))))

(defmethod translate ((this node) trans &key (modify t))
  "Translate the node. Takes a vector3."
  (if modify 
      (setf (translation this) 
	    (v:+ trans (translation this)))
      (v:+ trans (translation this))))

(defmethod scale ((this node) size &key (modify t))
  "Scales a node. Takes a vector3."
  (if modify 
      (setf (scaling this) 
	    (v:* size (scaling this)))
      (v:* size (scaling this))))

(defmethod n* ((this node) (that node) &key new-node)
  "Multiplies a node? I'm not sure if this works." ;;!!!!
  (if new-node
      (make-instance 'node
		     :scale (v:* (scaling this) (scaling that))
		     :rotation (q:* (rotation this) (rotation that))
		     :translation (v:+ (translation this) (translation that)))
      (m:* (transform this)
	     (transform that))))

(defmethod inverse ((this node))
  (m4:affine-inverse (transform this)))


(defmethod traverse-node ((this node))
  (loop for c in (children this)
     when c
     append (cond 
	      ((typep c 'node) (traverse-node c))
	      (t (list c)))))
