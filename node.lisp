;;;; node.lisp

(in-package #:clinch)

(defclass node (transform)
  ((children
    :initform nil
    :initarg  :children
    :accessor children)
   (current-transform
    :accessor current-transform
    :initform nil)))
    
(defmethod initialize-instance :after ((this node) &key parent)
  (when parent
    (add-child parent this)))

(defmethod print-object ((this node) s)
  (format s "#<NODE children: ~A ~S~%{~{~,4f~t~,4f~t~,4f~t~,4f~^~% ~}}>" (length (children this)) (qtype this) (transform->list (transform this))))

(defmethod changed? ((this node))
  (null (current-transform this)))

(defmethod (setf changed?) (val (this node))
  (setf (current-transform this) (when val nil)))

(defmethod add-child ((this node) child &key)
  (with-accessors ((children children)) this
    (unless (member child children)
      (setf children
	    (cons child children)))))

(defmethod update ((this node) &optional parent &key matrix force)
  (when (or force (changed? this))
    (setf (current-transform this)
  	  (if parent
  	      (m*  (or matrix (current-transform parent)) this)
  	      this)))
  (loop for child in (children this)
       do (update child this :force force))
  
  (current-transform this))
		     
(defmethod render ((this node) &key parent matrix)
  (when (changed? this)
    (update this parent :matrix matrix))
  
  (loop for i in (children this)
     do (render i :parent this :matrix (current-transform this))))

(defmethod render ((this list) &key parent matrix)
  (loop for i in this
       do (render i :parent parent :matrix matrix)))

    
(defmethod (setf node) :after ((other-transform transform) (this node))
  "Inherited function for setting changed?"
  (setf (changed? this) t))

(defmethod set-identity-transform :after ((this node) &key)
  "Inherited function for setting changed?"
  (setf (changed? this) t))

(defmethod m* :after ((this node) (that transform) &optional (in-place t))
  "Inherited function for setting changed?"
  (when in-place
    (setf (changed? this) t)))

(defmethod transpose :after ((this node) &optional (in-place t))
  "Inherited function for setting changed?"
  (when in-place
    (setf (changed? this) t)))

(defmethod invert :after ((this node) &optional (in-place t))
  "Inherited function for setting changed?"
  (when in-place
    (setf (changed? this) t)))

(defmethod scale :after ((this node) x y z &optional (in-place t))
  "Inherited function for setting changed?"
  (when in-place
    (setf (changed? this) t)))

(defmethod translate :after ((this node) x y z &optional (in-place t))
  "Inherited function for setting changed?"
  (when in-place
    (setf (changed? this) t)))

(defmethod rotate :after ((this node) angle x y z &optional (in-place t))
  "Inherited function for setting changed?"
  (when in-place
    (setf (changed? this) t)))














  


