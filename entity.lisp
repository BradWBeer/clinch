;;;; entity.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defclass entity ()
  ((use-gl-stack
    :initform t
    :initarg :use-gl-stack?
    :reader use-gl-stack?)
   (shader
    :initform nil
    :initarg :shader
    :accessor shader)
   (indexes
    :initform nil
    :initarg :indexes
    :accessor indexes)
   (render-values
    :initform nil
    :initarg :values
    :accessor render-values)
   (func)))

(defmethod initialize-instance :after ((this entity) &key (compile t) parent)
  (when parent (add-child parent this))
  (when compile (make-render-func this)))

(defmethod print-object ((this entity) s)
  (format s "#<entity>"))

(defmethod make-render-func ((this entity))
  (setf (slot-value this 'func)
	(eval `(lambda (&key parent-transform projection-transform)
		 (declare (optimize (speed 3)))
		 (gl:matrix-mode :modelview)
		 (use-shader ,(shader this))
		 ,@(loop
		      with tex-unit = 0
		      for (atr-or-uni name value) in (render-values this)
		      collect (cond ((eql atr-or-uni :uniform) `(attach-uniform ,(shader this) ,name ,@value))
				    ((and (eql atr-or-uni :attribute)
					  (typep value 'texture)) (prog1 `(bind-sampler ,value ,(shader this) ,name ,tex-unit) (incf tex-unit)))
				    ((and (eql atr-or-uni :attribute)
					  (typep value 'buffer)) 
				     `(bind-buffer-to-attribute-array ,value ,(shader this) ,name))
				    ((eql atr-or-uni :attribute) `(bind-static-values-to-attribute ,(shader this) ,name ,@value))
				    ((eql atr-or-uni :vertices) 
				     `(bind-buffer-to-vertex-array ,name))
				    ((eql atr-or-uni :normals) 
				    `(bind-buffer-to-normal-array ,name))))
		 
		 (draw-with-index-buffer ,(indexes this))))))

(defmethod update ((this entity) &key parent matrix force)
  )

(defmethod render ((this entity) &key parent matrix projection)
  (when (and (use-gl-stack? this)
	     (or parent matrix))
    (gl:matrix-mode :modelview)
    (gl:load-matrix (or matrix
			(current-transform parent)
			(transform parent))))
      
  
  (funcall (slot-value this 'func) :parent-transform (or matrix parent) :projection-transform projection))

(defmethod slow-render ((this entity))
  (gl:matrix-mode :modelview)
  ;;(use-transform this)
  (use-shader (shader this))
  (loop
     with tex-unit = 0
     for (atr-or-uni name value) in (render-values this)
     do (cond ((eql atr-or-uni :uniform) (apply #'attach-uniform (shader this) name value))
	      ((and (eql atr-or-uni :attribute)
		    (typep value 'texture)) (bind-sampler value (shader this) name tex-unit) (incf tex-unit))
	      ((and (eql atr-or-uni :attribute)
		    (typep value 'buffer)) 
	       (bind-buffer-to-attribute-array value (shader this) name))
	      ((eql atr-or-uni :attribute) (apply #'bind-static-values-to-attribute (shader this) name value))
	      ((eql atr-or-uni :vertices) 
	       (bind-buffer-to-vertex-array name))))

  (draw-with-index-buffer (indexes this)))
