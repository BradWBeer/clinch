;;;; entity.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defclass entity ()
  ((use-gl-stack
    :initform t
    :initarg :use-gl-stack?
    :reader use-gl-stack?)
   (VAO
     :initform nil
     :reader VAO)
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

(defmethod get-render-value ((this entity) name)
  (or (second
       (assoc name
	      (clinch::render-values this)))
      (loop for i in (clinch::render-values this)
	   if (and (>= 3 (length i))
		   (equal name (second i)))
	   do (return (third i)))))

;; (defmethod get-primitive ((this entity) name)
;;   (let* ((buff      (get-render-value this name))
;; 	 (stride    (stride buff))
;; 	 (icount    (vertex-count (indexes this)))
;; 	 (itype     (qtype (indexes this)))
;; 	 (btype      (clinch:qtype buff)))

;;     (clinch:with-mapped-buffer (iptr (indexes this) :read-only)
;;       (clinch:with-mapped-buffer (bptr buff :read-only)
;; 	(print (/ icount 3))
;; 	(loop
;; 	   for i from 0 to (1- (/ icount 3))
;; 	   collect (loop for j from 0 to 2
;; 		      collect (loop
;; 				 with ret = (make-array stride :element-type 'single-float)
;; 				 for k from 0 to (1- stride)
;; 				 do (setf (elt ret k)
;; 					  (cffi:mem-aref bptr btype
;; 							 (+ k (* (cffi:mem-aref iptr itype (+ (* i stride) j)) stride))))
;; 				 finally (return ret))))))))

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
	     
	     
(defun rec (primitives i distance u v index)
  )
				   
(defmethod triangle-intersection? ((this entity) start dir &key (vertex-name :vertices))
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
			
		   
(defmethod make-render-func ((this entity) &key)
  (setf (slot-value this 'func)
	(eval `(lambda (&key parent-transform projection-transform)
		 (declare (optimize (speed 3)))
		 (gl:matrix-mode :modelview)
		 (when ,(shader this)
		   (use-shader ,(shader this)))
		 ,@(loop
		      with tex-unit = 0
		      for (atr-or-uni name value) in (render-values this)
		      collect (cond ((and (eql atr-or-uni :uniform)
					  (typep value 'texture)) (prog1 `(bind-sampler ,value ,(shader this) ,name ,tex-unit) (incf tex-unit)))
				    ((eql atr-or-uni :uniform) (if (atom value)
								   `(attach-uniform ,(shader this) ,name ,value)
								   `(attach-uniform ,(shader this) ,name ,@value)))

				    ((and (eql atr-or-uni :attribute)
					  (typep value 'buffer)) 
				     `(bind-buffer-to-attribute-array ,value ,(shader this) ,name))
				    ((eql atr-or-uni :attribute) (if (atom value)
								     `(bind-static-values-to-attribute ,(shader this) ,name ,value)
								     `(bind-static-values-to-attribute ,(shader this) ,name ,@value)))
				    ((eql atr-or-uni :vertices) 
				     `(bind-buffer-to-vertex-array ,name))
				    ((eql atr-or-uni :normals) 
				    `(bind-buffer-to-normal-array ,name))))
		 
		 (draw-with-index-buffer ,(indexes this))))))

;; (defmethod make-VAO-render-func ((this entity) &key)
;;   (gl:bind-vertex-array
;;    (setf (slot-value this 'VAO)
;; 	 (car (gl:gen-vertex-arrays 1))))

;;   (loop
;;      )

;;   (setf (slot-value this 'func)
;; 	(eval `(lambda (&key parent-transform projection-transform)
;; 		 (declare (optimize (speed 3)))
;; 		 (gl:matrix-mode :modelview)
;; 		 (use-shader ,(shader this))
;; 		 ,@(loop
;; 		      with tex-unit = 0
;; 		      for (atr-or-uni name value) in (render-values this)
;; 		      collect (cond ((eql atr-or-uni :uniform) `(attach-uniform ,(shader this) ,name ,@value))
;; 				    ((and (eql atr-or-uni :attribute)
;; 					  (typep value 'texture)) (prog1 `(bind-sampler ,value ,(shader this) ,name ,tex-unit) (incf tex-unit)))
;; 				    ((and (eql atr-or-uni :attribute)
;; 					  (typep value 'buffer)) 
;; 				     `(bind-buffer-to-attribute-array ,value ,(shader this) ,name))
;; 				    ((eql atr-or-uni :attribute) `(bind-static-values-to-attribute ,(shader this) ,name ,@value))
;; 				    ((eql atr-or-uni :vertices) 
;; 				     `(bind-buffer-to-vertex-array ,name))
;; 				    ((eql atr-or-uni :normals) 
;; 				    `(bind-buffer-to-normal-array ,name))))
		 
;; 		 (draw-with-index-buffer ,(indexes this))))))


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
  (when (shader this)
    (use-shader (shader this)))
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
