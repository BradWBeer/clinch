(in-package :clinch)


(defclass skeleton ()
  ((bones-array
    :accessor bones
    :initform nil
    :initarg :skeleton)
   (bone-offsets
    :accessor bone-offsets
    :initform nil
    :initarg :offsets)
   (weights
    :accessor weights
    :initform nil
    :initarg :weights)
   (bone-buffer
    :accessor bone-buffer
    :initform nil
    :initarg :bone-buffer)
   (boneID-buffer :reader bone-buffer
		  :initform nil
		  :initarg :bone-buffer)
   (weights-buffer
    :accessor weights-buffer
    :initform nil
    :initarg :weights-buffer)))

(defmethod initialize-instance ((this skeleton) &key mesh node-hash)

  (with-accessors ((bb bone-buffer)
		   (IDs boneID-buffer)
		   (weights weights-buffer)) this

    (when (and mesh node-hash)
      
      (multiple-value-bind (offsets vertices) (make-skeleton mesh node-hash)
	(let ((offset-length (length offsets))
	      (v-length (length vertices)))
	  
    ;; (! (setf bb (make-instance 'clinch:buffer 
    ;; 			     :count offset-length
    ;; 			     :stride 16
    ;; 			     :data (make-array (* offset-length 16) :initial-element 0.0 :element-type :float)))

    ;;    (setf bb (make-instance 'clinch:buffer 
    ;; 			     :count 12
    ;; 			     :stride 5
    ;; 			     :data (make-array (* offset-length 16) :initial-element 0.0 :element-type :int)))
       ))))))

  (defun sort-vertex-weights (weights)
    (sort weights (lambda (a b)
		    (> (cdr a) (cdr b)))))

  (defun sort-all-vertex-weights (weights)
    (loop for x from 0 below (length weights)
       do (setf (elt weights x) (sort-vertex-weights (elt weights x))))
    weights)

  (defmethod make-skeleton ((mesh classimp:mesh) (node-hash hash-table))
    (let* ((vertex-count (length (classimp:vertices mesh)))
	   (vertices (make-array vertex-count :initial-element nil))
	   (classimp-bones (classimp:bones mesh))
	   (bone-count (length classimp-bones))
	   (bones (make-array bone-count)))

      (loop for x from 0 below bone-count
	 for cb = (elt classimp-bones x)
	 for b = (gethash (classimp:name cb) node-hash)
	 do (progn 
	      (setf (elt bones x) (cons b (classimp:offset-matrix cb)))
	      
	      (loop with weights = (classimp:weights cb)
		 for weight across weights
		 do (push (cons x
				(classimp:weight weight)) 
			  (elt vertices (classimp:id weight))))))
      (values bones
	      (sort-all-vertex-weights vertices))))


;; Need to make this work with array instead of list
(defun fill-two-arrays-by-chunks (arr stride)
  (let ((arr1 (make-array (* (length arr) stride) :initial-element 0 :element-type 'integer))
	(arr2 (make-array (* (length arr) stride) :initial-element 0.0 :element-type 'float)))

    (loop
       for x from 0 below (length arr1) by stride
       for tuple across arr
       do (loop 
	     for y from 0 below stride
	     for value in tuple 
	     do (setf (elt arr1 (+ x y)) (car value)
		      (elt arr2 (+ x y)) (cdr value))))
    (values arr1 arr2)))


  (defmethod generate-buffers ((this skeleton) &key)
    )

  (defmethod generate-bone-buffer ((this skeleton) &key)

    )


  ;; (loop with arr = (make-array (* (length tmp) 5) :element-type :int :initial-element 0)
  ;; 	      for x from 0 below 
