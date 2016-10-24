(in-package :clinch)

(defconstant +MAX-NUMER-OF-BONES+ 100)
(defconstant +MAX-NUMER-OF-BONE/VERTEX+ 5)

(defclass skeleton ()
  ((bones
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
   (bone-id-buffer
    :accessor bone-id-buffer
    :initform nil
    :initarg :bone-id-buffer)
   (weights-buffer
    :accessor weights-buffer
    :initform nil
    :initarg :weights-buffer)))

(defmethod initialize-instance ((this skeleton) &key mesh node-hash)

  (with-accessors ((IDs bones)
		   (weights weights)
		   (bb bone-buffer)
		   (bib bone-id-buffer)
		   (w weights-buffer)
		   (bo bone-offsets)) this

    (when (and mesh node-hash)

      (setf bo
	    (map 'list (lambda (bone)
			 (classimp:offset-matrix bone))
		 (classimp:bones mesh)))

      (multiple-value-bind (tmp-ids tmp-weights) (make-skeleton mesh node-hash)


	(setf IDs (coerce tmp-ids 'list)
	      weights tmp-weights)

	(multiple-value-bind (vIdArr vWeightsArr) (fill-two-arrays-by-chunks tmp-weights 5)
	  
	  (let ((num-vertices (/ (length vIdArr) 5))
		(num-bones (length (classimp:bones mesh))))
	    
	    (!
	      (setf bb (make-instance 'clinch:buffer 
	      			     :count num-bones
				     :qtype :float
	      			     :stride 16))
	      
	      (setf bib (make-instance 'clinch:buffer 
				       :count num-vertices
				       :stride +MAX-NUMER-OF-BONE/VERTEX+
				       :qtype :unsigned-int
				       :data vIdArr)))

	    (setf w (make-instance 'clinch:buffer 
				   :count num-vertices
				   :stride +MAX-NUMER-OF-BONE/VERTEX+
				   :qtype :float
				   :data vWeightsArr)))

	  )))))

(defmethod update ((this skeleton) &key)
  (with-mapped-buffer (buf (clinch::bone-buffer this) :write-only)
    (loop
       for x from 0 by 16
       for b in (clinch::bones this)
       for o in (clinch::bone-offsets this)
       do (loop
	     for y from 0 below 16
	     for m across (clinch:n* b o)
	     do (setf (cffi:mem-aref buf :float (+ x y)) m)))))


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
	    (setf (elt bones x) b)
	    
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
