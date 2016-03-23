;;;; bone.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defclass bone-animation ()
  ((bone :accessor bone
	 :initform nil
	 :initarg :bone)
   (positions :accessor positions
	      :initform nil
	      :initarg :positions)
   (rotations :accessor rotations
	      :initform nil
	      :initarg :rotations)
   (scaling :accessor scaling
	    :initform nil
	    :initarg :scaling)))

(defclass bone ()
  ((node :reader node
	 :initform nil
	 :initarg :node)
   (offset-matrix :accessor matrix
		  :initform (m4:identity)
		  :initarg :matrix)
   (weights :accessor weights
	    :initform nil
	    :initarg :weights)))


(defmethod translate-weights ((bone classimp:bone))
  (map 'list (lambda (x) (cons (classimp:id x) (classimp:weight x))) (classimp:weights bone)))

(defgeneric translate-classimp-bone (bone node-hash))
(defmethod translate-classimp-bone ((bone classimp:bone) (node-hash hash-table))
  (make-instance 'clinch::bone 
		 :node (gethash (classimp:name bone) node-hash)
		 :matrix (classimp:offset-matrix bone)
		 :weights (clinch::translate-weights bone)))

(defmethod bone-weights-into-array ((bone clinch::bone) (arr simple-array))
  (loop for (index . w) in (clinch::weights bone)
     do (push (cons bone w) (elt arr index))))

(defmethod translate-bones ((mesh classimp:mesh) (node-hash hash-table))
  
  (let ((weights (make-array (length (classimp:faces mesh)) :element-type 'list :initial-element nil))
	(bone-hash (make-hash-table :test 'equal))
	(bones (map 'list (lambda (b)
			    
			    (translate-classimp-bone b node-hash))
		    (classimp:bones mesh))))
    
    (map nil (lambda (x)
	       (setf (gethash (name (node x)) bone-hash) x)
	       (bone-weights-into-array x weights))
	 bones)

    (values bone-hash weights)))
