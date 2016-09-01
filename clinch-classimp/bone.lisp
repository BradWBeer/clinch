;;;; bone.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

;; (defclass bone-animation ()
;;   ((bone :accessor bone
;; 	 :initform nil
;; 	 :initarg :bone)
;;    (positions :accessor positions
;; 	      :initform nil
;; 	      :initarg :positions)
;;    (rotations :accessor rotations
;; 	      :initform nil
;; 	      :initarg :rotations)
;;    (scaling :accessor scaling
;; 	    :initform nil
;; 	    :initarg :scaling)))

;; (defclass bone ()
;;   ((node :reader node
;; 	 :initform nil
;; 	 :initarg :node)
;;    (offset-matrix :accessor matrix
;; 		  :initform (m4:identity)
;; 		  :initarg :matrix)
;;    (weights :accessor weights
;; 	    :initform nil
;; 	    :initarg :weights)))

(defclass bone (node) 
  ((name :initform nil
       :initarg :name
       :reader name)
   (id :initform (error "Bones must have a unique id for the gl-buffer!")
       :initarg :id
       :reader id)
   (gl-buffer :initform nil
	      :initarg :buffer
	      :accessor buffer)
   (offset-matrix :initform (m4:identity)
		  :initarg :offset-matrix
		  :accessor offset-matrix)

   ;; I'm not sure I need this but I'll put it here for now.
   (weights :initform nil
	    :initarg :weights
	    :accessor weights)))

(defmethod weights-to-alist (bone)
	   (loop for i across (weights bone)
	      collect (cons (id i) (weight i))))

;; Subclass method transform to add offset matrix
;; and do animations?


(defclass bone-animation () 
  ((bone :initform (error "Bone animations must have a bone attached!")
	 :initarg :bone
	 :accessor bone)
   (position-frames :initform nil
		    :initarg :position-frames
		    :accessor position-frames)
   (rotation-frames :initform nil
		    :initarg :rotation-frames
		    :accessor rotation-frames)
   (scale-frames :initform nil
		 :initarg :scale-frames
		 :accessor scale-frames)))

(defun key-frames-to-list (frames)
  (map 'list (lambda (k)
	       (cons (slot-value k 'time)
		     (value k)))
       frames))

;; (defmethod translate-weights ((bone classimp:bone))
;;   (map 'list (lambda (x) (cons (classimp:id x) (classimp:weight x))) (classimp:weights bone)))

;; (defgeneric translate-classimp-bone (bone node-hash))
;; (defmethod translate-classimp-bone ((bone classimp:bone) (node-hash hash-table))
;;   (make-instance 'clinch::bone 
;; 		 :node (gethash (classimp:name bone) node-hash)
;; 		 :matrix (classimp:offset-matrix bone)
;; 		 :weights (clinch::translate-weights bone)))

;; (defmethod bone-weights-into-array ((bone clinch::bone) (arr simple-array))
;;   (loop for (index . w) in (clinch::weights bone)
;;      do (push (cons bone w) (elt arr index))))

;; (defmethod translate-bones ((mesh classimp:mesh) (node-hash hash-table))
  
;;   (let ((weights (make-array (length (classimp:faces mesh)) :element-type 'list :initial-element nil))
;; 	(bone-hash (make-hash-table :test 'equal))
;; 	(bones (map 'list (lambda (b)
			    
;; 			    (translate-classimp-bone b node-hash))
;; 		    (classimp:bones mesh))))
    
;;     (map nil (lambda (x)
;; 	       (setf (gethash (name (node x)) bone-hash) x)
;; 	       (bone-weights-into-array x weights))
;; 	 bones)

;;     (values bone-hash weights)))
