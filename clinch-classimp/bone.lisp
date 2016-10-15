;;; bone.lisp
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
  ((id :accessor id
       :initform nil
       :initarg :id)
   (offset :initform (m4:identity)
	   :initarg :offset
	   :accessor offset)
   (weights :initform nil
	    :initarg :weights
	    :accessor weights)))


(defmethod transform ((this bone) &key)
  "Gets the transform matrix."
  (with-slots ((scale s-matrix)
	       (trans t-matrix)
	       (rot   r-matrix)
	       (transform transform)
	       (offset offset)) this

    (if (and scale rot trans transform)
	(m4:* offset transform)
	(setf transform
	      (reduce #'m:* (list (or offset (offset this))
				  (or trans (translation-matrix this))
				  (or rot   (rotation-matrix this))
				  (or scale (scale-matrix this))))))))

(defmethod print-object ((this bone) s)
  "Print function for node."
  (with-accessors ((name name)) this
    (format s "#<BONE ~A children:~A ~A>"
	    (cond
	      ((stringp name) (concatenate 'string "\"" name "\""))
	      (name name)
	      (t "<unnamed>"))
	    (length (children this))
	    (transform this))))



(defmethod make-bone ((this classimp:node) &key bone-hash node-name-hash entities bone-count)

  (let* ((id bone-count)
	 (current-id id)
	 (children (append (map 'list 
				(lambda (n) (multiple-value-bind (node id) 
						(get-nodes n
							   :bone-hash bone-hash
							   :node-name-hash node-name-hash
							   :entities entities
							   :bone-count current-id)
					      ;;(format t "id2 = ~A (~A)~%" id current-id)
					      (setf current-id id)
					      node))					      
				(map 'list (lambda (e)
					     (nth e entities))
					     (classimp:children this)))
			   (coerce (classimp:meshes this) 'list)))
	 (bone (gethash (classimp:name this) bone-hash))
	 (ret (make-instance 'bone 
			     :id current-id
			     :name (classimp:name this)
			     :offset (classimp:offset-matrix bone)
			     :weights (get-bone-weights (gethash (classimp:name this) bone-hash))
			     :children children)))

    (when node-name-hash 
      (setf (gethash (classimp:name this) node-name-hash) ret))
    (values ret current-id)))

(defmethod get-mesh-bone-hash ((this classimp:scene) &key )

  (let* ((meshes (coerce (classimp:meshes this) 'list))
	 (root (classimp:root-node this))
	 (ret (make-hash-table :test 'equal)))

    (map 'list (lambda (m) 
		 (map 'list (lambda (b) 
			      (setf (gethash (classimp:name b) ret) b))
		      (classimp:bones m)))
	 meshes)
    ret))

(defmethod get-bone-weights ((this classimp:bone))
  (map 'list (lambda (b)
		 (cons (classimp:id b)
		       (classimp:weight b)))
       (classimp:weights this)))


;; (defun remove-non-bones (lst)
;;   (loop for i in lst
;;      when (typep i 'bone)
;;      collect i))

;; (defmethod number-bones ((this node) &key)
;;   (loop for n in (remove-non-bones
;; 		  (topological-sort (node-top-map this) :test 'eq))
;;      for x from 0
;;      collect (cons x n)))


;; (defmethod weights-to-alist (bone)
;;   (loop for i across (weights bone)
;;      collect (cons (id i) (weight i))))

;; (defmethod get-mesh-bone-names ((this classimp:mesh))
;;   (map 'list #'classimp:name (classimp:bones this)))

;; (defmethod get-all-mesh-bone-names ((this classimp:scene))
;;   (loop for m in (coerce (classimp:meshes this) 'list)
;;      append (get-mesh-bone-names m)))

;; ;; Subclass method transform to add offset matrix
;; ;; and do animations?

;; (defclass bone-animation () 
;;   ((bone :initform (error "Bone animations must have a bone attached!")
;; 	 :initarg :bone
;; 	 :accessor bone)
;;    (position-frames :initform nil
;; 		    :initarg :position-frames
;; 		    :accessor position-frames)
;;    (rotation-frames :initform nil
;; 		    :initarg :rotation-frames
;; 		    :accessor rotation-frames)
;;    (scale-frames :initform nil
;; 		 :initarg :scale-frames
;; 		 :accessor scale-frames)))


;; (defun weight->cell (weight)
;;   (cons (classimp:id weight)
;; 	(classimp:weight weight)))

;; (defun get-bone-weights (bone)
;;   (loop for x below (length (classimp:weights bone))
;;      collect (weight->cell (elt (classimp:weights bone) x))))

;; (defun get-mesh-bones (mesh &optional (bone-hash (make-hash-table :test 'equal)))
;;   (let ((bones (classimp:bones mesh)))
;;     (loop for x from 0 below (length bones)
;;        do (let ((bone (elt bones x)))
;; 	    (setf (gethash (classimp:name bone) bone-hash)
;; 		  (cons x
;; 			(cons (classimp:offset-matrix bone)
;; 			      (get-bone-weights bone)))))))
;;   bone-hash)



;; (defun get-all-bones (scene)
;;   (let ((bone-hash (make-hash-table :test 'equal)))
;;     (map 'nil 
;; 	 (lambda (i)
;; 	   (get-mesh-bones i bone-hash))
;; 	 (classimp:meshes scene))
;;     bone-hash))


;; (defun key-frames-to-list (frames)
;;   (map 'list (lambda (k)
;; 	       (cons (slot-value k 'time)
;; 		     (value k)))
;;        frames))

;; ;; (defmethod translate-weights ((bone classimp:bone))
;; ;;   (map 'list (lambda (x) (cons (classimp:id x) (classimp:weight x))) (classimp:weights bone)))

;; ;; (defgeneric translate-classimp-bone (bone node-hash))
;; ;; (defmethod translate-classimp-bone ((bone classimp:bone) (node-hash hash-table))
;; ;;   (make-instance 'clinch::bone 
;; ;; 		 :node (gethash (classimp:name bone) node-hash)
;; ;; 		 :matrix (classimp:offset-matrix bone)
;; ;; 		 :weights (clinch::translate-weights bone)))

;; ;; (defmethod bone-weights-into-array ((bone clinch::bone) (arr simple-array))
;; ;;   (loop for (index . w) in (clinch::weights bone)
;; ;;      do (push (cons bone w) (elt arr index))))

;; ;; (defmethod translate-bones ((mesh classimp:mesh) (node-hash hash-table))

;; ;;   (let ((weights (make-array (length (classimp:faces mesh)) :element-type 'list :initial-element nil))
;; ;; 	(bone-hash (make-hash-table :test 'equal))
;; ;; 	(bones (map 'list (lambda (b)

;; ;; 			    (translate-classimp-bone b node-hash))
;; ;; 		    (classimp:bones mesh))))

;; ;;     (map nil (lambda (x)
;; ;; 	       (setf (gethash (name (node x)) bone-hash) x)
;; ;; 	       (bone-weights-into-array x weights))
;; ;; 	 bones)

;; ;;     (values bone-hash weights)))


;; (defmethod find-top-level-bone ((this node) channels) 
;;   (with-accessors ((children children)) this
;;     (when children 
;;       (if (gethash (name this) channels)
;; 	  (dolist (n children) 
;; 	    (let ((ret (find-top-level-bones n)))
;; 	      (when ret (return ret))))))))


;; (defmethod find-top-level-bones ((this node)) 
;;   (with-accessors ((children children)) this
;;     (when children 
;;       (if (some (lambda (n) 
;; 		  (typep n 'clinch::bone))
;; 		children)
;; 	  (make-instance 'clinch:node 
;; 			 :parent nil 
;; 			 :children (loop for i in children
;; 				      when (typep i 'clinch::bone)
;; 				      collect i))
;; 	  (dolist (n children) 
;; 	    (let ((ret (find-top-level-bones n)))
;; 	      (when ret (return ret))))))))


;; (defmethod debone ((this node))
;;   (with-accessors ((children children)) this
;;     (when children
;;       (setf children
;; 	    (remove-if (lambda (n)
;; 			 (typep n 'clinch::bone))
;; 		       children))
;;       (map nil (lambda (n)
;; 		 (when (typep n 'node)
;; 		   (debone n)))
;; 	   children))))
