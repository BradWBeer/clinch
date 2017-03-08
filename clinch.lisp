;;;; clinch.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

;; global variables. Not exported. 
(defparameter *window* nil
  "Global window object.")
(defparameter *context* nil
  "Global Opengl Contex object.")

(defparameter *inited* nil)
(defparameter *running* nil)

(defparameter *fbo* nil
  "Current Frame Buffer Object.")

(defparameter *viewport* nil
  "Current viewport object.")

(defparameter *projection* nil
  "Current default projection.")

(defparameter *ortho-projection* nil
  "An automatically generated orthogonal projection for the window. Pixel to pixel.")

(defparameter *texture* nil
  "The current texture. Defaults to the window's overlay.")

(defparameter *entity* nil
  "The current entity")

;; not currently using these
(defparameter *current-shader-attributes*
  #+(or ccl ecl) (make-hash-table :test 'eq)
  #-(or ccl ecl) (trivial-garbage:make-weak-hash-table :test 'eq))

(defparameter *current-shader-uniforms*
  #+(or ccl ecl) (make-hash-table :test 'eq)
  #-(or ccl ecl) (trivial-garbage:make-weak-hash-table :test 'eq))

(defparameter *shaders->shader-programs* (make-hash-table))

(defparameter *uncollected*
  #+(or ccl ecl) (make-hash-table :test 'eq)
  #-(or ccl ecl) (trivial-garbage:make-weak-hash-table :weakness :key-or-value)
  "Weak hash of loaded OpenGL objects.")

(defparameter *dependents*  
  #+(or ccl ecl) (make-hash-table :test 'eq)
  #-(or ccl ecl) (trivial-garbage:make-weak-hash-table :weakness :key-or-value)
  "Weak hash of OpenGL objects waiting to be unloaded by another.")

(defmacro ! (&body body)
  "Runs body in main thread for safe OpenGL calls. Waits for return value."
  `(sdl2:in-main-thread ()
     ,@body))

(defmacro !! (&body body)
  "Runs body in main thread for safe OpenGL calls. Returns immediately."
  `(sdl2:in-main-thread (:background t)
     ,@body))

(defgeneric unload (this &key) 
  (:documentation "Unloads an opengl object. Does nothing for non-opengl objects."))

(defmethod unload ((this t) &key))

(defun add-uncollected (this)
  "Adds item to list of loaded OpenGL objects."
  (setf (gethash (key this) *uncollected*) this))

(defun remove-uncollected (this)
  "Removes item from list of loaded OpenGL objects. Does NOT call unload."
  (remhash (key this) *uncollected*))

(defun unload-all-uncollected ()
  "Unloads all loaded OpenGL objects."
  (loop for key being the hash-keys of *uncollected*
     do (unload (gethash key *uncollected*))))

;; Internal dependance tracking functions
(defun unload-dependent (this dependent)
  (let* ((deps (gethash (key this) *dependents*))
	 (k (key dependent)))
    (when (member k deps)
      (unload dependent)
      (setf (gethash (key this) *dependents*)
	    (remove k (gethash (key this) *dependents*))))))

(defun add-dependent (this dependent)
  (setf (gethash (key this) *dependents*)
	(cons (key dependent)
	      (gethash this *dependents*)))
  dependent)

(defun remove-dependent (this dependent)
  (setf (gethash this *dependents*)
	(remove dependent
		(gethash this *dependents*))))
  
(defun unload-all-dependants (this)
  (map nil (lambda (key)
	     (let ((val (gethash key *uncollected*)))
	       (when val
		 (unload val))))
       (gethash this *dependents*))
  (remhash this *dependents*))

(defun normalize-for-3D (m)
  (m4:*s m (aref m 15)))

(defun decompose-transform (m)
  "Decomposes a matrix into it's position vector3, rotation quaterion and scaling vector3.
   Useful for creating/updating the node object."
  (let* ((rot (q:normalize
	      (q:from-mat3
	       (m4:to-mat3 m))))
	 (pos (subseq (m4:get-column m 3) 0 3))
	 (scale (m4:*
		 (m4:affine-inverse
		  (q:to-mat4 rot)) m)))
    (values pos
	    rot
	    (clinch:v! (m:elm scale 0 0)
		       (m:elm scale 1 1)
		       (m:elm scale 2 2)))))

(defun set-assoc-value (alist item value)
  (setf (cdr (assoc item alist)) value))

(defun set-assoc-name (alist item value)
  (setf (car (assoc item alist)) value))

(defun get-point (array i)
  (let ((x (* i 3)))
    (subseq array x (+ x 3))))


(defun ray-triangle-intersect? (origin ray-dir v0 v1 v2)
  "Given an origin, direction and a triangle returns if and where they intersect.
   Presently does not cull backfacing triangles."
  (let ((epsilon 1.0e-6))
    (let ((edge1 (v3:- v1 v0))
	  (edge2 (v3:- v2 v0)))
      (let* ((pvec (v3:cross ray-dir edge2))
	     (det (v3:dot edge1 pvec)))
	(when (not (and (> det (- epsilon)) (< det epsilon)))
	  (let* ((inverse-det (/ 1.0 det))
		 (tvec (v3:- origin v0))
		 (u (* (v3:dot tvec pvec) inverse-det)))
	    (when (not (or (< u 0.0) (> u 1.0)))
	      (let* ((qvec (v3:cross tvec edge1))
		     (v (* (v3:dot ray-dir qvec) inverse-det)))
		(when (not (or (< v 0.0) (> (+ u v) 1.0)))
		  (let ((hit-distance (* (v3:dot edge2 qvec)
					 inverse-det)))
		    ;; values ?
		    (when (>= hit-distance 0.0)
		      (values hit-distance u v))))))))))))

(defun ray-triangles-intersect (points index origin direction)
  (loop for x from 0 below (length index) by 3
       
     collect (let ((ret (multiple-value-list 
			 (ray-triangle-intersect? origin 
						  direction 
						  (get-point-by-index points (elt index (+ 0 x)))
						  (get-point-by-index points (elt index (+ 1 x)))
						  (get-point-by-index points (elt index (+ 2 x)))))))
	       (when (car ret) ret))))
	 

(defun get-intersections (start direction iarray varray)
  (loop 
     for i from 0
     for x from 0 below (length iarray) by 3

     for ret = (let ((xx (get-point varray (aref iarray x)))
		     (yy (get-point varray (aref iarray (+ x 1))))
		     (zz (get-point varray (aref iarray (+ 2 x)))))

		 (multiple-value-bind (distance u v)
		     (ray-triangle-intersect?
		      start direction xx yy zz)
		   (when distance (list i distance u v))))
     when (cdr ret) collect ret))



(defmacro clone-function (old new)
  `(setf (fdefinition ',new) (fdefinition ',old)))

(defun split-keywords (lst &optional keys objects)
  (cond 
    ((or (null lst)
	 (and (keywordp (first lst))
	      (null (cdr lst))))
     (values keys (reverse objects)))
    
    ((and (first lst) 
	  (second lst)
	  (keywordp (first lst))) 
     (push (second lst) keys)
     (push (first lst) keys)
     (split-keywords (cddr lst) keys objects))
    
    (t 
     (push (first lst) objects) 
     (split-keywords (cdr lst) keys objects))))

(defun transform-tree (tester transformer tree)
  "I think this is a tree walker which applies a function to a clinch node tree."
  (cond ((consp tree)
	 ;; it's a cons. process the two subtrees.
	 (destructuring-bind (left . right) tree
	   (cons
	    ;; process left subtree.
	    (if (funcall tester left)
		(funcall transformer left)
		;; nothing to transform here. move on down the left side.
		(if (consp left)
		    (transform-tree tester transformer left)
		    left))
	    ;; process right subtree.
	    (transform-tree tester transformer right))))
	;; it's not a cons. test it.
	((funcall tester tree)
	 (funcall transformer tree))
	;; it failed the test. leave it alone.
	(t tree)))


(defun separate (lst len)
  (when lst
    (cons (subseq lst 0 len)
	  (separate (subseq lst len) len))))

(defun midpoint (vectors)
  (map 'list (lambda (x) (/ x (length vectors)))
       (reduce #'v:+ vectors)))
	 

(defun topological-sort (graph &key (test 'eql))
  "Graph is an association list whose keys are objects and whose
values are lists of objects on which the corresponding key depends.
Test is used to compare elements, and should be a suitable test for
hash-tables.  Topological-sort returns two values.  The first is a
list of objects sorted toplogically.  The second is a boolean
indicating whether all of the objects in the input graph are present
in the topological ordering (i.e., the first value)."
  (let ((entries (make-hash-table :test test)))
    (flet ((entry (vertex)
	     "Return the entry for vertex.  Each entry is a cons whose
              car is the number of outstanding dependencies of vertex
              and whose cdr is a list of dependants of vertex."
	     (multiple-value-bind (entry presentp) (gethash vertex entries)
	       (if presentp entry
		   (setf (gethash vertex entries) (cons 0 '()))))))
      ;; populate entries initially
      (dolist (vertex graph)
	(destructuring-bind (vertex &rest dependencies) vertex
	  (let ((ventry (entry vertex)))
	    (dolist (dependency dependencies)
	      (let ((dentry (entry dependency)))
		(unless (funcall test dependency vertex)
		  (incf (car ventry))
		  (push vertex (cdr dentry))))))))
      ;; L is the list of sorted elements, and S the set of vertices
      ;; with no outstanding dependencies.
      (let ((L '())
	    (S (loop for entry being each hash-value of entries
		  using (hash-key vertex)
		  when (zerop (car entry)) collect vertex)))
	;; Until there are no vertices with no outstanding dependencies,
	;; process vertices from S, adding them to L.
	(do* () ((endp S))
	  (let* ((v (pop S)) (ventry (entry v)))
	    (remhash v entries)
	    (dolist (dependant (cdr ventry) (push v L))
	      (when (zerop (decf (car (entry dependant))))
		(push dependant S)))))
	;; return (1) the list of sorted items, (2) whether all items
	;; were sorted, and (3) if there were unsorted vertices, the
	;; hash table mapping these vertices to their dependants
	(let ((all-sorted-p (zerop (hash-table-count entries))))
	  (values (nreverse L)
		  all-sorted-p
		  (unless all-sorted-p
		    entries)))))))

(defun merge-hashes (hashes)

  (let ((ret (make-hash-table :test 'equal)))
    (map 'list (lambda (h)
		 (maphash (lambda (key val)
			    (setf (gethash key ret) val)) h))
	 hashes)
    ret))

(defun make-list-length (len lst &optional (fill-value 0))
  (loop for i in lst
     append (loop 
	       with llst = i
	       for x from 0 below len
	       collect (if llst (car llst) fill-value)
	       do (setf llst (when llst (cdr llst))))))


(defun get-triangle-normal (a b c)
  (v3:normalize 
   (v3:cross 
    (v3:- b a) 
    (v3:- c a))))

(defmacro make-3f-buffer (&rest data)
  `(make-instance 'buffer
		  :stride 3
		  :data (map 'list (lambda (x)
				     (coerce x 'single-float))
			     (concatenate 'list ,@data))))
(defmacro make-2f-buffer (&rest data)
	   `(make-instance 'buffer
			   :stride 2
			   :data (map 'list (lambda (x)
					      (coerce x 'single-float))
				      (concatenate 'list ,@data))))

