;;;; clinch.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

;; not currently using these
(defparameter *current-shader-attributes* (trivial-garbage:make-weak-hash-table))
(defparameter *current-shader-uniforms*   (trivial-garbage:make-weak-hash-table))

(defparameter *shaders->shader-programs* (make-hash-table))

(defparameter *uncollected*  (trivial-garbage:make-weak-hash-table :weakness :value)
  "Weak hash of loaded OpenGL objects.")

(defun unload-all-uncollected ()
  "Unloads all loaded OpenGL objects."
  (loop for key being the hash-keys of *uncollected*
     do (unload (gethash key *uncollected*))))

(defmacro ! (&body body)
  "Runs body in main thread for safe OpenGL calls. Waits for return value."
  `(sdl2:in-main-thread ()
     ,@body))

(defmacro !! (&body body)
  "Runs body in main thread for safe OpenGL calls. Returns immediately."
  `(sdl2:in-main-thread (:background t)
     ,@body))


(defun decompose-transform (m)
  "Decomposes a matrix into it's position vector3, rotation quaterion and scaling vector3.
   Useful for creating/updating the node object."
  (let* ((rot (q:normalize
	      (q:make-quat-from-rotation-matrix3
	       (m4:to-matrix3 m))))
	 (pos (subseq (m4:get-column m 3) 0 3))
	 (scale (m4:m*
		 (m4:affine-inverse
		  (q:to-matrix4 rot)) m)))
    (values pos
	    rot
	    (clinch:v! (m:elm scale 0 0)
		       (m:elm scale 1 1)
		       (m:elm scale 2 2)))))

(defun set-assoc-value (alist item value)
  (setf (cdr (assoc item alist)) value))

(defun set-assoc-name (alist item value)
  (setf (car (assoc item alist)) value))



(defun ray-triangle-intersect? (origin ray-dir v0 v1 v2)
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



(defun list-shader-uniforms (shader)
	   (! (loop for i from 0 below (gl:get-program (program shader) :active-uniforms) 
		 collect (cons i (multiple-value-list (gl:get-active-uniform (program shader) i))))))

(defun list-shader-attributes (shader)
  (! (loop for i from 0 below (gl:get-program (program shader) :active-attributes) 
	collect (cons i (multiple-value-list (gl:get-active-attrib (program shader) i))))))
