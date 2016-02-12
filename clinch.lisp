;;;; clinch.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

;; not currently using these
(defparameter *current-shader-attributes* (trivial-garbage:make-weak-hash-table))
(defparameter *current-shader-uniforms*   (trivial-garbage:make-weak-hash-table))

(defparameter *shaders->shader-programs* (make-hash-table))

(defparameter *uncollected* (trivial-garbage:make-weak-hash-table :weakness :key-or-value)
  "Weak hash of loaded OpenGL objects.")

(defparameter *dependents*  (trivial-garbage:make-weak-hash-table :weakness :key-or-value)
  "Weak hash of OpenGL objects waiting to be unloaded by another.")

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

(defun add-dependent (this dependent)
  (setf (gethash (key this) *dependents*)
	(cons (key dependent)
	      (gethash this *dependents*))))

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


;; (defun make-local-path (file)
;;   (concatenate 'string 
;; 	       (directory-namestring
;; 		(asdf:system-relative-pathname :clinch "clinch.asd"))
;; 	       file))

;; (defun eval-from-file (file)
;;   (eval
;;    (read-from-string
;;     (alexandria:read-file-into-string
;;      (make-local-path file)))))

;; (defun make-standard-shader (name)
;;   (eval-from-file 
;;    (concatenate 'string 
;; 		"assets/shaders/"
;; 		name
;; 		".lisp")))

