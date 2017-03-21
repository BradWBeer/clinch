;;;; gc.lisp
;;;; Please see the licence.txt for the CLinch 

(defparameter *uncollected*
  #+(or ccl ecl) (make-hash-table :test 'eq)
  #-(or ccl ecl) (trivial-garbage:make-weak-hash-table :weakness :key-or-value)
  "Weak hash of loaded OpenGL objects.")

(defparameter *dependents*  
  #+(or ccl ecl) (make-hash-table :test 'eq)
  #-(or ccl ecl) (trivial-garbage:make-weak-hash-table :weakness :key-or-value)
  "Weak hash of OpenGL objects waiting to be unloaded by another.")

(defparameter *objects-with-modified-dependants*  
  #+(or ccl ecl) (make-hash-table :test 'eq)
  #-(or ccl ecl) (trivial-garbage:make-weak-hash-table :weakness :key-or-value)
  "Weak hash of OpenGL objects waiting to be unloaded by another.")

(defgeneric unload (this &key) 
  (:documentation "Unloads an opengl object. Does nothing for non-opengl objects."))
(defmethod unload ((this t) &key))

(defgeneric get-dependants (this &key) 
  (:documentation "Unloads an opengl object. Does nothing for non-opengl objects."))
(defmethod get-dependants ((this t) &key))

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


(defmacro ready-finialize ((this) &body body)
  (let ((local-this (gensym "this"))
	(local-key (gensym "key")))
    `(let* ((,local-this ,this)
	    (,local-key (key ,local-this)))
       (trivial-garbage:cancel-finalization ,local-this)
       (add-uncollected ,local-this)
       (trivial-garbage:finalize ,local-this
				 (lambda ()
				   (!!
				    ,@body
				    (remove-uncollected ,local-key)))))))



;; figure out how to find dependancies....
(defun find-deps () 
  )

node
->entity
  node
 


./shaders.lisp
./framebuffer.lisp
./texture-animation.lisp
./default-shaders.lisp
./shader-program.lisp
./pixel-buffer.lisp
./viewport.lisp
./entity.lisp
./window.lisp
./threads.lisp
./shapes.lisp
./animation.lisp
./node.lisp
./transform.lisp
./2d-node.lisp
./texture.lisp
./index-buffer.lisp
./buffer.lisp


./clinch-cairo/package.lisp
./clinch-cairo/clinch-cairo.lisp
./clinch-cairo/shapes.lisp

./clinch-pango/package.lisp
./clinch-pango/clinch-pango.lisp
./clinch.lisp
./clinch-freeimage/clinch-freeimage.lisp
./clinch-freeimage/package.lisp



./clinch-classimp/clinch-classimp.lisp
./clinch-classimp/package.lisp
./clinch-classimp/bone.lisp
./clinch-classimp/skeleton.lisp
./clinch-classimp/node.lisp
./clinch-classimp/node-animation.lisp
