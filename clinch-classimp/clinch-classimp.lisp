;;;; clinch-classimp.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defparameter *identity-texture* nil)

(defparameter *identity-texture* nil)

(defun get-identity-texture ()
  (or *identity-texture*
      (setf *identity-texture*
	    (make-instance 'clinch:texture
			   :data   '(255 255 255 255)
			   :width  1
			   :height 1
			   :stride 4
			   :count  1
			   :qtype  :unsigned-char))))


(defun load-mesh (path)
  (classimp:with-log-to-stdout ()
    (classimp:import-into-lisp 
     (cffi-sys:native-namestring (truename path))
     :processing-flags '(:ai-Process-Triangulate :ai-Process-Join-Identical-Vertices :ai-Process-Sort-By-P-Type))))


(defun translate-node-to-clinch (node entities) 
  (make-instance 'node 
		 :matrix (classimp:transform node)
		 :children (append 
			    (map 'list
				 (lambda (x)
				   (nth x entities))
				 (coerce (classimp:meshes node) 'list))
			    (reverse (map 'list 
					  (lambda (x)
					    (translate-node-to-clinch x entities))
					  (classimp:children node))))))

(defun get-base-path (file)
  (format nil "~{/~A~}/" 
	  (cdr 
	   (pathname-directory 
	    (truename file)))))


(defun get-material (materials index)
  (alexandria:hash-table-alist (elt materials index)))

(defun get-uniforms (material)
  (map 'list (lambda (x)
	       (list 
		:uniform
		(subseq (car x) 5)
		(cdr x)))
       material))

(defun get-all-materials (scene) 
  (loop 
     with mats = (classimp:materials scene)
     for i from 0 below (length mats)
     collect (get-uniforms (get-material mats i))))

(defun load-textures-from-material (material directory textures-hash)
  (!
    (map 'list
	 (lambda (x)
	   (unless (gethash x textures-hash)
	     (setf (gethash x textures-hash) (create-texture-from-file (concatenate 'string directory x)))))
	 (remove-duplicates 
	  (loop for i in material
	     when (string-equal (second i) "file")
	     append (map 'list #'caddr (third i)))
	:test #'string-equal))))
	      
(defun load-all-textures (materials base-path)
  (let ((textures-hash (make-hash-table :test 'equal)))
    (map nil (lambda (x)
	       (load-textures-from-material x base-path textures-hash))
	 materials)
    textures-hash))

(defun add-textures-to-material (mat textures-hash)
  (let ((tex-uniforms 
	 (car (loop for (uniform name . rest) in mat
		 if (string-equal name "file")
		 return rest))))
    (remove-if-not (lambda (x)
		     (cadr x))
		   (loop for (type _ file) in tex-uniforms
		      collect (list
			       :uniform
			       (case type
				 (:AI-TEXTURE-TYPE-AMBIENT "ambientTexture")
				 (:AI-TEXTURE-TYPE-DIFFUSE "diffuseTexture"))
			       (gethash file textures-hash))))))

(defun add-textures-to-all-materials (materials textures-hash)
  (loop for m in materials
     collect (add-textures-to-material m textures-hash)))

(defun get-all-material-uniforms (materials textures-hash)
  (loop for m in materials 
     collect (append 
	      (add-textures-to-material m textures-hash)
	      (remove-if (lambda (x)
			   (string-equal "file" (second x)))
			 m))))


(defun get-mesh (scene index) 
  (elt (classimp:meshes scene) index))


(defun make-index-buffer (v)
  (let* ((len (* (length v) (length (elt v 0)))))
    (cffi:with-foreign-object (p :unsigned-int len)
      (loop 
	 for i from 0 below (length v)
	 do (loop 
	       with tuple = (elt v i)
	       for j from 0 below 3
	       do (setf (cffi:mem-aref p :int (+ (* 3 i) j))
			(elt tuple j))))
      
      (make-instance 'index-buffer
		     :data p
		     :count len
		     :qtype :unsigned-int
		     :target :element-array-buffer))))

(defun make-vector-buffer (v &key stride)
  (let* ((stride (or stride (length (elt v 0))))
	 (len (* (length v) stride)))
    (cffi:with-foreign-object (p :float len)
      (loop 
	 for i from 0 below (length v)
	 do (loop 
	       with tuple = (elt v i)
	       for j from 0 below stride
	       do (setf (cffi:mem-aref p :float (+ (* stride i) j))
			(elt tuple j))))
      (make-instance 'clinch:buffer 
		     :data p
		     :count len
		     :Stride stride))))

(defun make-texture-coord-buffer (mesh index)
  (clinch::make-vector-buffer (elt (classimp:texture-coords mesh) index)
			      :stride (elt (classimp:components-per-texture-coord mesh) index)))
