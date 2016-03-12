;;;; clinch-classimp.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

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
	       (cons
		(subseq (car x) 5)
		(cdr x)))
       material))

(defun get-materials (scene)
  (loop with mats = (classimp:materials scene)
     for x from 0 below (length mats)
     collect (get-material mats x)))

(defun replace-slashes (str)
  (map 'string (lambda (x)
		 (if (char= #\\ x) #\/ x)) str))

(defun process-material (material texture-hash base-path)
  (values
   (loop for i in material 
      collect (if (string-equal (first i) "file")
		  (let* ((file (replace-slashes (third (cadr i))))
			 (tex (or (gethash file texture-hash)
				  (setf (gethash file texture-hash)
					(create-texture-from-file (concatenate 'string base-path file))))))
		    (cons "t1" tex))			 
		  i))
   texture-hash))

(defun process-materials (materials texture-hash base-path)
  (loop for i in materials
     collect (process-material (get-uniforms i) texture-hash base-path)))


(defun make-classimp-entity (index-buffer vertex-buffer normal-buffer &key texture texture-coordinate-buffer vertex-color-buffer)
									
  (make-instance 'clinch:entity
		 :shader-program (get-generic-single-diffuse-light-shader)
		 :indexes index-buffer
		 :attributes `(("v" . ,vertex-buffer)
			       ("n" . ,normal-buffer)
			       ("c" . ,(or vertex-color-buffer '(1.0 1.0 1.0 1.0)))
			       ("tc1" . ,(or texture-coordinate-buffer '(0 0))))
		 :uniforms `(("M" . :model)
			     ("P" . :projection)
			     ("N" . :normal)
			     ("t1" . ,(or texture (get-identity-texture)))
			     ("ambientLight" . (.2 .2 .2))
			     ("lightDirection" . (0.5772705 0.5772705 -0.5772705))
			     ("lightIntensity" . (.8 .8 .8)))))

(defun import-mesh (path &optional (texture-hash (make-hash-table :test 'equal)))
  (let* ((scene (load-mesh path))
	 (base-path (get-base-path path))
	 (materials (process-materials (get-materials scene) texture-hash base-path))
	 (meshes (classimp:meshes scene))

	 (entities 
	  (loop for x from 0 below (length meshes)
	     collect (let* ((mesh (elt meshes x))
			    (material (nth (classimp:material-index mesh) materials)))

		       (make-classimp-entity
			(make-index-buffer (classimp:faces mesh))
			(make-vector-buffer (classimp:vertices mesh))
			(make-vector-buffer (classimp:normals mesh))
			:texture (cdr (assoc "t1" material :test #'string-equal))
			:texture-coordinate-buffer (let ((tc (classimp:texture-coords mesh)))
						     (when (> (length tc) 0)
						       (make-texture-coord-buffer mesh 0)))
			:vertex-color-buffer (let ((tc (classimp:colors mesh)))
					       (when (> (length tc) 0)
						 (elt tc 0))))))))

    (translate-node-to-clinch (classimp:root-node scene) entities)))


;; (defun get-uniforms (scene texture-hash )
;;   (map 'list (lambda (x)
;; 	       (process-material x texture-hash base-path))
;;        (get-materials scene)))

;; (defun load-textures-from-material (material path hash)
;;   (loop 
;;      with base-path = (get-base-path path)
;;      for (type tc file) in (cdr (assoc "file" material :test #'string-equal))
;;      collect (let ((tex (create-texture-from-file (concatenate 'string base-path file))))
;; 	       (setf (gethash file hash) tex)
;; 	       (list type tc tex))))
	      
;; (defun load-all-textures (materials base-path)
;;   (let ((textures-hash (make-hash-table :test 'equal)))
;;     (map nil (lambda (x)
;; 	       (load-textures-from-material x base-path textures-hash))
;; 	 materials)
;;     textures-hash))

;; (defun add-textures-to-material (mat textures-hash)
;;   (let ((tex-uniforms 
;; 	 (car (loop for (uniform name . rest) in mat
;; 		 if (string-equal name "file")
;; 		 return rest))))
;;     (remove-if-not (lambda (x)
;; 		     (cadr x))
;; 		   (loop for (type _ file) in tex-uniforms
;; 		      collect (list
;; 			       :uniform
;; 			       (case type
;; 				 (:AI-TEXTURE-TYPE-AMBIENT "ambientTexture")
;; 				 (:AI-TEXTURE-TYPE-DIFFUSE "diffuseTexture"))
;; 			       (gethash file textures-hash))))))

;; (defun add-textures-to-all-materials (materials textures-hash)
;;   (loop for m in materials
;;      collect (add-textures-to-material m textures-hash)))

;; (defun get-all-material-uniforms (materials textures-hash)
;;   (loop for m in materials 
;;      collect (append 
;; 	      (add-textures-to-material m textures-hash)
;; 	      (remove-if (lambda (x)
;; 			   (string-equal "file" (second x)))
;; 			 m))))


;; (defun get-mesh (scene index) 
;;   (elt (classimp:meshes scene) index))


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
