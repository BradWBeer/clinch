;;;; clinch-classimp.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defun load-mesh (path)
  (classimp:with-log-to-stdout ()
    (classimp:import-into-lisp 
     (cffi-sys:native-namestring (truename path))
     :processing-flags '(:ai-Process-Triangulate 
			 :ai-Process-Join-Identical-Vertices 
			 :ai-Process-Sort-By-P-Type 
			 :ai-process-optimize-graph 
			 :ai-process-optimize-meshes 
			 :ai-Process-Find-Instances 
			 :ai-Process-Find-Invalid-Data 
			 :ai-Process-Remove-Redundant-Materials 
			 :ai-process-Gen-Normals))))

(defun animated? (scene)
  (or (> (length (classimp:animations scene)) 0)
      (loop for m across (classimp:meshes scene) 
	 when (> (length (classimp:bones m)) 0) do (return t))))
          

(defun get-base-path (file)
  (format nil "~{/~A~}/" 
	  (cdr 
	   (pathname-directory 
	    (truename file)))))


(defun import-scene (path &key (texture-hash (make-hash-table :test 'equal)))
  (let ((scene (load-mesh path))
	(base-path (get-base-path path)))
    (import-static-scene scene base-path :texture-hash texture-hash)))
  
(defun import-static-scene (scene base-path &key (texture-hash (make-hash-table :test 'equal)) (node-names (make-hash-table :test 'equal)))
  (let* ((materials (process-materials (get-materials scene) texture-hash base-path))
	 (meshes (classimp:meshes scene))
	 (entities 
	  (loop for x from 0 below (length meshes)
	     collect (let* ((mesh (elt meshes x))
			    (material (nth (classimp:material-index mesh) materials))
			    (skeleton (unless (zerop (length (ai:bones mesh)))
					(make-instance 'clinch::skeleton :mesh mesh :node-hash node-names))))
		       
		       (make-classimp-entity
			(make-index-buffer (classimp:faces mesh))
			(make-vector-buffer (classimp:vertices mesh))
			(make-vector-buffer (classimp:normals mesh))
			:bones skeleton
			:texture (cdr (assoc "t1" material :test #'string-equal))
			:texture-coordinate-buffer (let ((tc (classimp:texture-coords mesh)))
						     (when (> (length tc) 0)
						       (make-texture-coord-buffer mesh 0)))
			:vertex-color-buffer (let ((tc (classimp:colors mesh)))
					       (when (> (length tc) 0)
						 (elt tc 0))))))))
    
    (multiple-value-bind (ret node-hash)
	(get-nodes (classimp:root-node scene) :entities entities :node-name-hash node-names)

      (let ((animations
	     (loop for a across (ai:animations scene)
		collect (make-instance 'clinch::node-animation
				       :node ret
				       :ai-animation a 
				       :node-names node-names))))

	(values ret
		animations
		node-hash
		scene
		base-path
		materials
		meshes
		entities
		node-names)))))
  

(defun make-classimp-entity (index-buffer vertex-buffer normal-buffer &key bones shader-program texture texture-coordinate-buffer vertex-color-buffer parent)
  (let ((entity 
	 (make-instance 'clinch:entity
			:parent parent
			:shader-program (cond (shader-program shader-program)
					      (bones (get-generic-single-diffuse-light-animation-shader))
					      (t (get-generic-single-diffuse-light-shader)))
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
				    ("lightIntensity" . (.8 .8 .8))))))
    (when bones
      (setf (uniform entity "bones") bones
	    (attribute entity "weights") (weights-buffer bones)
	    (attribute entity "boneIDs") (bone-id-buffer bones)))
    entity))    



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
					(make-texture-from-file (concatenate 'string base-path file))))))
		    (cons "t1" tex))			 
		  i))
   texture-hash))

(defun process-materials (materials texture-hash base-path)
  (loop for i in materials
     collect (process-material (get-uniforms i) texture-hash base-path)))

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
