;;; default-shaders.lisp
;;;; Please see the licence.txt for the CLinch

(in-package #:clinch)

(defparameter *generic-single-texture-shader* nil)
(defparameter *generic-solid-phong-shader* nil)
(defparameter *generic-single-diffuse-light-animation-shader* nil)
(defparameter *generic-single-diffuse-light-shader* nil)
(defparameter *generic-single-diffuse-light-per-vertex-color* nil)
(defparameter *generic-single-color-shader* nil)

(defun get-generic-single-texture-shader ()
  "Creates/returns a shader-program which blits a texture to an entity.
   Uniforms:
    P: projection matrix
    M: model-view matrix
    t1: texture
   Attributes:
    v: Vertexes
    tc1: texture coordinates"
    
  (if (and *generic-single-texture-shader* (program *generic-single-texture-shader*))
      *generic-single-texture-shader*
      (setf *generic-single-texture-shader*
	    (make-instance 'clinch:shader-program
			   :name "generic-single-texture-shader"
			   :vertex-shader (alexandria:read-file-into-string
					   (concatenate 'string 
							(directory-namestring
							 (asdf:system-relative-pathname :clinch "clinch.asd"))
							"shaders/generic-single-texture-shader.vert"))
			   :fragment-shader (alexandria:read-file-into-string
					     (concatenate 'string 
							  (directory-namestring
							   (asdf:system-relative-pathname :clinch "clinch.asd"))
							  "shaders/generic-single-texture-shader.frag"))
			   :uniforms '(("P" :matrix)
				       ("M" :matrix)
				       ("t1" :int))
			   :attributes '(("tc1" :float)
					 ("v" :float))))))


(defun get-generic-solid-phong-shader ()
  "Creates/returns a shader-program which uses simple phong shading with a single color.
   Uniforms:
    P: projection matrix
    M: model-view matrix
    ambientLight: Ambient Light Color
    lightDirection: Direction of light
    lightIntensity: Color of light
    color: color of object
   Attributes:
    v: Vertexes"
    
  (if (and *generic-solid-phong-shader* (program *generic-solid-phong-shader*))
      *generic-solid-phong-shader*
      (setf *generic-solid-phong-shader*
	    (make-instance 'clinch:shader-program
			   :name "generic-solid-phong-shader"
			   :vertex-shader (alexandria:read-file-into-string
					   (concatenate 'string 
							(directory-namestring
							 (asdf:system-relative-pathname :clinch "clinch.asd"))
							"shaders/generic-solid-phong-shader.vert"))
			   :fragment-shader (alexandria:read-file-into-string
					     (concatenate 'string 
							  (directory-namestring
							   (asdf:system-relative-pathname :clinch "clinch.asd"))
							  "shaders/generic-solid-phong-shader.frag"))
			   :uniforms '(("P" :matrix)
				       ("M" :matrix)
				       ("ambientLight" :float)
				       ("lightDirection" :float)
				       ("lightIntensity" :float)
				       ("color" :float))
			   :attributes '(("v" :float))))))


(defun get-generic-single-diffuse-light-shader ()
  (if (and *generic-single-diffuse-light-shader* (program *generic-single-diffuse-light-shader*))
      *generic-single-diffuse-light-shader*
      (setf *generic-single-diffuse-light-shader*
	    (make-instance 'clinch:shader-program
			   :name "generic-single-diffuse-light-shader"
			   :vertex-shader (alexandria:read-file-into-string
					   (concatenate 'string 
							(directory-namestring
							 (asdf:system-relative-pathname :clinch "clinch.asd"))
							"shaders/generic-single-diffuse-light-shader.vert"))
			   :fragment-shader (alexandria:read-file-into-string
					   (concatenate 'string 
							(directory-namestring
							 (asdf:system-relative-pathname :clinch "clinch.asd"))
							"shaders/generic-single-diffuse-light-shader.frag"))			   
			   :uniforms '(("M" :matrix)
				       ("N" :matrix)
				       ("P" :matrix)
				       ("t1" :int)
				       ("ambientLight" :float)
				       ("lightDirection" :float)
				       ("lightIntensity" :float))
				       
			   :attributes '(("v" :float)
					 ("n" :float)
					 ("c" :float)
					 ("tc1" :float))))))


(defun get-generic-single-diffuse-light-animation-shader ()
  (if (and *generic-single-diffuse-light-animation-shader* (program *generic-single-diffuse-light-animation-shader*))
      *generic-single-diffuse-light-animation-shader*
      (setf *generic-single-diffuse-light-animation-shader*
	    (make-instance 'clinch:shader-program
			   :name "test-shader"
			   :vertex-shader (alexandria:read-file-into-string
					   (concatenate 'string 
							(directory-namestring
							 (asdf:system-relative-pathname :clinch "clinch.asd"))
							"shaders/generic-single-diffuse-light-animation-shader.vert"))
			   :fragment-shader (alexandria:read-file-into-string
					   (concatenate 'string 
							(directory-namestring
							 (asdf:system-relative-pathname :clinch "clinch.asd"))
							"shaders/generic-single-diffuse-light-animation-shader.frag"))
			   
			   :uniforms '(("M" :matrix)
				       ("N" :matrix)
				       ("P" :matrix)
				       ("t1" :int)
				       ("ambientLight" :float)
				       ("lightDirection" :float)
				       ("lightIntensity" :float)
				       ("bones" :matrix)
				       )
				       
			   :attributes '(("v" :float)
					 ("n" :float)
					 ("c" :float)
					 ("tc1" :float)
					 ("boneIDs" :int)
					 ("weights" :float))))))


(defun get-generic-single-diffuse-light-per-vertex-color-shader ()
  (if (and *generic-single-diffuse-light-per-vertex-color* (program *generic-single-diffuse-light-per-vertex-color*))
      *generic-single-diffuse-light-per-vertex-color*
      (setf *generic-single-diffuse-light-per-vertex-color*
	    (make-instance 'clinch:shader-program
			   :name "test-shader"
			   :vertex-shader (alexandria:read-file-into-string
					   (concatenate 'string 
							(directory-namestring
							 (asdf:system-relative-pathname :clinch "clinch.asd"))
							"shaders/generic-single-diffuse-light-per-vertex-color.vert"))
			   :fragment-shader (alexandria:read-file-into-string
					   (concatenate 'string 
							(directory-namestring
							 (asdf:system-relative-pathname :clinch "clinch.asd"))
							"shaders/generic-single-diffuse-light-per-vertex-color.frag"))))))


(defun get-generic-single-color-shader ()
  (if (and *generic-single-color-shader* (program *generic-single-color-shader*))
      *generic-single-color-shader*
      (setf *generic-single-color-shader*
	    (make-instance 'clinch:shader-program
			   :name "test-shader"
			   :vertex-shader (alexandria:read-file-into-string
					   (concatenate 'string 
							(directory-namestring
							 (asdf:system-relative-pathname :clinch "clinch.asd"))
							"shaders/generic-single-color-shader.vert"))
			   :fragment-shader (alexandria:read-file-into-string
					   (concatenate 'string 
							(directory-namestring
							 (asdf:system-relative-pathname :clinch "clinch.asd"))
							"shaders/generic-single-color-shader.frag"))))))




