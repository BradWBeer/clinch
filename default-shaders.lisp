;;; default-shaders.lisp
;;;; Please see the licence.txt for the CLinch

(in-package #:clinch)

(defparameter *generic-shader-hash-table* (make-hash-table))

(defun get-generic-single-texture-shader ()
  "Creates/returns a shader-program which blits a texture to an entity.
   Uniforms:
    P: projection matrix
    M: model-view matrix
    t1: texture
   Attributes:
    v: Vertexes
    tc1: texture coordinates"

  (let ((shader (gethash 'generic-single-texture-shader *generic-shader-hash-table*)))
  (if (and shader (program shader)) shader
      (setf (gethash 'generic-single-texture-shader *generic-shader-hash-table*)
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
							  "shaders/generic-single-texture-shader.frag")))))))

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

  (let ((shader (gethash 'generic-solid-phong-shader *generic-shader-hash-table*)))
    (if (and shader (program shader)) shader
      (setf (gethash 'generic-solid-phong-shader *generic-shader-hash-table*)
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
							  "shaders/generic-solid-phong-shader.frag")))))))


(defun get-generic-single-diffuse-light-shader ()
  (let((shader (gethash 'generic-single-diffuse-light-shader *generic-shader-hash-table*)))
  (if (and shader (program shader)) shader
      (setf (gethash 'generic-single-diffuse-light-shader *generic-shader-hash-table*)
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
							"shaders/generic-single-diffuse-light-shader.frag")))))))


(defun get-generic-single-diffuse-light-animation-shader ()
  (let ((shader (gethash 'generic-single-diffuse-light-animation-shader *generic-shader-hash-table*)))
  (if (and shader (program shader)) shader
      (setf (gethash 'generic-single-diffuse-light-animation-shader *generic-shader-hash-table*)
	    (make-instance 'clinch:shader-program
			   :name "generic-single-diffuse-light-animation-shader"
			   :vertex-shader (alexandria:read-file-into-string
					   (concatenate 'string 
							(directory-namestring
							 (asdf:system-relative-pathname :clinch "clinch.asd"))
							"shaders/generic-single-diffuse-light-animation-shader.vert"))
			   :fragment-shader (alexandria:read-file-into-string
					   (concatenate 'string 
							(directory-namestring
							 (asdf:system-relative-pathname :clinch "clinch.asd"))
							"shaders/generic-single-diffuse-light-animation-shader.frag")))))))


(defun get-generic-single-diffuse-light-per-vertex-color-shader ()
  (let ((shader (gethash 'generic-single-diffuse-light-per-vertex-color *generic-shader-hash-table*)))
    (if (and shader (program shader)) shader
	(setf (gethash 'generic-single-diffuse-light-per-vertex-color *generic-shader-hash-table*)
	      (make-instance 'clinch:shader-program
			     :name "generic-single-diffuse-light-per-vertex-color-shader"
			     :vertex-shader (alexandria:read-file-into-string
					   (concatenate 'string 
							(directory-namestring
							 (asdf:system-relative-pathname :clinch "clinch.asd"))
							"shaders/generic-single-diffuse-light-per-vertex-color.vert"))
			   :fragment-shader (alexandria:read-file-into-string
					   (concatenate 'string 
							(directory-namestring
							 (asdf:system-relative-pathname :clinch "clinch.asd"))
							"shaders/generic-single-diffuse-light-per-vertex-color.frag")))))))

(defun get-generic-single-color-shader ()
  (let ((shader (gethash 'generic-single-color *generic-shader-hash-table*)))
    (if (and shader (program shader)) shader
	(setf (gethash 'generic-single-color *generic-shader-hash-table*)
	      (make-instance 'clinch:shader-program
			     :name "generic-single-color-shader"
			     :vertex-shader (alexandria:read-file-into-string
					   (concatenate 'string 
							(directory-namestring
							 (asdf:system-relative-pathname :clinch "clinch.asd"))
							"shaders/generic-single-color-shader.vert"))
			   :fragment-shader (alexandria:read-file-into-string
					   (concatenate 'string 
							(directory-namestring
							 (asdf:system-relative-pathname :clinch "clinch.asd"))
							"shaders/generic-single-color-shader.frag")))))))

(defun get-generic-per-vertex-color-shader ()
  (let ((shader (gethash 'generic-per-vertex-color *generic-shader-hash-table*)))
    (if (and shader (program shader)) shader
	(setf (gethash 'generic-per-vertex-color *generic-shader-hash-table*)
	      (make-instance 'clinch:shader-program
			     :name "generic-per-vertex-color-shader"
			     :vertex-shader (alexandria:read-file-into-string
					   (concatenate 'string 
							(directory-namestring
							 (asdf:system-relative-pathname :clinch "clinch.asd"))
							"shaders/generic-per-vertex-color-shader.vert"))
			   :fragment-shader (alexandria:read-file-into-string
					   (concatenate 'string 
							(directory-namestring
							 (asdf:system-relative-pathname :clinch "clinch.asd"))
							"shaders/generic-per-vertex-color-shader.frag")))))))
(defun get-generic-single-color-shader ()
  (let ((shader (gethash 'generic-single-color *generic-shader-hash-table*)))
    (if (and shader (program shader)) shader
	(setf (gethash 'generic-single-color *generic-shader-hash-table*)
	      (make-instance 'clinch:shader-program
			     :name "generic-single-color-shader"
			     :vertex-shader (alexandria:read-file-into-string
					   (concatenate 'string 
							(directory-namestring
							 (asdf:system-relative-pathname :clinch "clinch.asd"))
							"shaders/generic-single-color-shader.vert"))
			   :fragment-shader (alexandria:read-file-into-string
					   (concatenate 'string 
							(directory-namestring
							 (asdf:system-relative-pathname :clinch "clinch.asd"))
							"shaders/generic-single-color-shader.frag")))))))




(defmacro define-default-shader (name)
  (let ((sym (intern (string-upcase (princ-to-string name))))
	(func (intern (string-upcase (concatenate 'string "get-" (princ-to-string name)))))
	(str (string-downcase (princ-to-string name))))
    `(defun ,func ()
       (let ((shader (gethash ',name *generic-shader-hash-table*)))
	 (if (and shader (program shader)) shader
	     (setf (gethash ',name *generic-shader-hash-table*)
		   (make-instance 'shader-program
				  :name ,str
				  :vertex-shader (alexandria:read-file-into-string
						  (concatenate 'string 
							       (directory-namestring
								(asdf:system-relative-pathname :clinch "clinch.asd"))
							       (concatenate 'string "shaders/" ,str ".vert"))
				  :fragment-shader (alexandria:read-file-into-string
						    (concatenate 'string 
								 (directory-namestring
								  (asdf:system-relative-pathname :clinch "clinch.asd"))
							       (concatenate 'string "shaders/" ,str ".vert")))))))))))
