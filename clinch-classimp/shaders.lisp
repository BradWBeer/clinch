;;;; clinch-classimp/shaders.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defparameter *generic-raw-texture-shader* nil)
(defparameter *generic-uniform-color-with-single-diffuse-light-shader* nil)
(defparameter *generic-vertex-color-with-diffuse-light-shader* nil)
(defparameter *generic-texture-with-single-diffuse-light-shader* nil)

(defun get-generic-raw-texture-shader ()
  (or *generic-raw-texture-shader* 
      (setf *generic-raw-texture-shader*
	    (make-instance 'clinch:shader-program
			   :name "generic-raw-texture-shader"
			   :vertex-shader (alexandria:read-file-into-string "/home/warweasle/work/glsl/raw-texture-shader.vert")
			   :fragment-shader (alexandria:read-file-into-string "/home/warweasle/work/glsl/raw-texture-shader.frag")
			   
			   :uniforms '(("P" :matrix)
				       ("M" :matrix)
				       ("t1" :int))
			   :attributes '(("v" :float)
					 ("tc1" :float))))))
				       			 
(defun get-generic-uniform-color-with-single-diffuse-light-shader ()
  (or *generic-uniform-color-with-single-diffuse-light-shader*
      (setf *generic-uniform-color-with-single-diffuse-light-shader*
	    (make-instance 'clinch:shader-program
			   :name "generic-uniform-color-with-single-diffuse-light-shader"
			   :vertex-shader (alexandria:read-file-into-string "/home/warweasle/work/glsl/uniform_color_with_single-diffuse.vert")
			   :fragment-shader (alexandria:read-file-into-string "/home/warweasle/work/glsl/uniform_color_with_single-diffuse.frag")
			   
			   :uniforms '(("M" :matrix)
				       ("N" :matrix)
				       ("P" :matrix)
				       ("c" :float)
				       ("ambientLight" :float)
				       ("lightDirection" :float)
				       ("lightIntensity" :float))
			   :attributes '(("v" :float)
					 ("n" :float))))))
		       
(defun get-generic-vertex-color-with-diffuse-light-shader ()
  (or *generic-vertex-color-with-diffuse-light-shader*
      (setf *generic-vertex-color-with-diffuse-light-shader*
	    (make-instance 'clinch:shader-program
			   :name "generic-vertex-color-with-diffuse-light-shader"
			   :vertex-shader (alexandria:read-file-into-string "/home/warweasle/work/glsl/vertex-color-with-diffuse-light-shader.vert")
			   :fragment-shader (alexandria:read-file-into-string "/home/warweasle/work/glsl/vertex-color-with-diffuse-light-shader.frag")
			   
			   :uniforms '(("M" :matrix)
				       ("N" :matrix)
				       ("P" :matrix)
				       ("ambientLight" :float)
				       ("lightDirection" :float)
				       ("lightIntensity" :float))
			   :attributes '(("v" :float)
					 ("c" :float)
					 ("n" :float))))))

(defun get-generic-texture-with-single-diffuse-light-shader ()
  (or *generic-texture-with-single-diffuse-light-shader*
      (setf *generic-texture-with-single-diffuse-light-shader*
	    (make-instance 'clinch:shader-program
			   :name "generic-texture-with-single-diffuse-light-shader"
			   :vertex-shader (alexandria:read-file-into-string "/home/warweasle/work/glsl/texture-with-diffuse-light-shader.vert")
			   :fragment-shader (alexandria:read-file-into-string "/home/warweasle/work/glsl/texture-with-diffuse-light-shader.frag")
			   
			   :uniforms '(("M" :matrix)
				       ("N" :matrix)
				       ("P" :matrix)
				       ("t1" :int)
				       ("ambientLight" :float)
				       ("lightDirection" :float)
				       ("lightIntensity" :float))
			   :attributes '(("v" :float)
					 ("tc1" :float)
					 ("n" :float))))))



