;;;; clinch-classimp/shaders.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defparameter *generic-single-diffuse-light-shader* nil)
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
							"clinch-classimp/generic-single-diffuse-light-shader.vert"))
			   :fragment-shader (alexandria:read-file-into-string
					   (concatenate 'string 
							(directory-namestring
							 (asdf:system-relative-pathname :clinch "clinch.asd"))
							"clinch-classimp/generic-single-diffuse-light-shader.frag"))
			   
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


