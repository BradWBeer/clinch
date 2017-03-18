(make-instance
 'clinch:entity
 :shader shader
 :indexes (make-instance 'clinch:buffer :qtype :unsigned-int
			 :target :element-array-buffer
			 :Stride 1
			 :data clode:box-indexes)
 :vertices (make-instance 'clinch:buffer 
			  :Stride 3
			  :data (map 'list (lambda (x)
					     (coerce x 'single-float)) clode:box-vertexes))
 :normals  (make-instance 'clinch:buffer 
			  :Stride 3
			  :data (map 'list (lambda (x)
					     (coerce x 'single-float)) clode:box-normals))
 :values   `((:uniform "ambientLight"   ,ambientLight)
	     (:uniform "lightIntensity" ,lightIntensity)
	     (:uniform "lightDirection" ,(lambda (&optional a b c) lightDirection))
	     (:uniform "color"  (1.0 1.0 0.0 1.0))))
