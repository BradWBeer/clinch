(make-instance
 'clinch:entity
 :shader shader
 :indexes (make-instance 'clinch:buffer :qtype :unsigned-int
			 :target :element-array-buffer
			 :Stride 1
			 :data '(0 1 2 1 3 2))
 :vertices (make-instance 'clinch:buffer 
			  :Stride 3
			  :data (map 'list (lambda (x)
					     (coerce x 'single-float)) '(-100 0 -100
									 -100 0  100
									 100 0 -100
									 100 0  100)))

 :normals  (make-instance 'clinch:buffer 
			  :Stride 3
			  :data (map 'list (lambda (x)
					     (coerce x 'single-float)) '(0 1 0
									 0 1 0
									 0 1 0
									 0 1 0)))
 :values   `((:uniform "ambientLight"   ,ambientLight)
	     (:uniform "lightIntensity" ,lightIntensity)
	     (:uniform "lightDirection" ,(lambda (&optional a b c) lightDirection))
	     (:uniform "color"  (1.0 1.0 1.0 1.0))))
