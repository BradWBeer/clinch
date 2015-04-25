(ql:quickload :CLinch)
(ql:quickload :CLinch-sdl)
(use-package :clinch)

(defun start ()
  
  (window :title "Tutorial 2"
	  (viewport :clear-color '(0 0 0)
		    :projection-transform (clinch:make-orthogonal-transform (attribute *parent* 'width)
									    (attribute *parent* 'height)
									    .25 100)
		    (entity
		      :indexes (buffer
				 :qtype :unsigned-int
				 :target :element-array-buffer
				 :Stride 1
				 :data '(0 1 2 0 1 3))
		      :vertices (buffer
				  :qtype :double
				  :Stride 3
				  :data '(   0.0d0  100.0d0 0.0d0
					  -100.0d0 -100.0d0 0.0d0
					  100.0d0 -100.0d0 0.0d0
					  100.0d0  100.0d0 0.0d0))))))
  
(start)
