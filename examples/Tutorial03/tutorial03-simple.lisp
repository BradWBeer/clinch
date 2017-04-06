(ql:quickload :clinch)
(use-package :clinch)

;; The triangle entity which connects the shader, vertexes and projection matrix.
(defparameter *triangle* nil)

;; Run this once before the next on-idle call.
(clinch:defevent clinch:*next* ()
  (format t "initialization!~%")

  (gl:clear-color 0 0 0 0)

  ;; create the triangle entity. 
  (setf *triangle*
	(make-instance 'clinch:entity
		       
		       ;; Add the shader. This is a pre-made shader.
		       ;; You can look at the code (or pullg the data when loaded)
		       ;; to see what arguments it takes.
		       ;; Normally you don't need to specify a shader. It's done when
		       ;; you load a mesh.
		       :shader-program (get-generic-per-vertex-color-shader)                  

		       ;; Add the index buffer (which points and in what order)
		       ;; Buffers are GPU data which are held on the card. 
		       :indexes (make-instance 'clinch:index-buffer :data '(0 1 2))

		       ;; Uniform values stay the same across an entire mesh
		       :uniforms
		       
		       ;; Set the projection matrix (:projection is a special value, which uses
		       ;; the projection matrix passed to the entity. "P" is just the name I picked.
		       `(("P" . :projection)

			 ;; Sets the model matrix (:model is also special) from the parent "node",
			 ;; in this case root.
			 ("M" . :model))
		       
		       ;; Attributes are per point values.
		       :attributes

		       ;; v stands for vertexes. This is a GPU side buffer with 3 points (1, 2 & 3 in the index buffer above)
		       `(("v" . ,(make-instance 'clinch:buffer              
						:data '( 0.0  100.0 0.0
							-100.0 -100.0 0.0
							100.0 -100.0 0.0)))

			 ;; color of each vertex, also in a buffer
			 ("colors" . ,(make-instance 'clinch:buffer
						     :data '(1.0 0.0 0.0
							     0.0 1.0 0.0
							     0.0 0.0 1.0))))

		       ;; Add it to the *root* node. 
		       :parent *root*)))

;; Create an on-idle envent handler.
(clinch:defevent clinch:*on-idle* ()
  
  ;; clear the window
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  ;; Render the triangle using the orthogonal projection (per pixel coordinates) 
  (clinch:render *triangle* :projection *ortho-projection*))

;; Start the window.
(clinch:init)
