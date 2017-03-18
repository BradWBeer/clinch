(ql:quickload :clinch)

;; Load the clinch freeimage plugin to load a file
;; I could have clinch-cairo too, since it has create-texture-from-png,
;; and I'm using a png file.
(ql:quickload :clinch-freeimage)

;; Var to hold the square entityl
(defparameter *quad-mesh*  nil)

;; var to hold the texture;
(defparameter *texture* nil)

;; var to hold the node.
(defparameter *node* nil)

;; var to hold the projection matrix
(defparameter *projection* nil)

;; Creates the texture. 
;; In a real program, I would load these 
;; from a file.
;; You can see this when it's running 
;; using (clinch:pullg *simple-texture-shader*)
;; It will also show the input variables.


;; Initialize the test. 
(defun init-test ()
  
  ;; Create the entity (mesh)
  (setf *quad-mesh*
	(make-instance 'clinch:entity

		       ;; shader-program to use
		       :shader-program  (clinch:get-generic-single-texture-shader)

		       ;; Index values. 
		       :indexes (make-instance 'clinch:index-buffer :data '(0 1 2 0 2 3))

		       ;; List of attributes.
		       ;; "v" is a buffer which holds the vertices.
		       ;; "tc1" is a buffers which holds the texture coordinates.
		       :attributes   `(("v" . ,(make-instance 'clinch:buffer 
							      :Stride 3
							      :data (map 'list (lambda (x)
										 (coerce x 'single-float))
									 '( -1   1 0
									   -1  -1 0
									   1  -1 0
									   1   1 0))))
				       ("tc1" . ,(make-instance 'clinch:buffer 
								:Stride 2
								:data (map 'list (lambda (x)
										   (coerce x 'single-float))
									   '(0.0   1.0
									     0.0   0.0
									     1.0   0.0
									     1.0   1.0)))))
		       :uniforms `(("M" . :model)                     ;; :model is a special value which inserts the current model matrix.
				   ("P" . :projection)                ;; :projection is a special value which inserts the current projection matrix.
				   ("t1" . :place-holder)))) ;; Texture value, a place holder for now.

  ;; Creates the texture to use.
  (setf *texture*
	(clinch::make-texture-from-file 
	 (concatenate 'string 
		      (directory-namestring
		       (asdf:system-relative-pathname :clinch "clinch.asd"))
		      "examples/assets/img/lambda.png")))

  (setf (clinch:uniform *quad-mesh* "t1") *texture*)
  
  ;; I could have put this inside the quad's attributes, but I wanted to 
  ;; demonstrate setting a uniform. Attributes work the same way. 
  (setf (clinch:uniform *quad-mesh* "ambientTexture") (lambda () *texture*))

  ;; Create a node so I can move the quad around.
  ;; :Children takes a list.
  (setf *node* (make-instance 'clinch:node :children (list *quad-mesh*)))

  ;; Move the node so it's in front of the camera.
  (clinch:translate *node* (clinch:v! 0 0 -2)))


;; Next runs one time before the next on-idle.
(clinch:defevent clinch:*next* ()

  ;; Enable a few opengl features. 
  (gl:enable :blend :depth-test :line-smooth :point-smooth :texture-2d :cull-face)
  (%gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:polygon-mode :front-and-back :fill)

  (gl:clear-color 0 0 1 0)

  ;; Initialize 
  (init-test))


;; Main loop
(clinch:defevent clinch:*on-idle* ()

  ;; clear the screen
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  ;; render the root node which renders everything under it.
  (clinch:render *node* :projection *projection*))

;; Handler for when the mouse moves...
(clinch:defevent clinch:*on-mouse-move* (win mouse state x y xrel yrel ts)
  ;;(format t "x:~A y:~A mouse:~A state:~A~%" x y mouse state)

  ;; state is an OR'ed array of press mouse buttons.
  (case state
    ;; Left mouse button down: rotate.
    (1 (clinch:rotate *node*
		      (q:from-fixed-angles (clinch:degrees->radians yrel) (clinch:degrees->radians xrel) 0)))
    
    ;; Mouse wheel down: translate.
    (2 (clinch:translate *node* (clinch:v! (/ xrel 2) (/ yrel -2) 0)))))


;; Resize the window and change the projection matrix.    
(clinch:defevent clinch:*on-window-resized* (win width height ts)
  (format t "Resized: ~A ~A~%" width height)
  
  (setf *projection* (clinch::make-perspective-transform (clinch:degrees->radians 45)
							 (/ width height) .1 1000)))

;; When the mouse wheel moves, translate forward or backward.
(clinch:defevent clinch:*on-mouse-wheel-move* (win mouse x y ts)
  ;;(format t "win=~A mouse=~A x=~A y=~A ts=~A~%" win mouse x y ts)
  (clinch:translate *node* (clinch:v! 0 0 (/ y 1))))

;; Create the window and start the app.
;; controllers keep the window from restarting, 
;; so I removed them for now.
(clinch:init :init-controllers nil :title "Tutorial05")
