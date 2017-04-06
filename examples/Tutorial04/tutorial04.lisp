(ql:quickload :clinch)

;; The projection matrix. 
(defparameter *projection* nil)

;; The triangle entity which connects the shader, vertexes and projection matrix.
(defparameter *cube* nil)

;; Lighting values
;; ambientLight   The lowest amount of light to use. An RGB value.
(defparameter ambientLight '(.2 .2 .2))

;; lightIntensity The maximum power of the light.    An RGB value.
(defparameter lightIntensity '(.8 .8 .8))
       
;; lightDirection The direction of the light source. An XYZ normal value.
(defparameter lightDirection '(0.5772705 -0.5772705 -0.5772705))

(defparameter *cube-node* nil)



;; Create a window-resized event handler 
(clinch:defevent clinch:*on-window-resized* (win width height ts)
  (format t "Window Resized: ~A ~A~%" width height)
  (setf *projection* (clinch::make-perspective-transform (clinch:degrees->radians 45)
							 (/ width height) .1 1000)))


;; Run this once before the next on-idle call.
(clinch:defevent clinch:*next* ()

  (format t "initialization!~%")

  ;; Set the window's clear color. I like blue.
  (gl:clear-color 0 0 0 0)

  ;; Enable a few opengl features. 
  (gl:enable :blend :depth-test :line-smooth :point-smooth :texture-2d)

  ;; Set the blending mode. 
  (%gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:polygon-mode :front-and-back :fill)

  ;; create the triangle entity. 
  (setf *cube*
	(make-instance 'clinch:entity
		       :shader-program  (clinch::get-generic-single-diffuse-light-per-vertex-color-shader)                                   ;; Add the shader
		       :indexes (make-instance 'clinch:index-buffer :data '(0  1  2       ;; Add the index buffer
									    2  1  3
									    4  5  6
									    6  5  7
									    8  9 10 
									    10  9 11
									    12 13 14 
									    14 13 15
									    16 17 18 
									    18 17 19
									    20 21 22 
									    22 21 23))   
		       :uniforms   `(("P" . :projection)                                 ;; Set the projection matrix (:projection is a special value, which uses 
				     ("M" . :model);; the projection matrix passed to the entity. "P" is just the name I picked.
				     ("N" . :normal)                                       ;; Current normal matrix
				     ("ambientLight"  . ,(lambda () ambientLight))      ;; Ambient light
				     ("lightIntensity" . ,(lambda () lightIntensity))    ;; Brightness
				     ("lightDirection" . ,(lambda () lightDirection)))   ;; Direction


		       :attributes   `(("v" . ,(make-instance 'clinch:buffer              ;; Set the vertex buffer, "v" is just the name I picked.
							      :Stride 3                   ;; Stride 3, there are 3 float (x, y, z) for each vertex.
							      :data '(-0.5 -0.5  0.5
						0.5 -0.5  0.5
						-0.5  0.5  0.5
						0.5  0.5  0.5
						-0.5 -0.5 -0.5
						0.5 -0.5 -0.5
						-0.5 -0.5  0.5
						0.5 -0.5  0.5
						-0.5  0.5 -0.5
						0.5  0.5 -0.5
						-0.5 -0.5 -0.5
						0.5 -0.5 -0.5
						-0.5  0.5  0.5
						0.5  0.5  0.5
						-0.5  0.5 -0.5
						0.5  0.5 -0.5
						0.5 -0.5  0.5
						0.5 -0.5 -0.5
						0.5  0.5  0.5
						0.5  0.5 -0.5
						-0.5 -0.5 -0.5 
						-0.5 -0.5  0.5 
						-0.5  0.5 -0.5 
						-0.5  0.5  0.5)))
				       ("n" . ,(make-instance 'clinch:buffer              ;; Set the vertex buffer, "v" is just the name I picked.
							      :Stride 3                   ;; Stride 3, there are 3 float (x, y, z) for each vertex.
							      :data '(0.0 0.0 1.0
								      0.0 0.0 1.0
								      0.0 0.0 1.0
								      0.0 0.0 1.0
								      0.0 -1.0 0.0
								      0.0 -1.0 0.0
								      0.0 -1.0 0.0
								      0.0 -1.0 0.0
								      0.0 0.0 -1.0
								      0.0 0.0 -1.0
								      0.0 0.0 -1.0
								      0.0 0.0 -1.0
								      0.0 1.0 0.0
								      0.0 1.0 0.0
								      0.0 1.0 0.0
								      0.0 1.0 0.0
								      1.0 0.0 0.0
								      1.0 0.0 0.0
								      1.0 0.0 0.0
								      1.0 0.0 0.0
								      -1.0 0.0 0.0
								      -1.0 0.0 0.0
								      -1.0 0.0 0.0
								      -1.0 0.0 0.0)))
				       ("vertexColor" . ,(make-instance 'clinch:buffer         ;; Create a buffer to hold the color data. "colors" is just the name I picked.
								   :stride 3              ;; There are three floats (red, green, blue) for each vertex.
								   :data '(1.0 0.0 0.0
									     1.0 0.0 0.0
									     1.0 0.0 0.0
									     1.0 0.0 0.0
									     0.0 1.0 0.0
									     0.0 1.0 0.0
									     0.0 1.0 0.0
									     0.0 1.0 0.0
									     1.0 1.0 0.0
									     1.0 1.0 0.0
									     1.0 1.0 0.0
									     1.0 1.0 0.0
									     0.0 1.0 1.0
									     0.0 1.0 1.0
									     0.0 1.0 1.0
									     0.0 1.0 1.0
									     0.0 0.0 1.0
									     0.0 0.0 1.0
									     0.0 0.0 1.0
									     0.0 0.0 1.0
									     1.0 0.0 1.0
									     1.0 0.0 1.0
									     1.0 0.0 1.0
									     1.0 0.0 1.0))))))

  (setf *cube-node* (make-instance 'clinch:node :children (list *cube*) :translation (clinch:v! 0 0 -2)))
  )

(clinch:defevent clinch:*on-mouse-move* (win mouse state x y xrel yrel ts)
  ;;(format t "x:~A y:~A mouse:~A state:~A~%" x y mouse state)
  (case state
    (1 (clinch:rotate *cube-node*
		      (q:from-fixed-angles (clinch:degrees->radians yrel) (clinch:degrees->radians xrel) 0.0)))
    
    (2 (clinch:translate *cube-node* (clinch:v! (/ xrel 2) (/ yrel -2) 0)))))

(clinch:defevent clinch:*on-mouse-wheel-move* (win mouse x y ts)
  ;;(format t "win=~A mouse=~A x=~A y=~A ts=~A~%" win mouse x y ts)
  (clinch:translate *cube-node* (clinch:v! 0 0 (/ y 1))))


;; Create an on-idle envent handler.
(clinch:defevent clinch:*on-idle* ()
  
  ;; clear the window
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  ;; rotate the cube's node.
  ;; (clinch:rotate *cube-node*
  ;; 		 (q:from-fixed-angles 0 0
  ;; 				      (clinch:degrees->radians 2))) 


  ;; Render the triangle.
  (clinch:render *cube-node* :projection *projection*))

;; Start the window.
(clinch:init :init-controllers nil)


