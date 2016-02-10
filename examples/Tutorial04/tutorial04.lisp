(ql:quickload :clinch)

;; The projection matrix. 
(defparameter *projection* nil)

;; The triangle entity which connects the shader, vertexes and projection matrix.
(defparameter *cube* nil)

;; The shader program. A gpu side program which writes to the screen.
(defparameter *shader-program* nil)

;; Lighting values

;; ambientLight   The lowest amount of light to use. An RGB value.
(defparameter ambientLight '(.2 .2 .2))

;; lightIntensity The maximum power of the light.    An RGB value.
(defparameter lightIntensity '(.8 .8 .8))
       
;; lightDirection The direction of the light source. An XYZ normal value.
(defparameter lightDirection '(-0.5772705 -0.5772705 -0.5772705))

(defparameter *cube-node* nil)

(defun make-shader-program ()
  ;; A normal shader program is made of two shaders, a vertex shader and a fragment shader.

  (let ((vertex-shader-source
	"
#version 330

uniform   mat4 P;
uniform   mat3 N;
uniform   vec3 ambientLight;
uniform   vec3 lightDirection;
uniform   vec3 lightIntensity;

in  vec3 v;
in  vec3 n;
in  vec3 vertexColor;

out vec4 fragmentColor;

void main() {
            gl_Position = P * vec4(v, 1);

            float power = max(dot(-lightDirection,
                                  normalize(N * n)),
                              0);
            fragmentColor = vec4(vertexColor * (ambientLight + (lightIntensity * power)), 1.0);
}")
       
       
       (fragment-shader-source
	"
#version 330
in vec4 fragmentColor;
void main() {
            gl_FragColor = fragmentColor;
        }"))


    (setf *shader-program*
	  (make-instance 'clinch:shader-program
		   :name "Shader01"
		   :vertex-shader vertex-shader-source
		   :fragment-shader fragment-shader-source
		   :uniforms '(("P" :matrix)                ;; Current projection matrix
			       ("N" :matrix)                ;; Current normal matrix
			       ("ambientLight" :float)      ;; Ambient light
			       ("lightIntensity" :float)    ;; Brightness
			       ("lightDirection" :float))   ;; Direction

		   :attributes '(("v" :float)               ;; Vertices
				 ("n" :float)               ;; Normals
				 ("vertexColor" :float)))))) ;; Colors
		   


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
  (gl:enable :blend :depth-test :line-smooth :point-smooth :texture-2d :cull-face)

  ;; Set the blending mode. 
  (%gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:polygon-mode :front-and-back :fill)

  ;; Make the shader-program
  (setf *shader-program* (make-shader-program))

  ;; create the triangle entity. 
  (setf *cube*
	(make-instance 'clinch:entity
		       :shader-program *shader-program*                                   ;; Add the shader
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
		                                                                         ;; the projection matrix passed to the entity. "P" is just the name I picked.
				     ("N" . :normal)                                       ;; Current normal matrix
				     ("ambientLight" . (lambda () ambientLight))      ;; Ambient light
				     ("lightIntensity" . (lambda () lightIntensity))    ;; Brightness
				     ("lightDirection" . (lambda () lightDirection)))   ;; Direction


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
;; Create an on-idle envent handler.
(clinch:defevent clinch:*on-idle* ()
  
  ;; clear the window
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  ;; Render the triangle.
  (clinch:render *cube* :parent (m4:identity) :projection *projection*))

;; Start the window.
(clinch:init)


