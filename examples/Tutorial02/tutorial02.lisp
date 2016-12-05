(ql:quickload :clinch)

;; The projection matrix. 
(defparameter *projection* nil)

;; The triangle entity which connects the shader, vertexes and projection matrix.
(defparameter *triangle* nil)

;; The shader program. A gpu side program which writes to the screen.
(defparameter *shader-program* nil)


(defun make-shader-program ()
  ;; A normal shader program is made of two shaders, a vertex shader and a fragment shader.
  (let ((vert-source
	       "
#version 130
// Vertex Shader Source Code

// A UNIFORM is single value which is passed to all programs in a run.
uniform mat4 P;

// An ATTRIBUTE is an array which gives one value for each vertex.
in vec3 v;

        void main() {
            gl_Position = P * vec4(v, 1);
        }")
	      (frag-source
	       "
#version 130
// Fragment Shader Source Code

// This returns the fragment's color.
out vec4 fragColor;
        void main() {

            // The triangle will just be white for now.
            fragColor = vec4(1, 1, 1, 1);
        }"))

	  (make-instance 'clinch:shader-program
			 :name "Shader01"
			 :vertex-shader vert-source
			 :fragment-shader frag-source
			 :uniforms '(("P" :matrix))
			 :attributes '(("v" :float)))))

;; Create a window-resized event handler
(clinch:defevent clinch:*on-window-resized* (win width height ts)
  (format t "Window Resized: ~A ~A~%" width height)
  (setf *projection* (clinch:make-orthogonal-transform width height .25 1000)))

;; Run this once before the next on-idle call.
(clinch:defevent clinch:*next* ()
  (format t "initialization!~%")

  ;; Set the window's clear color. I like blue.
  (gl:clear-color 0 0 1 0)

  ;; Enable a few opengl features. 
  (gl:enable :blend :depth-test :line-smooth :point-smooth :texture-2d :cull-face)

  ;; Set the blending mode. 
  (%gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:polygon-mode :front-and-back :fill)

  ;; Make the shader program.
  (setf *shader-program* (make-shader-program))

  ;; create the triangle entity. 
  (setf *triangle*
	(make-instance 'clinch:entity
		       :shader-program *shader-program*                                   ;; Add the shader
		       :indexes (make-instance 'clinch:index-buffer :data '(0 1 2))       ;; Add the index buffer
		       :uniforms   `(("P" . :projection))                                 ;; Set the projection matrix (:projection is a special value, which uses
		                                                                          ;; the projection matrix passed to the entity. "P" is just the name I picked.

		       :attributes   `(("v" . ,(make-instance 'clinch:buffer              ;; Set the vertex buffer, "v" is just the name I picked.
							      :Stride 3                   ;; Stride 3, there are 3 float (x, y, z) for each vertex.
							      :data '( 0.0  100.0 0.0
								      -100.0 -100.0 0.0
								      100.0 -100.0 0.0)))))))
;; Create an on-idle envent handler.
(clinch:defevent clinch:*on-idle* ()
  
  ;; clear the window
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  ;; Render the triangle.
  (clinch:render *triangle* :projection *projection*))

;; Start the window.
(clinch:init)


