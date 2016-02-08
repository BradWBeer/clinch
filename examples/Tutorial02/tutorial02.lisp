(ql:quickload :clinch)

;; the viewport sets or resets the window size.
(defparameter *viewport* nil) 

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
#version 330
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
#version 330
// Fragment Shader Source Code

// This returns the fragment's color.
out vec4 fragColor;
        void main() {

            // The triangle will just be white for now.
            fragColor = vec4(1, 1, 1, 1);
        }"))

	  (make-instance 'clinch:shader-program
			 :name "Shader01"
			 :vertex-shader-text vert-source
			 :fragment-shader-text frag-source
			 :uniforms '(("P" :matrix))
			 :attributes '(("v" :float)))))

(clinch:defevent clinch:*on-window-resized* (win width height ts)
  (format t "Window Resized: ~A ~A~%" width height)
  (clinch::quick-set *viewport* 0 0 width height)
  (clinch:render *viewport*)
  (setf *projection* (clinch:make-orthogonal-transform width height .25 1000))
  (clinch:render *viewport*))


(clinch:defevent clinch:*next* ()

  (format t "initialization!~%")

  ;; Set the window's clear color. I like blue.
  (gl:clear-color 0 0 1 0)

  ;; Enable a few opengl features. 
  (gl:enable :blend :depth-test :line-smooth :point-smooth :texture-2d :cull-face)

  ;; Set the blending mode. 
  (%gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:polygon-mode :front-and-back :fill)

  (setf *viewport* (make-instance 'clinch:viewport))

  (setf *shader-program* (make-shader-program))

  (setf *triangle*
	(make-instance 'clinch:entity
		       :shader-program *shader-program*
		       :indexes (make-instance 'clinch:index-buffer :data '(0 1 2))
		       :uniforms   `(("P" . :projection))
		       :attributes   `(("v" . ,(make-instance 'clinch:buffer 
							      :Stride 3
							      :data '( 0.0  100.0 0.0
								      -100.0 -100.0 0.0
								      100.0 -100.0 0.0)))))))

(clinch:defevent clinch:*on-idle* ()
  
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (clinch:render *triangle* :projection *projection*))

(clinch:init :init-controllers nil)


