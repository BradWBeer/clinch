(ql:quickload :cl-glfw)
(ql:quickload :clinch)

(defvar alpha)
(defvar frame-count)

;; alpha is the amount of alpha to use
;; vertexColor is the color value of the vertex
;; fragmentColor takes the vertexColors, mixes them by distance and sends them to the fragment shader
(defvar vertex-shader-source
  "
#version 120

uniform  float alpha;
attribute vec3 vertexColor;
varying   vec4 fragmentColor;

void main() {
            gl_Position = ftransform();
            fragmentColor = vec4(vertexColor, alpha);
        }")


(defvar fragment-shader-source
  "
#version 120
varying vec4 fragmentColor;
void main() {
            gl_FragColor = fragmentColor;
        }")


(defvar viewport)
(defvar projection-matrix)

(defvar triangle)
(defvar triangle-point-buffer)
(defvar triangle-indices-buffer)
(defvar triangle-color-buffer)

(defvar node)

(defun init ()

  (setf alpha 1.0)
  (setf frame-count 0)

  (setf viewport (make-instance 'clinch:viewport))
  (glfw:set-window-size-callback #'window-size-callback)

  (gl:enable :blend :depth-test :line-smooth :point-smooth :polygon-smooth :texture-2d)
  (%gl:blend-func :src-alpha :one-minus-src-alpha)


  (setf shader (make-instance 'clinch:shader
			      :name "shader"
			      :vertex-shader-text vertex-shader-source
			      :fragment-shader-text fragment-shader-source
			      :attributes '(("vertexColor" :float))
			      :uniforms   '(("alpha" :float))))


  (setf triangle-point-buffer 
	(make-instance 'clinch:buffer 
		       :Stride 3
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

  (setf triangle-indices-buffer 
	(make-instance 'clinch:buffer :qtype :unsigned-int
		       :target :element-array-buffer
		       :Stride 1
		       :data '(0  1  2 
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
			       22 21 23)))

  (setf triangle-color-buffer 
	(make-instance 'clinch:buffer 
		       :Stride 3
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
			       1.0 0.0 1.0)))
  
  (setf node (make-instance 'clinch:node))
  
    (setf triangle 
	(make-instance 'clinch:entity
		       :parent node
		       :shader  shader
		       :indexes triangle-indices-buffer 
		       :values `((:vertices ,triangle-point-buffer)
				 (:attribute "vertexColor" ,triangle-color-buffer)
				 (:uniform "alpha" alpha)))))


(defun main-loop ()

  (incf frame-count) 

  (clinch:set-identity-transform node)
  (clinch:rotate node (clinch:degrees->radians (mod frame-count 360) ) 0 0.8942871 0.44714355)
  (clinch:translate node  0 0 -1.5)
  
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (clinch:render node))


(defun clean-up ()
  (clinch:unload triangle-point-buffer)
  (clinch:unload triangle-indices-buffer)
  (clinch:unload shader))

(defun window-size-callback (width height)
  (clinch::quick-set viewport 0 0 width height)
  (clinch::render viewport)
  
  (setf projection-matrix (clinch::make-perspective-transform (clinch:degrees->radians 65) (/ width height) .1 100))
  (print projection-matrix)

  (gl:matrix-mode :projection)
  (gl:load-matrix projection-matrix)
  (gl:matrix-mode :modelview)
  (gl:load-identity))


(defun start ()
  (declare (optimize (speed 3)))
  (glfw:do-window (:title "Tutorial 4"
			  :redbits 8
			  :greenbits 8
			  :bluebits 8
			  :alphabits 8
			  :depthbits 16)
    ((init))
    
    (main-loop))
  
  ;; End Program
  (clean-up))
