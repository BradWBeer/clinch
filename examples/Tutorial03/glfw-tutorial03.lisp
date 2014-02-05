(ql:quickload :cl-glfw)
(ql:quickload :clinch)

(defvar shader)
(defvar alpha 0)

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

(defun init ()

  (setf alpha .5)

  (setf viewport (make-instance 'clinch:viewport))
  (glfw:set-window-size-callback #'window-size-callback)

  (gl:enable :blend :depth-test :line-smooth :point-smooth :polygon-smooth :texture-2d)
  (%gl:blend-func :src-alpha :one-minus-src-alpha)


  (setf shader (make-instance 'clinch:shader
				   :vertex-shader-text vertex-shader-source
				   :fragment-shader-text fragment-shader-source
				   :attributes '(("vertexColor" :float))
				   :uniforms   '(("alpha" :float))))


  (setf triangle-point-buffer 
	(make-instance 'clinch:buffer 
		       :Stride 3
		       :data '(   0.0  100.0 -1.0
			       -100.0 -100.0 -1.0
			       100.0 -100.0 -1.0)))

  (setf triangle-indices-buffer 
	(make-instance 'clinch:buffer :qtype :unsigned-int
		       :target :element-array-buffer
		       :Stride 1
		       :data '(0 1 2)))

  (setf triangle-color-buffer 
	(make-instance 'clinch:buffer 
		       :Stride 3
		       :data '(1.0 0.0 0.0 
			       0.0 1.0 0.0 
			       0.0 0.0 1.0)))
			       

  (setf triangle 
	(make-instance 'clinch:entity
		       :shader  shader
		       :indexes triangle-indices-buffer 
		       :values (list (list :vertices triangle-point-buffer)
				     (list :attribute "vertexColor" triangle-color-buffer)
				     (list :uniform "alpha" alpha)))))

  
(defun main-loop ()
  (setf alpha (mod (+ alpha 1/60) 1.1))
  
  
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (clinch:render triangle))

  
(defun clean-up ()
  (clinch:unload triangle-point-buffer)
  (clinch:unload triangle-indices-buffer)
  (clinch:unload shader))

(defun window-size-callback (width height)
  (clinch::quick-set viewport 0 0 width height)
  (clinch::render viewport)
  
  (setf projection-matrix (clinch:make-orthogonal-transform width height .25 100))
  (print projection-matrix)

  (gl:matrix-mode :projection)
  (gl:load-matrix projection-matrix)
  (gl:matrix-mode :modelview)
  (gl:load-identity))


(defun start ()
  (declare (optimize (speed 3)))
  (glfw:do-window (:title "Tutorial 1"
			  :redbits 8
			  :greenbits 8
			  :bluebits 8
			  :alphabits 8
			  :depthbits 16)
      ((init))
    
    (main-loop))
  
  ;; End Program
  (clean-up))
