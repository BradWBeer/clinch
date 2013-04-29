(ql:quickload :lispbuilder-sdl)
(ql:quickload :clinch)


(defvar shader)
(defvar alpha)

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
		       :values `((:vertices ,triangle-point-buffer)
				 (:attribute "vertexColor" ,triangle-color-buffer)
				 (:uniform "alpha" alpha)))))

  
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
  (sdl:with-init ()
    (sdl:window 400 300
		:flags sdl-cffi::sdl-opengl
		:resizable t
		:double-buffer t
		:title-caption "Tutorial 1"
		:icon-caption "Tutorial 1")
    (init)
    (window-size-callback 400 300)
    (setf (sdl:frame-rate) 60)
    (sdl:with-events ()
      (:quit-event () t)
      (:VIDEO-RESIZE-EVENT (:W W :H H) 
			   (window-size-callback w h))
      (:idle ()         
	     (main-loop)
	     (sdl:update-display)))
    (clean-up)))


