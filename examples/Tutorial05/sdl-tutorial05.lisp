(ql:quickload :lispbuilder-sdl)
(ql:quickload :clinch)
(ql:quickload :clinch-cairo)

(defvar viewport)
(defvar projection-matrix)

(defvar node)
(defvar frame-count)


;; shader
(defvar shader)

;; cube entity
(defvar cube)

;; cube index buffer
(defvar cube-indices-buffer)

;; cube vertexes
(defvar cube-point-buffer)

;; cube normals
(defvar cube-normal-buffer)

;; ambientLight   The lowest amount of light to use. An RGB value.
(defvar ambientLight '(.2 .2 .2))

;; lightIntensity The maximum power of the light.    An RGB value.
(defvar lightIntensity '(.8 .8 .8))

;; lightDirection The direction of the light source. An XYZ normal value.
(defvar lightDirection '(0.5772705 0.5772705 0.5772705))

;; texture01 is the texture
(defvar texture01)

;; textureCoord is the texture coordinate value of the vertex
(defvar cube-texture-coordinate-buffer)

;; fragmentColor takes the vertexColors, mixes them by distance and sends them to the fragment shader
(defvar vertex-shader-source
  "
#version 120

uniform   sampler2D texture01;

uniform   vec3      ambientLight;
uniform   vec3      lightDirection;
uniform   vec3      lightIntensity;

attribute vec2      textureCoord;

varying   vec3      vLightIntensity;
varying   vec3      vAmbientLight;
varying   vec2      vTextureCoord;


void main() {
            gl_Position = ftransform();

            vLightIntensity = max(dot(lightDirection,
                                       normalize(gl_NormalMatrix * gl_Normal)),
                                  0) * lightIntensity;

            vAmbientLight = ambientLight;
            vTextureCoord = textureCoord;
}")


(defvar fragment-shader-source
  "
#version 120

uniform   sampler2D texture01;
varying   vec3      vLightIntensity;
varying   vec3      vAmbientLight;
varying   vec2      vTextureCoord;


void main() {
             gl_FragColor = texture2D(texture01, vTextureCoord) * vec4(vAmbientLight + vLightIntensity, 1);
        }")


(defun init ()

  (setf frame-count 0)

  (setf viewport (make-instance 'clinch:viewport))

  (gl:enable :blend
	     :depth-test
	     :line-smooth
	     :point-smooth
	     :polygon-smooth
	     :texture-2d
	     :cull-face)
  (%gl:blend-func :src-alpha :one-minus-src-alpha)

  (setf node (make-instance 'clinch:node))


  (setf shader (make-instance 'clinch:shader
                  :name "shader"
                  :vertex-shader-text vertex-shader-source
                  :fragment-shader-text fragment-shader-source
                  :attributes '(("textureCoord" :float))
                  :uniforms   '(("texture01" :int)
				("ambientLight" :float)
                                ("lightIntensity" :float)
				("lightDirection" :float))))
  
  (setf cube-indices-buffer 
    (make-instance 'clinch:buffer
		   :qtype :unsigned-int
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
  
  (setf cube-point-buffer 
    (make-instance 'clinch:buffer 
		   :stride 3
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

  (setf cube-normal-buffer 
    (make-instance 'clinch:buffer 
		   :stride 3
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

  (setf cube-texture-coordinate-buffer 
    (make-instance 'clinch:buffer 
		   :stride 2
		   :data '(0.0 1.0
			   1.0 1.0
			   0.0 0.0
			   1.0 0.0
			   0.0 1.0
			   1.0 1.0
			   0.0 0.0
			   1.0 0.0
			   0.0 1.0
			   1.0 1.0
			   0.0 0.0
			   1.0 0.0
			   0.0 1.0
			   1.0 1.0
			   0.0 0.0
			   1.0 0.0
			   0.0 1.0
			   1.0 1.0
			   0.0 0.0
			   1.0 0.0
			   0.0 1.0
			   1.0 1.0
			   0.0 0.0
			   1.0 0.0)))

  (setf texture01
	(clinch::create-texture-from-png (concatenate 'string 
						      (directory-namestring
						       (asdf:system-relative-pathname :clinch "clinch.asd"))
						      "examples/Tutorial05/lambda.png")))

  (setf cube 
	(make-instance 'clinch:entity
		       :parent node
		       :shader shader
		       :indexes cube-indices-buffer 
		       :values `((:vertices ,cube-point-buffer)
				 (:attribute "textureCoord" ,cube-texture-coordinate-buffer)
				 (:normals ,cube-normal-buffer)
				 (:uniform "texture01" ,texture01)
				 (:uniform "ambientLight" ambientLight)
				 (:uniform "lightIntensity" lightIntensity)
				 (:uniform "lightDirection" lightDirection))
		       )))


(defun main-loop ()
  
  (incf frame-count 1) 

  (clinch:set-identity-transform node)
  (clinch:rotate node (clinch:degrees->radians (mod frame-count 360) ) 0 0.8942871 0.44714355)
  (clinch:translate node  0 0 -1.5)
  
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (clinch:render node))


(defun clean-up ()
  (clinch:unload cube-indices-buffer)
  (clinch:unload cube-point-buffer)
  (clinch:unload cube-normal-buffer)
  (clinch:unload cube-texture-coordinate-buffer)
  (clinch:unload shader))

(defun window-size-callback (width height)
  (clinch::quick-set viewport 0 0 width height)
  (clinch::render viewport)
  
  (setf projection-matrix (clinch::make-perspective-transform (/ clinch::+pi+ 4) (/ width height) .1 100))
  (print projection-matrix)

  (gl:matrix-mode :projection)
  (gl:load-matrix projection-matrix)
  (gl:matrix-mode :modelview)
  (gl:load-identity))


(defun start ()
  (declare (optimize (speed 3)))
  (sdl:with-init ()
    (sdl:window 300 400
		:title-caption "Tutorial 5"
		:icon-caption "Tutorial 5"
		:flags sdl-cffi::sdl-opengl
		:resizable t
		:double-buffer t)
    (setf (sdl:frame-rate) 60)
    (init)
    (window-size-callback 300 400)
    (sdl:with-events ()
      (:quit-event () t)
      (:VIDEO-RESIZE-EVENT (:W W :H H) 
			   (window-size-callback w h))
      (:idle ()         
	     (main-loop)
	     (sdl:update-display)))
  ;; End Program
    (clean-up)))
