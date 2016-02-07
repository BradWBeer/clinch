(ql:quickload :clinch)
(ql:quickload :clinch-pango)
(ql:quickload :clinch-freeimage)

(defparameter *quad-mesh*  nil)
(defparameter *texture-shader* nil)
(defparameter *texture* nil)
(defparameter *node* nil)
(defparameter *projection* nil)
(defparameter *viewport* nil)

(defparameter *simple-texture-shader* nil)

(defun make-simple-texture-shader ()
  (or *simple-texture-shader*
      (setf *simple-texture-shader*
	    (let ((vert-source
		   "
#version 330

uniform mat4 P;
uniform mat4 M;

uniform sampler2D ambientTexture;
in vec3 v;
in vec2 tc1;
out vec2 v_tc1;
        void main() {
            gl_Position = P * M * vec4(v, 1);
            v_tc1 = vec2(tc1.x, -tc1.y);
        }")
	      
	      ;; String for the Fragment Shader
	      ;;   t1    is the texture sampler
	      ;;   v_tc is the texture coordinates from the fragment shader
	      (frag-source
	       "
#version 330
uniform sampler2D ambientTexture;
in vec2 v_tc1;
out vec4 fragColor;
//layout (location = 0) out vec4 colorOut;

        void main() {
            fragColor = texture2D(ambientTexture, v_tc1);
        }"))

	  
	  (make-instance 'clinch:shader-program
			 :name "Shader01"
			 :vertex-shader-text vert-source
			 :fragment-shader-text frag-source
			 :uniforms '(("P" :matrix)
				     ("M" :matrix)
				     ("ambientTexture" :int))
			 :attributes '(("tc1" :float)
				       ("v" :float)
				       ))))))

(defun init ()
  (setf *quad-mesh*
	(make-instance 'clinch:entity
		       :indexes (make-instance 'clinch:index-buffer :data '(0 1 2 0 2 3))
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
		       :uniforms `(("M" . :model)
				   ("P" . :projection)
				   ("ambientTexture" . :int)
				   ("ambientLight" . (.2 .2 .2))
				   ("lightIntensity" . (.8 .8 .8))
				   ("lightDirection" . (0.5772705 0.5772705 0.5772705)))))
  
  (make-simple-texture-shader)
  (setf *texture*
	(clinch::create-texture-from-file
	 (concatenate 'string 
		      (directory-namestring
		       (asdf:system-relative-pathname :clinch "clinch.asd"))
		      "examples/Tutorial05/lambda.png")))
		       
  
  (setf (clinch:shader-program *quad-mesh*) *simple-texture-shader*)
  (setf (clinch:uniform *quad-mesh* "ambientTexture") (lambda () *texture*))
  (setf *node* (make-instance 'clinch:node :children (list *quad-mesh*)))
  (clinch:translate *node* (clinch:v! 0 0 -2)))


;; Next runs one time before the next on-idle.
(clinch:defevent clinch:*next* ()
  (init)
  (setf *viewport* (make-instance 'clinch:viewport))
  (gl:clear-color 0 0 1 0))

(clinch:defevent clinch:*on-idle* ()
  (clinch:rotate *node*
		 (q:from-fixed-angles 0 0
				      (clinch:degrees->radians 2))) 
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (clinch:render *node* :projection *projection*))

(clinch:defevent clinch:*on-mouse-move* (win mouse state x y xrel yrel ts)
  (format t "x:~A y:~A mouse:~A state:~A~%" x y mouse state))
    
(clinch:defevent clinch:*on-window-resized* (win width height ts)
  (format t "Resized: ~A ~A~%" width height)
  (clinch::quick-set *viewport* 0 0 width height)
  
  (setf *projection* (clinch::make-perspective-transform (clinch:degrees->radians 45)
							 (/ width height) .1 1000))
  (clinch:render *viewport*))

(clinch:defevent clinch:*on-mouse-wheel-move* (win mouse x y ts)
  (format t "win=~A mouse=~A x=~A y=~A ts=~A~%" win mouse x y ts)
  (clinch:translate *node* (clinch:v! 0 0 (/ y 1))))

(clinch:init :asynchronous t :init-controllers nil)

(let ((vs t))
  (defun toggle-vsync ()
    (clinch:! (if (setf vs (not vs))
		  (sdl2:gl-set-swap-interval 1)
		  (sdl2:gl-set-swap-interval 0)))))

