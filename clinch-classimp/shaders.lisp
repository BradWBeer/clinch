;;;; clinch-classimp/shaders.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defparameter *simple-texture-shader* nil)
(defparameter *simple-color-shader* nil)


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


(defun make-simple-color-shader ()
  (or *simple-color-shader*
      (setf *simple-color-shader*
	    (let ((vert-source
		   "
#version 330

uniform mat4 P;
uniform mat4 M;
uniform vec4 color;

in vec3 v;
        void main() {
            gl_Position = P * M * vec4(v, 1);
        }")
	      
	      (frag-source
	       "
#version 330
uniform vec4 color;
out vec4 fragColor;
//layout (location = 0) out vec4 colorOut;

        void main() {
            fragColor = color;
        }"))

	  
	  (make-instance 'clinch:shader-program
			 :name "Shader01"
			 :vertex-shader-text vert-source
			 :fragment-shader-text frag-source
			 :uniforms '(("P" :matrix)
				     ("M" :matrix)
				     ("color" :float))
			 :attributes '(("v" :float)
				       ))))))

(defun make-entity-from-mesh (mesh)
  (let ((v (classimp:vertices mesh))
	(tan (classimp:tangents mesh))
	(n (classimp:normals mesh))
	(colors (classimp:colors mesh))
	(index (classimp:faces mesh))
	;;(material (classimp:material-index mesh))
	(tc (classimp:texture-coords mesh))
	(tc-comps (classimp:components-per-texture-coord mesh)))
    `(make-instance 'clinch:entity
		   :indexes ,(make-index-buffer index)
		   :values 
		   ,(remove-if-not (lambda (x) x)
				   `((:attribute "v" ,(make-vector-buffer v))
				     ,(when tan `(:attribute "tan" ,(make-vector-buffer tan)))
				     ,(when n `(:attribute "n" ,(make-vector-buffer n)))
				     ,(when (and colors (> (length colors) 0)) `(:attribute "colors" ,(make-vector-buffer colors)))
				     ,@(loop for i from 0 below (length tc)
					  collect (list :attribute 
							(format nil "tc~A" i)
							(make-vector-buffer (elt tc i) :stride (elt tc-comps i)))))))))
		   
	
