(ql:quickload :CLinch)
(ql:quickload :CLinch-sdl)
(use-package :clinch)

(defun start ()

  (let ((vertex-shader-source
	 "
#version 120

uniform  float alpha;
attribute vec3 vertexColor;
varying   vec4 fragmentColor;

void main() {
            gl_Position = ftransform();
            fragmentColor = vec4(vertexColor, alpha);
        }")


	(fragment-shader-source
	 "
#version 120
varying vec4 fragmentColor;
void main() {
            gl_FragColor = fragmentColor;
        }")
	(alpha 1))

    (window :title "Tutorial 3"
	    (viewport :clear-color '(0 0 0)
		      :projection-transform (clinch:make-orthogonal-transform (attribute *parent* 'width)
									      (attribute *parent* 'height)
									      .25 100)
		      (entity
			:shader (gl-shader
				  :vertex-shader-text vertex-shader-source
				  :fragment-shader-text fragment-shader-source
				  :attributes '(("vertexColor" :float))
				  :uniforms   '(("alpha" :float)))
			:indexes (buffer
				   :qtype :unsigned-int
				   :target :element-array-buffer
				   :Stride 1
				   :data '(0 1 2))
			:vertices (buffer
				    :Stride 3
				    :data '(   0.0  100.0 0.0
					    -100.0 -100.0 0.0
					    100.0 -100.0 0.0))
			:values   `((:attribute "vertexColor" ,(buffer 
								:Stride 3
								:data '(1.0 0.0 0.0 
									0.0 1.0 0.0 
									0.0 0.0 1.0)))
				    (:uniform "alpha" ,alpha)))))))

(start)

