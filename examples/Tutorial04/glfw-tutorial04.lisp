(ql:quickload :CLinch)
(ql:quickload :CLinch-glfw)
(use-package :clinch)

(defun start ()

  (let
      ;; ambientLight   The lowest amount of light to use. An RGB value.
      ((ambientLight '(.2 .2 .2))
       
       ;; lightIntensity The maximum power of the light.    An RGB value.
       (lightIntensity '(.8 .8 .8))
       
       ;; lightDirection The direction of the light source. An XYZ normal value.
       (lightDirection '(-0.5772705 -0.5772705 -0.5772705))
       
       
       (vertex-shader-source
	"
#version 120

uniform   vec3 ambientLight;
uniform   vec3 lightDirection;
uniform   vec3 lightIntensity;
attribute vec3 vertexColor;
varying   vec4 fragmentColor;

void main() {
            gl_Position = ftransform();

            float power = max(dot(-lightDirection,
                                  normalize(gl_NormalMatrix * gl_Normal)),
                              0);
            fragmentColor = vec4(vertexColor * (ambientLight + (lightIntensity * power)), 1.0);
}")
       
       
       (fragment-shader-source
	"
#version 120
varying vec4 fragmentColor;
void main() {
            gl_FragColor = fragmentColor;
        }"))

    (window :title "Tutorial 4"
	    (viewport :clear-color '(0 0 0)
		      :projection-transform (clinch::make-perspective-transform (clinch:degrees->radians 65) (/ (attribute *parent* 'width)
														(attribute *parent* 'height)) .1 100)
		      (node
			:once (lambda (this) (clinch:translate this  0 0 -1.5))

			(node
			  :before-render (lambda (this)
					   (rotate this (radians->degrees .001) 0 0.8942871 0.44714355))
			  
			  
			  (entity
			    :shader (gl-shader
				      :name "shader"
				      :vertex-shader-text vertex-shader-source
				      :fragment-shader-text fragment-shader-source
				      :attributes '(("vertexColor" :float))
				      :uniforms   '(("ambientLight" :float)
						    ("lightIntensity" :float)
						    ("lightDirection" :float)))
			    :indexes (buffer
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
					       22 21 23))
			    :vertices (buffer
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
						-0.5  0.5  0.5))
			    :normals (buffer :Stride 3
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
						     -1.0 0.0 0.0))
			    :values   `((:uniform "ambientLight"   ,ambientLight)
					(:uniform "lightIntensity" ,lightIntensity)
					(:uniform "lightDirection" ,lightDirection)
					(:attribute "vertexColor"  ,(buffer 
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
									     1.0 0.0 1.0)))))))))))

