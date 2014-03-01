(ql:quickload :CLinch)
(ql:quickload :CLinch-glfw)
(ql:quickload :clinch-cairo)

(use-package :clinch)

(defun start ()

  (let
      ;; ambientLight   The lowest amount of light to use. An RGB value.
      ((ambient-light '(.2 .2 .2))
       
       ;; lightIntensity The maximum power of the light.    An RGB value.
       (light-intensity '(.8 .8 .8))
       
       ;; lightDirection The direction of the light source. An XYZ normal value.
       (light-direction '(0.5772705 0.5772705 0.5772705))
       
       
       (vertex-shader-source
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
       
       (fragment-shader-source
	"
#version 120

uniform   sampler2D texture01;
varying   vec3      vLightIntensity;
varying   vec3      vAmbientLight;
varying   vec2      vTextureCoord;


void main() {
             gl_FragColor = texture2D(texture01, vTextureCoord) * vec4(vAmbientLight + vLightIntensity, 1);
        }"))

    (window :title "Tutorial 5"
	    (viewport :clear-color '(0 0 0)
		      :projection-transform (clinch::make-perspective-transform (/ clinch::+pi+ 4) (/ (attribute *parent* 'width)
												      (attribute *parent* 'height)) .1 100)
		      (node
			:once (lambda (this) (clinch:translate this  0 0 -1.5))

			(node
			  :before-render (lambda (this)
					   (rotate this (* 2 (coerce PI 'single-float) *delta-time* 1/5) 0 0.8942871 0.44714355))
			  
			  
			  (entity
			    :shader (gl-shader
				      :name "shader"
				      :vertex-shader-text vertex-shader-source
				      :fragment-shader-text fragment-shader-source
				      :attributes '(("textureCoord" :float))
				      :uniforms   '(("texture01" :int)
						    ("ambientLight" :float)
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
			    :values   `((:uniform "ambientLight"   ,ambient-light)
					(:uniform "lightIntensity" ,light-intensity)
					(:uniform "lightDirection" ,light-direction)
					(:uniform "texture01"      ,(clinch::create-texture-from-png
								     (concatenate 'string 
										  (directory-namestring
										   (asdf:system-relative-pathname :clinch "clinch.asd"))
										  "examples/Tutorial05/lambda.png")))
					(:attribute "textureCoord"  ,(buffer 
								     :Stride 2
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
									     1.0 0.0)))))))))))
(start)


