(let ((vert-source
       "
#version 120

uniform sampler2D t1;
attribute vec2 tc1;
varying vec2 v_tc1;
        void main() {
            gl_Position = ftransform(); //gl_ModelViewProjectionMatrix * gl_Vertex;
            v_tc1 = tc1;
        }")
      
      ;; String for the Fragment Shader
      ;;   t1    is the texture sampler
      ;;   v_tc1 is the texture coordinates from the fragment shader
      (frag-source
       "
#version 120
uniform sampler2D t1;
varying vec2 v_tc1;
        void main() {
            gl_FragColor = texture2D(t1, v_tc1);
        }"))

  

  (make-instance 'clinch:shader
		 :name "Shader01"
		 :vertex-shader-text vert-source
		 :fragment-shader-text frag-source
		 :uniforms '(("t1" :int))
		 :attributes '(("tc1" :float))))




