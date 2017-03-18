(make-instance 'clinch:shader :name "shader"
	       :fragment-shader-text (alexandria:read-file-into-string (concatenate 'string *working-dir* "solid-phong-shader.frag"))
	       :vertex-shader-text (alexandria:read-file-into-string (concatenate 'string *working-dir* "solid-phong-shader.vert"))
	       :uniforms   '(("ambientLight" :float)
			     ("lightIntensity" :float)
			     ("lightDirection" :float)
			     ("color" :float)))
