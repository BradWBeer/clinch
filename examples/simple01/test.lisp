(ql:quickload :clinch)
(ql:quickload :cl-glfw)

(setf vert-source
      "
#version 120

uniform sampler2D t1;
attribute vec2 tc1;
varying vec2 v_tc1;
        void main() {
            gl_Position = ftransform(); //gl_ModelViewProjectionMatrix * gl_Vertex;
              v_tc1 = tc1;

        }")

(setf frag-source
      "
#version 120
uniform sampler2D t1;
varying vec2 v_tc1;
        void main() {
            gl_FragColor = texture2D(t1, v_tc1);
        }")

(let ((viewport (make-instance 'clinch::viewport))
      (pipeline)
      (camera)
      (vertexes)
      (indexes)
      (shader)
      (tex-id)
      (texture)
      (tex-coord)
      (texture-size)
      (root)
      (box)
      (*root*)
      (*node1*)
      (*node2*)
      (rot (coerce (* 2 pi (/ 1 360)) 'single-float)))

  (defun window-key-press (key action)
    (clinch::with-mapped-buffer (bits texture)
      (let* ((surf (cairo:create-image-surface-for-data bits :argb32 (car texture-size) (cdr texture-size) (* 4 (car texture-size)))))
	(cairo:with-context ((cairo:create-context surf))
	  (cairo:save)
	  (cairo:set-source-rgba 0 0 0 0)
	  (cairo:set-operator :source)
	  (cairo:paint)
	  (cairo:restore)
	  (cairo:set-source-rgb 0 0 0)
	  (cairo:move-to 0 0)
	  (cairo:line-to (clinch:width texture) (clinch:height texture))
	  (cairo:move-to (clinch:width texture) 0)
	  (cairo:line-to 0 (clinch:height texture))
	  (cairo:set-line-width 50)
	  (cairo:stroke))
	(cairo:destroy surf))))


  (defun window-size-callback (width height)
    (format t "Resize called with: w=~A h=~A~%" width height)
    (clinch::quick-set viewport 0 0 width height)
    (clinch::render viewport)

    (setf camera (clinch::make-perspective-transform (/ (* 65 pi) 360) (/ width height) .5 100))
    (clinch::use-projection-transform camera)
    
  (defun start ()
    (declare (optimize (speed 3)))
    (glfw:do-window (:redbits 8
			      :greenbits 8
			      :bluebits 8
			      :alphabits 8
			      :depthbits 16
			      :opengl-version-major 3
			      :opengl-version-minor 1)
	
	((print "init")

	 (setf pipeline (clinch::make-pipeline  
			 :init ((gl:load-identity)
				(gl:clear-color 0.0 1.0 1.0 0.0)
				(gl:clear :color-buffer-bit :depth-buffer-bit)
				(gl:enable :blend :depth-test :line-smooth :point-smooth :polygon-smooth :texture-2d)
				(%gl:blend-func :src-alpha :one-minus-src-alpha))

			 :loop ((gl:clear :color-buffer-bit :depth-buffer-bit)
				(clinch::rotate *node2* rot 0 1 0 t)
				(clinch::render *root*))))

	 (setf *root* (make-instance 'clinch::node))

	 (setf *node1* (make-instance 'clinch::node :parent *root*))
	 (clinch::translate *node1* -1 0 -2 t)
	 
	 (setf *node2* (make-instance 'clinch::node :parent *node1*))


	 (glfw:set-window-size-callback 'window-size-callback)
	 (glfw:set-key-callback  'window-key-press)

	 (cairo:with-png-surface ("test_pattern.png" surf)
	   (let ((bits (cairo:image-surface-get-data surf :pointer-only t))
		 (w (cairo:image-surface-get-width surf))
		 (h (cairo:image-surface-get-height surf)))
	     (setf texture-size (cons w h))

	     (setf texture (make-instance 'clinch:texture :width w :height h :stride 4 :count (* w h) :data bits :qtype :unsigned-char :target :pixel-unpack-buffer))))	 
	 
	 (setf shader (make-instance 'clinch:shader
	 			     :name "Shader01"
	 			     :vertex-shader-text vert-source
	 			     :fragment-shader-text frag-source
				     :uniforms '(("t1" :int))
				     :attributes '(("tc1" :float))))
	 
	 ;; create buffers....
	 (setf vertexes (make-instance 'clinch:buffer 
				       :Stride 3
				       :data '(-0.5 -0.5 0.0
					       0.5 -0.5 0.0
					       -0.5  0.5 0.0
					       0.5  0.5 0.0)))
	 (setf indexes  (make-instance 'clinch:buffer :qtype :unsigned-int
				       :target :element-array-buffer
				       :Stride 1
				       :data '(0 1 2 2 1 3)))
	 (setf tex-coord (make-instance 'clinch:buffer
					:stride 2
					:data '(0.0 1.0
						1.0 1.0
						0.0 0.0
						1.0 0.0)))

	 ;; Create entity (the actual thing that gets drawn on screen)
	 (setf box (make-instance 'clinch:entity
				  :parent *node2*
				  :indexes indexes
				  :shader shader
				  :values `((:attribute "tc1" ,tex-coord)
					    (:attribute "t1" ,texture)
					    (:vertices ,vertexes))))
	 (clinch::run-init pipeline)
	 )
      (clinch::run-loop pipeline (clinch::width viewport) (clinch::height viewport))
      )
    ;; End Program
        
    (print "closed")))


