(ql:quickload :cl-glfw)
(ql:quickload :clinch)
(ql:quickload :clinch-cairo)
(ql:quickload :clinch-pango)
(ql:quickload :bordeaux-threads)

(defvar vert-source
  "
#version 120

uniform sampler2D t1;
attribute vec2 tc1;
varying vec2 v_tc1;
varying vec3   normal;
        void main() {
            gl_Position = ftransform(); //gl_ModelViewProjectionMatrix * gl_Vertex;
            normal = gl_NormalMatrix * gl_Normal;
            v_tc1 = tc1;
        }")

;; String for the Fragment Shader
;;   t1    is the texture sampler
;;   v_tc1 is the texture coordinates from the fragment shader
(defvar frag-source
  "
#version 120
uniform sampler2D t1;
varying vec2 v_tc1;
varying vec3  normal;
        void main() {
            gl_FragColor = texture2D(t1, v_tc1) * max(.4, abs(dot(normal, vec3(0, 0, 1))));
        }")




(defvar viewport)
(defvar lens)
(defvar main-shader)
(defvar hub)
(defvar big-square)
(defvar big-square-texture)
(defvar big-square-node)
(defvar *todo*)
(defvar *always-do*)

(defvar mouse-state)
(defvar last-mouse-down)
(defvar last-mouse-over)

(defvar mouse-click-targets)
(defvar mouse-move-targets)
(defvar mouse-drag-values)
(defvar mouse-drag-nodes-start-pos)
(defvar a)
(defvar a-pos)


(defmethod add-mouse-handler ((element integer) (node clinch:node) (entity clinch:entity) button handler)
  (unless (gethash button mouse-click-targets)
    (setf (gethash button mouse-click-targets) (make-hash-table :test 'equal)))

  (let* ((key (list node entity))
	 (handlers (or (gethash key (gethash button mouse-click-targets))
		       (make-array 6 :initial-element nil))))
    (setf (elt handlers element) handler)
    (if (or (elt handlers 0)
	    (elt handlers 1)
	    (elt handlers 2)
	    (elt handlers 3)
	    (elt handlers 4)
	    (elt handlers 5))
	(setf (gethash key (gethash button mouse-click-targets)) handlers)
	(remhash key (gethash button mouse-click-targets))))
  handler)

(defmethod add-mouse-down-handler ((node clinch:node) (entity clinch:entity) button (handler function))
  (add-mouse-handler 0 node entity button handler))

(defmethod add-mouse-up-handler ((node clinch:node) (entity clinch:entity) button (handler function))
  (add-mouse-handler 2 node entity button handler))

(defmethod add-mouse-click-handler ((node clinch:node) (entity clinch:entity) button (handler function))
  (add-mouse-handler 1 node entity button handler))

(defmethod add-drag-handler ((node clinch:node) (entity clinch:entity) handler)
  (add-mouse-handler 3 node entity :left handler))

(defmethod add-drop-handler ((node clinch:node) (entity clinch:entity) handler)
  (add-mouse-handler 4 node entity :left handler))

(defmethod set-draggable ((node clinch:node) (entity clinch:entity) nodes)
  (add-mouse-handler 5 node entity :left nodes)
  (print mouse-click-targets))


(defmethod add-mouse-movement-handler ((element integer) (node clinch:node) (entity clinch:entity) (handler function))
  (let* ((key (list node entity))
	 (handlers (or (gethash key mouse-move-targets)
		       (make-array 3 :initial-element nil))))
    (setf (elt handlers element) handler)
    (if (or (elt handlers 0)
	    (elt handlers 1)
	    (elt handlers 2))
	(setf (gethash key mouse-move-targets) handlers)
	(remhash key mouse-move-targets)))
  handler)

(defmethod add-mouse-over-handler ((node clinch:node) (entity clinch:entity) (handler function))
  (add-mouse-movement-handler 0 node entity handler))


(defmethod add-mouse-still-over-handler ((node clinch:node) (entity clinch:entity) (handler function))
  (add-mouse-movement-handler 1 node entity handler))

(defmethod add-mouse-exit-handler ((node clinch:node) (entity clinch:entity) (handler function))
  (add-mouse-movement-handler 2 node entity handler))


(defvar unit-square-vertexes)
(defvar unit-square-indexes)
(defvar unit-square-tex-coords)
(defvar unit-square-normals)

(defvar halo)
(defvar data)

(defun write-to-texture (text texture)
  (setf data
	(format nil "~A~%~A" text data))

  (clinch:with-context-for-texture (texture)
    (clinch:clear-cairo-context 0 0 0 1)
    (cairo:move-to 40 40)
    (cairo:set-source-rgb 1 1 1)
    (clinch:print-text data :width 320 :alignment :pango_align_left)))


(defun create-rectangle (texture node &key (width 1) (height 1))
  (make-instance 'clinch:entity
		 :parent node
		 :indexes unit-square-indexes
		 :shader main-shader
		 :values `((:attribute "tc1" ,unit-square-tex-coords)
			   (:uniform "t1" ,texture)
			   (:vertices ,(make-instance 'clinch:buffer 
						      :Stride 3
						      :data (let* ((w/2 (float (/ width 2)))
								   (h/2 (float (/ height 2)))
								   (-w/2 (- w/2))
								   (-h/2 (- h/2)))
							      (list -w/2 -h/2 0.0
								    w/2 -h/2 0.0
								    -w/2  h/2 0.0
								    w/2  h/2 0.0))))
			   (:normals ,unit-square-normals))))



(defun window-mouse-button-press (button action)
  (destructuring-bind (x y) (glfw:get-mouse-pos)
    (let  ((lens-1 (sb-cga:inverse-matrix lens)))
      
      (multiple-value-bind  (button-start-position button-end-position)
	  (clinch:unproject x y (clinch:width viewport)
			    (clinch:height viewport)
			    lens-1)

	(if (= action 1)
	    
	    (let ((entity-distance)
		  (entity-u)
		  (entity-v)
		  (entity-triangle)
		  (entity-triangle-index)
		  (entity-key)
		  (entity-value))

	      (maphash (lambda (key value)
			 (when (or (elt value 0)
				   (elt value 1))

			   (multiple-value-bind (found-entity-distance
						 found-entity-u
						 found-entity-v
						 found-entity-triangle
						 found-entity-triangle-index)
			       (clinch:ray-entity-intersect? (second key) (clinch:current-transform (first key))
							     button-start-position button-end-position)
			     
			     (when (or (not entity-distance)
				       (and found-entity-distance
					    (> found-entity-distance entity-distance)))
			       
			       (setf entity-distance found-entity-distance
				     entity-u found-entity-u
				     entity-v	found-entity-v
				     entity-triangle found-entity-triangle
				     entity-triangle-index found-entity-triangle-index
				     entity-key key
				     entity-value value)))))
		       
		       (gethash button mouse-click-targets))

	      (when (and entity-distance (elt entity-value 0))
		(funcall (elt entity-value 0) (first entity-key) (second entity-key))
		;; are we starting a drag and drop?

		(when (elt entity-value 5)

		  (setf mouse-state :dragging
			mouse-drag-nodes-start-pos (map 'list #'clinch:transform (elt entity-value 5))
			mouse-drag-values entity-value
			a-pos button-start-position
			a     (sb-cga:vec* button-end-position entity-distance))

		  (when (elt entity-value 3)
		    (funcall (elt entity-value 3) (first entity-key) (second entity-key))))
		
		(setf last-mouse-down entity-key)))

	    ;; mouse up
	    (if (eql mouse-state :dragging)
		(progn 
		  (setf mouse-state nil)
		  (if (elt mouse-drag-values 4)
		      (funcall (elt mouse-drag-values 4) (first last-mouse-down) (second last-mouse-down))))

		
		(let ((entity-distance)
		      (entity-u)
		      (entity-v)
		      (entity-triangle)
		      (entity-triangle-index)
		      (entity-key)
		      (entity-value))
		  
		  (maphash (lambda (key value)
			     (when (or (elt value 1)
				       (elt value 2))
			       
			       (multiple-value-bind (found-entity-distance
						     found-entity-u
						     found-entity-v
						     found-entity-triangle
						     found-entity-triangle-index)
				   (clinch:ray-entity-intersect? (second key) (clinch:current-transform (first key))
								 button-start-position button-end-position)
				 
				 (when (or (not entity-distance)
					   (and found-entity-distance
						(> found-entity-distance entity-distance)))
				   
				   (setf entity-distance found-entity-distance
					 entity-u found-entity-u
					 entity-v found-entity-v
					 entity-triangle found-entity-triangle
					 entity-triangle-index found-entity-triangle-index
					 entity-key key
					 entity-value value)))))
			   
			   (gethash button mouse-click-targets))
		  
		  (when entity-distance
		    (when (elt entity-value 2)
		      (funcall (elt entity-value 2) (first entity-key) (second entity-key)))
		    (when (and (elt entity-value 1) (equal entity-key last-mouse-down))
		      (funcall (elt entity-value 1) (first entity-key) (second entity-key))))
		  
		  
		  (setf last-mouse-down nil
			mouse-state nil
			mouse-drag-nodes-start-pos nil
			mouse-drag-values nil))))))))


(defun calculate-movement (d-pos d-hat a-pos a b-pos b)
  (let ((a-rel (sb-cga:vec- a-pos d-pos))
	(b-rel (sb-cga:vec- b-pos d-pos)))
    (sb-cga:vec-
     (sb-cga:vec+ b-rel
		  (sb-cga:vec* b
			       ;; find 'x'
			       (/ (sb-cga:dot-product (sb-cga:vec- (sb-cga:vec+ a-rel a)
								   b-rel)
						      d-hat)
				  (sb-cga:dot-product b d-hat))))
     (sb-cga:vec+ a-rel a))))



;;
(defun window-mouse-move (x y)
  (destructuring-bind (x y) (glfw:get-mouse-pos)
    (let  ((lens-1 (sb-cga:inverse-matrix lens)))
      
      (multiple-value-bind  (button-start-position button-end-position)
	  (clinch:unproject x y (clinch:width viewport)
			    (clinch:height viewport)
			    lens-1)

	(if (eql mouse-state :dragging)
	    
	    (multiple-value-bind (d-pos d) (clinch:get-screen-direction (sb-cga:inverse-matrix lens))
	      (let ((trans (calculate-movement d-pos d a-pos a button-start-position button-end-position)))
		
		(map nil (lambda (node start-pos)
			   (setf (clinch:transform node)
				 (sb-cga:matrix* (sb-cga:translate (clinch:make-vector (elt trans 0) (elt trans 1) (elt trans 2)))
						 start-pos)))
		     (elt mouse-drag-values 5) mouse-drag-nodes-start-pos)))
	    

	    (let ((entity-distance)
		  (entity-u)
		  (entity-v)
		  (entity-triangle)
		  (entity-triangle-index)
		  (entity-key)
		  (entity-value))
	      
	      (maphash (lambda (key value)
			 
			 (when (or (elt value 0)
				   (elt value 1))
			   
			   (multiple-value-bind (found-entity-distance
						 found-entity-u
						 found-entity-v
						 found-entity-triangle
						 found-entity-triangle-index)
			       (clinch:ray-entity-intersect? (second key) (clinch:current-transform (first key))
							     button-start-position button-end-position)
			     
			     (when (or (not entity-distance)
				       (and found-entity-distance
					    (> found-entity-distance entity-distance)))
			       
			       (setf entity-distance found-entity-distance
				     entity-u found-entity-u
				     entity-v	found-entity-v
				     entity-triangle found-entity-triangle
				     entity-triangle-index found-entity-triangle-index
				     entity-key key
				     entity-value value)))))
		       
		       mouse-move-targets)
	      
	      (if last-mouse-over 
		  (if entity-distance 
		      
		      (if (equal entity-key (car last-mouse-over))
			  ;; mouse-still-over
			  (when (elt entity-value 1)
			    (funcall (elt entity-value 1) (first entity-key) (second entity-key)))
			  
			  ;; mouse-exit & mouse-enter
			  (progn (when (elt (cdr last-mouse-over) 2)
				   (funcall (elt (cdr last-mouse-over) 2) (first (car last-mouse-over)) (second (car last-mouse-over))))
				 (when (elt entity-value 0)
				   (funcall (elt entity-value 0) (first entity-key) (second entity-key)))
				 (setf last-mouse-over (cons entity-key entity-value))))
		      
		      ;; only the mouse exit...
		      (progn (when (elt (cdr last-mouse-over) 2)
			       (funcall (elt (cdr last-mouse-over) 2) (first (car last-mouse-over)) (second (car last-mouse-over))))
			     (setf last-mouse-over nil)))
		  
		  ;; just the mouse enter
		  (if entity-distance
		      (progn (when (elt entity-value 0)
			       (funcall (elt entity-value 0) (first entity-key) (second entity-key)))
			     (setf last-mouse-over (cons entity-key entity-value)))
		      
		      ;; or not...
		      (setf last-mouse-over nil)))))))))




(defun init ()
  (print "init")

  (setf *todo* nil)
  (setf *always-do* nil)

  (setf mouse-state nil)
  (setf last-mouse-down nil)
  (setf last-mouse-over nil)
  
  (setf mouse-click-targets (make-hash-table))
  (setf mouse-move-targets (make-hash-table :test 'equal))
  (setf mouse-drag-nodes-start-pos nil)
  (setf mouse-drag-values nil)


  (setf data "This is a test!")

  ;; set the window event handlers

  (setf viewport (make-instance 'clinch:viewport))
  (glfw:set-window-size-callback #'window-size-callback)
  
  ;;(glfw:set-key-callback  'window-key-press)
  (glfw:set-mouse-button-callback  #'window-mouse-button-press)
  (glfw:set-mouse-pos-callback     #'window-mouse-move)

  (gl:enable :blend :depth-test :line-smooth :point-smooth :polygon-smooth :texture-2d)
  (%gl:blend-func :src-alpha :one-minus-src-alpha)

  (setf hub (make-instance 'clinch:node))
  (clinch:translate hub 0 0 -1.2 t )
  (clinch:update hub)

  (setf main-shader (make-instance 'clinch:shader
				   :name "Shader01"
				   :vertex-shader-text vert-source
				   :fragment-shader-text frag-source
				   :uniforms '(("t1" :int))
				   :attributes '(("tc1" :float))))

  ;; create buffers....
  (setf unit-square-vertexes (make-instance 'clinch:buffer 
					    :Stride 3
					    :data '(-0.5 -0.5 0.0
						    0.5 -0.5 0.0
						    -0.5  0.5 0.0
						    0.5  0.5 0.0)))
  (setf unit-square-indexes  (make-instance 'clinch:buffer :qtype :unsigned-int
					    :target :element-array-buffer
					    :Stride 1
					    :data '(0 1 2 2 1 3)))
  (setf unit-square-tex-coords (make-instance 'clinch:buffer
					      :stride 2
					      :data '(0.0 1.0
						      1.0 1.0
						      0.0 0.0
						      1.0 0.0)))
  (setf unit-square-normals (make-instance 'clinch:buffer 
					   :Stride 3
					   :data '(0.0 0.0 1.0
						   0.0 0.0 1.0
						   0.0 0.0 1.0
						   0.0 0.0 1.0)))

  (setf big-square-node (make-instance 'clinch:node))
  (clinch:add-child hub big-square-node)

  
  (setf big-square-texture (make-instance 'clinch:texture :width 400 :height 400))
  (clinch:with-context-for-texture (big-square-texture)
    (clinch:clear-cairo-context 0 0 0 1))    
  
  (setf big-square (create-rectangle big-square-texture
				     big-square-node))
  
  (add-mouse-over-handler big-square-node big-square
			  (lambda (x y) (write-to-texture "Big-Square: mouse-over" big-square-texture)))
  (add-mouse-exit-handler big-square-node big-square
			  (lambda (x y) (write-to-texture "Big-Square: mouse-exit" big-square-texture)))
  

  (setf halo (make-instance 'clinch:node))
  (clinch:translate halo 0 0 0)
  (clinch:add-child hub halo)
  
  (multiple-value-bind (node entity) (add-button halo (concatenate 'string 
								   (directory-namestring
								    (asdf:system-relative-pathname :clinch "clinch.asd"))
								   "examples/halo01/icons/move.png") -0.6 .6)
    
    (set-draggable node entity (list big-square-node halo))
    (add-drag-handler node entity (lambda (x y) (write-to-texture "move: Drag" big-square-texture)))
    (add-drop-handler node entity (lambda (x y) (write-to-texture "move: Drop" big-square-texture)))
    
    (add-mouse-down-handler node entity :left (lambda (x y) (write-to-texture "move: mouse-down" big-square-texture)))
    (add-mouse-up-handler node entity   :left (lambda (x y) (write-to-texture "move: mouse-up" big-square-texture)))
    (add-mouse-click-handler node entity   :left (lambda (x y) (write-to-texture "move: mouse-click" big-square-texture)))

    )
  
  
  (multiple-value-bind (node entity) (add-button halo (concatenate 'string 
								   (directory-namestring
								    (asdf:system-relative-pathname :clinch "clinch.asd"))
								   "examples/halo01/icons/resize-vert.png") 0 .6)
    (add-mouse-down-handler node entity :left (lambda (x y) (write-to-texture "Resize-Vert: mouse-down" big-square-texture)))
    (add-mouse-up-handler node entity   :left (lambda (x y) (write-to-texture "Resize-Vert: mouse-up" big-square-texture)))
    (add-mouse-click-handler node entity   :left (lambda (x y) (write-to-texture "Resize-Vert: mouse-click" big-square-texture))))
  
  
  (multiple-value-bind (node entity) (add-button halo (concatenate 'string 
								   (directory-namestring
								    (asdf:system-relative-pathname :clinch "clinch.asd"))
								   "examples/halo01/icons/close.png") 0.6 .6)
    (add-mouse-down-handler node entity :left (lambda (x y) (write-to-texture "Close: mouse-down" big-square-texture)))
    (add-mouse-up-handler node entity   :left (lambda (x y) (write-to-texture "Close: mouse-up" big-square-texture)))
    (add-mouse-click-handler node entity   :left (lambda (x y) (write-to-texture "Close: mouse-click" big-square-texture))))
  
  (multiple-value-bind (node entity) (add-button halo (concatenate 'string 
								   (directory-namestring
								    (asdf:system-relative-pathname :clinch "clinch.asd"))
								   "examples/halo01/icons/resize-horz.png") .6 0)
    (add-mouse-down-handler node entity :left (lambda (x y) (write-to-texture "Resize-Horz: mouse-down" big-square-texture)))
    (add-mouse-up-handler node entity   :left (lambda (x y) (write-to-texture "Resize-Horz: mouse-up" big-square-texture)))
    (add-mouse-click-handler node entity   :left (lambda (x y) (write-to-texture "Resize-Horz: mouse-click" big-square-texture))))
  
  (multiple-value-bind (node entity) (add-button halo (concatenate 'string 
								   (directory-namestring
								    (asdf:system-relative-pathname :clinch "clinch.asd"))
								   "examples/halo01/icons/bang.png") -.6 -.6)
    (add-mouse-down-handler node entity :left (lambda (x y) (write-to-texture "Bang: mouse-down" big-square-texture)))
    (add-mouse-up-handler node entity   :left (lambda (x y) (write-to-texture "Bang: mouse-up" big-square-texture)))
    (add-mouse-click-handler node entity   :left (lambda (x y) (write-to-texture "Bang: mouse-click" big-square-texture))))
  
  (multiple-value-bind (node entity) (add-button halo (concatenate 'string 
								   (directory-namestring
								    (asdf:system-relative-pathname :clinch "clinch.asd"))
								   "examples/halo01/icons/star.png") 0 -.6)
    (add-mouse-down-handler node entity :left (lambda (x y) (write-to-texture "Star: mouse-down" big-square-texture)))
    (add-mouse-up-handler node entity   :left (lambda (x y) (write-to-texture "Star: mouse-up" big-square-texture)))
    (add-mouse-click-handler node entity   :left (lambda (x y) (write-to-texture "Star: mouse-click" big-square-texture))))

  (multiple-value-bind (node entity) (add-button halo (concatenate 'string 
								   (directory-namestring
								    (asdf:system-relative-pathname :clinch "clinch.asd"))
								   "examples/halo01/icons/question.png") .6 -.6)
    (add-mouse-down-handler node entity :left (lambda (x y) (write-to-texture "Huh: mouse-down" big-square-texture)))
    (add-mouse-up-handler node entity   :left (lambda (x y) (write-to-texture "Huh: mouse-up" big-square-texture)))
    (add-mouse-click-handler node entity   :left (lambda (x y) (write-to-texture "Huh: mouse-click" big-square-texture))))

  (clinch:update hub))


(defun add-button (parent-node path x y)
  (let ((node (make-instance 'clinch:node)))
    (clinch:translate node 0 0 0 t )
    (clinch:add-child parent-node node)
    (clinch:translate node x y 0)
    (clinch:update node)
    (values node (create-rectangle (clinch::create-texture-from-png path) node :width .1 :height .1))))



(defun main-loop ()

  (when *todo*
    (map nil #'funcall *todo*)
    (setf *todo* nil))
  
  (when *always-do*
    (map nil #'funcall *always-do*))
  
  (gl:clear-color 0.0 0.0 1.0 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (clinch:render hub))


(defun clean-up ()
  (print "closed")
  (clinch:unload main-shader))

;; On resize set the camera transform and load it.
(defun window-size-callback (width height)
  (format t "Resize called with: w=~A h=~A~%" width height)
  (clinch::quick-set viewport 0 0 width height)
  (clinch::render viewport)
  
  (setf lens (clinch::make-perspective-transform (/ (* 65 pi) 360) (/ width height) .01 100))
  ;;(setf lens-1 (sb-cga:inverse-matrix lens))
  ;;(sb- lens 0 0 -12 t)
  (gl:matrix-mode :projection)
  (gl:load-matrix lens)
  (gl:matrix-mode :modelview)
  (gl:load-identity))


;; The start point...    
(defun start-single-threaded ()
  (declare (optimize (speed 3)))

  ;;(setf main-square nil)
  (glfw:do-window (:redbits 8
			    :greenbits 8
			    :bluebits 8
			    :alphabits 8
			    :depthbits 16
			    :opengl-version-major 3
			    :opengl-version-minor 1)
      ((init))

    (main-loop))

  ;; End Program
  (clean-up))

(defun start ()
  (bordeaux-threads:make-thread #'start-single-threaded :name "render-thread"))
