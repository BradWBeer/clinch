(ql:quickload :alexandria)
(ql:quickload :clode)
(ql:quickload :sdl2)
(ql:quickload :clinch)
(ql:quickload :clinch-pango)
(ql:quickload :clinch-slime)

(defvar ray)
(defvar phy-plane)
(defvar win-entity)
(defvar win-node)

(defvar viewport)
(defvar projection-matrix)
(defvar ortho-matrix)
(defvar overlay1)
(defvar sphere)
(defvar box)
(defvar cylinder)
(defvar root-node)
(defvar overlay-01)
(defvar player-node)
(defvar plane)
(defvar shader)
(defvar tex-shader) 
(defvar move-direction)
(defvar cursor-enabled) 
(defvar mouse-forward)
(defvar mouse-strafe)
(defvar wall)
(defvar tex-overlay)
(defvar player-object)
(defvar overlay-text "")

;; Draw text and crosshairs on the overlay.
(defun draw-overlay (texture text)
  (clinch::with-context-for-texture  (texture :width-var w :height-var h)
    
    ;; Draw the crosshairs
    (clinch:clear-cairo-context 0 0 0 0)
    (cairo:set-line-width 10)
    (cairo:set-line-cap :round)
    (cairo:set-source-rgba 0 0 1 .2)

    (let* ((cw (/ w 2))
	   (ch (/ h 2)))

      (cairo:move-to (- cw 50) (- ch 50))
      (cairo:line-to (+ cw 50) (+ ch 50))
      (cairo:move-to (+ cw 50) (- ch 50))
      (cairo:line-to (- cw 50) (+ ch 50))
      (cairo:stroke))

    ;; Draw the text
    (cairo:move-to 0 0)
    (unless (string-equal overlay-text "")
      (clinch:print-text `("span" (("font_desc" "Century Schoolbook L Roman bold 100"))
				  ,text)
			 :width w))))


;; ambientLight   The lowest amount of light to use. An RGB value.
(defvar ambientLight)
(setf ambientLight '(.3 .3 .3))

;; lightIntensity The maximum power of the light.    An RGB value.
(defvar lightIntensity)
(setf lightIntensity '(.8 .8 .8))

;; lightDirection The direction of the light source. An XYZ normal value.
(setf lightDirection 
      (coerce  (sb-cga:normalize (clinch:make-vector 1 5 3))
	       'list))

;; Find the working directory so we can load assets.
(defvar *working-dir*
  (concatenate 'string 
	       (directory-namestring
		(asdf:system-relative-pathname :clinch "clinch.asd"))
	       "examples/fps01/"))

;; Evals a file and returns the result.
(defun eval-from-file (file)
  (eval
   (read-from-string
    (alexandria:read-file-into-string (concatenate 'string *working-dir* file)))))

;; Allows SDL2 to access slime/swank.
(defmacro with-main (&body body)
  "Enables REPL access via UPDATE-SWANK in the main loop using SDL2. Wrap this around
the sdl2:with-init code."
  ;;TODO: understand this. Without this wrapping the sdl:with-init the sdl thread
  ;; is an "Anonymous thread" (tested using sb-thread:*current-thread*), while applying
  ;; this makes *current-thread* the same as the one one when queried directly from the
  ;; REPL thread: #<SB-THREAD:THREAD "repl-thread" RUNNING {adress...}>
  `(sdl2:make-this-thread-main
    (lambda ()
      ;; does work on linux+sbcl without the following line:
      #+sbcl (sb-int:with-float-traps-masked (:invalid) ,@body)
      #-sbcl ,@body)))

;; Adds a wall, given the size, position and rotation.
(defun add-wall (size position rotation)
  (let ((cn (make-instance 'clode:clode-node
			   :geometry (make-instance 'ode:physics-box :x (first size) :y (second size) :z (third size) :body nil
						    :mode '(:bounce :soft-CFM) :bounce .5 :bounce-vel .01)
			   :children (list (let ((n (make-instance 'clinch:node :children 
								   (list wall))))
					     (clinch:scale n (first size) (second size) (third size))
					     n)))))
    (when rotation
      (clinch:rotate cn (clinch:degrees->radians (first rotation)) (second rotation) (third rotation) (fourth rotation)))
    (when position (clinch:translate cn (first position) (second position) (third position)))
    (clinch:add-child root-node cn)
    cn))

;; Initialize the scene
(defun init ()

  ;; disable the cursor and allow player movement.
  (setf cursor-enabled nil)

  ;; set up the player 
  (setf mouselook (clinch:make-vector 0 0 0))
  (setf mouse-strafe 0)
  (setf mouse-forward 0)
  (setf move-direction (clinch:make-vector 0.0 0.0 0.0))

  (format t "Initializing physics...~%")
  (ode:physics-init)
  (format t "Done initing physics!~%");

  ;; set up global opengl settings
  (gl:enable :blend :depth-test :line-smooth :point-smooth :texture-2d :cull-face)
  (%gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:polygon-mode :front-and-back :fill)
  
  ;; load the simple color shader.
  (setf shader (eval-from-file "simple-shader.lisp"))

  ;; load the texture shader.
  (setf tex-shader (eval-from-file "simple-texture.lisp"))

  ;; Create the window's viewport
  (setf viewport (make-instance 'clinch:viewport))

  ;; load the wall, sphere, box, plane and cylinder meshes
  (setf wall (eval-from-file "wall.lisp"))
  (let ((sphere (eval-from-file "sphere.lisp"))
	(box (eval-from-file "box.lisp"))
	(plane (eval-from-file "plane.lisp"))
	(cylinder (eval-from-file "cylinder.lisp")))
	;(simple-rect (eval-from-file "simple-rect.lisp")))

    ;; create the overlay texture.
    (setf tex-overlay (make-instance 'clinch:texture
				     :width 800
				     :height 800
				     :stride 4
				     :qtype :unsigned-char
				     :target :pixel-unpack-buffer))

    ;; draw the overlay for the first time.
    (draw-overlay tex-overlay overlay-text)   
    
    ;; create the overlay node and mesh
    (setf overlay-01
	  (make-instance 'clinch:node
			 :enabled nil
			 :children (list
				    (setf overlay1
					  (make-instance
					   'clinch:entity
					   :shader tex-shader
					   :indexes (make-instance 'clinch:buffer :qtype :unsigned-int
								   :target :element-array-buffer
								   :Stride 1
								   :data '(0 1 2 0 2 3))
					   :vertices (make-instance 'clinch:buffer 
								    :Stride 3
								    :data (map 'list (lambda (x)
										       (coerce x 'single-float)) '( -1   1 0
														   -1  -1 0
														   1  -1 0
														   1   1 0)))

					   :values   `((:attribute "tc1" ,(make-instance 'clinch:buffer
											 :stride 2
											 :data '(0.0 0.0
												 0.0 1.0
												 1.0 1.0
												 1.0 0.0)))
						       (:uniform "t1" ,(lambda (&rest args) tex-overlay))))))))

    ;; create the "scene tree" of sorts
    (setf root-node
	  (make-instance 'clinch:node
			 :children
			 (list
			  (make-instance 'clinch:node :children (list plane))							 
			  (make-instance 'clode:clode-node
					 :geometry (setf phy-object1
							 (make-instance 'ode:physics-sphere
									:radius 1
									:density 1 :position '(3 2 5) :mode '(:bounce :soft-CFM) :bounce .5 :bounce-vel .01))
					 :children (list sphere))

			  (make-instance 'clode:clode-node
					 :geometry (setf phy-object2
							 (make-instance 'ode:physics-box :x 1 :y 1 :z 1 :density 1
									:position '(.1 4 -5) :mode '(:bounce :soft-CFM) :bounce .5 :bounce-vel .01))
					 :children (list box))
			  
			  (make-instance 'clode:clode-node
					 :geometry (setf phy-object3
							 (make-instance 'ode:physics-cylinder :radius 1 :length 1
									:density 1 :position '(3 8 -2) :mode '(:bounce :soft-CFM)
									:bounce .5 :bounce-vel .01))
					 :children (list cylinder))
			  
			  (make-instance 'clode:clode-node
					 :geometry (setf phy-object4
							 (make-instance 'ode:physics-cylinder :radius 1 :length 1
									:density 1 :position '(3 4 -2) :mode '(:bounce :soft-CFM)
									:bounce .5 :bounce-vel .01))
					 :children (list cylinder))
			  
			  (setf player-node (make-instance 'clode:clode-node
							   :geometry (setf phy-object5
									   (make-instance 'ode:physics-capsule :radius 1 :length 1
											  :position '(0 0 0)
											  :density 1 :mode '(:bounce :soft-CFM)
											  :bounce .5 :bounce-vel .01))
							   :children (list (make-instance 'clinch:node
											  :enabled t
											  :children (list
												     cylinder
												     (let ((node (make-instance 'clinch:node :children (list sphere))))
												       (clinch:translate node 0 0 1/2)
												       node)
												     (let ((node (make-instance 'clinch:node :children (list sphere))))
												       (clinch:translate node 0 0 -1/2)
												       node))))))
			  (setf win-node
				(make-instance 'clinch:node :children (let ((e (eval-from-file "box.lisp")))
									(setf win-entity e)
									(setf (clinch:render-value e "color") '(1.0 0.0 0.0 .5))
									(list e))))))))

  (clinch:scale  win-node  5 10 5)
  (clinch:translate  win-node  7.5 5.0 7.5)


  (add-wall '(1 20 20) '(10 10 0) nil)
  (add-wall '(1 20 20) '(-10 10 0) nil)
  (add-wall '(20 20 1) '(0 10 -10) nil)	
  (add-wall '(20 20 1) '(0 10 10) nil)	
  (add-wall '(10 5 2)  '(0 2.5 0) nil)


  (setf ray (make-instance 'clode::physics-spring :length 3 :springiness 1/2 :damping .1
			   :body (clode::body phy-object5)))

  (clinch:rotate player-node (clinch:degrees->radians -90) 1.0 0.0 0.0)
  (clode:body-set-position (clode::pointer (clode::body phy-object5)) -8 3 -8)

  (setf player-object (make-instance 'clode:physics-player :body (clode:body phy-object5)))
  (setf (clode:horizontal player-object) (- 180 45)))

(defun main-loop ()

  (gl:clear-color 0.0 0.0 1.0 0.0)
  
  
  (let* ((b (clode:pointer (clode:body player-object)))
	 (v (clode:body-get-linear-vel b)))
    
    (if (or (not (zerop mouse-forward))
	    (not (zerop mouse-strafe)))
	
	(let* ((m (sb-cga:rotate-around (clinch:make-vector 0 -1 0)
					(clinch:degrees->radians (clode:horizontal player-object))))
	       (p (clinch:transform-point (sb-cga:normalize (clinch:make-vector mouse-strafe 0 mouse-forward)) m)))
	  
	  (clode:body-set-linear-vel b (* 5 (aref p 0)) (aref v 1) (* 5 (aref p 2))))
	
	(clode:body-set-linear-vel b 0 (aref v 1) 0)))
  
  
  (let ((v1 (clode:body-get-linear-vel (clode::pointer (clode::body phy-object5)))))
    
    (clode:body-set-linear-vel (clode::pointer (clode::body phy-object5)) 
			       (aref v1 0)
			       (+ (aref v1 1)
				  -6/60)
			       (aref v1 2)))
  
  (ode:physics-step 1/60 (cffi:callback physics-near-callback))

  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (clinch:update player-node)
  
  (clode:load-player-camera player-object projection-matrix)

  
  (clinch:update root-node)
  
  (clinch:render root-node)

  (gl:matrix-mode :projection)
  (gl:load-matrix ortho-matrix)  
  (gl:matrix-mode :modelview)
  
  (clinch:update overlay-01)
  
  (clinch:render overlay-01))



(defun clean-up ()
  
  )

(defun window-size-callback (width height)
  (clinch::quick-set viewport 0 0 width height)
  (clinch::render viewport)
  (setf projection-matrix (clinch::make-perspective-transform (clinch:degrees->radians 45) (/ width height) .1 100))
  
  (gl:matrix-mode :projection)
  (gl:load-matrix projection-matrix)
  (gl:matrix-mode :modelview)
  (gl:load-identity)

  (when tex-overlay
    (clinch:unload tex-overlay)
    (setf tex-overlay (make-instance 'clinch:texture
				     :width width
				     :height height
				     :stride 4
				     :qtype :unsigned-char
				     :target :pixel-unpack-buffer))
    
    (draw-overlay tex-overlay overlay-text))

  (setf ortho-matrix (clinch:make-orthogonal-transform width height .25 100))
  (clinch:set-identity-transform overlay-01)
  (clinch:scale overlay-01 (/ width 2) (/ height 2) 0)
  (clinch:translate  overlay-01  0 0 5)
  )


(cffi:defcallback physics-near-callback :void ((data :pointer)
					       (o1 :pointer)
					       (o2 :pointer))
  (ode::physics-near-handler data o1 o2))



(defun basic-test ()
  "The kitchen sink."
  
  (let ((*local-stdout* *standard-output*)
	(*local-input* *standard-input*))

    (with-main
      (sdl2:with-init (:everything)
	(let ((*standard-output* *local-stdout*)
	      (*standard-input* *local-input*))	    
	  
	  (format t "Using SDL Library Version: ~D.~D.~D~%"
		  sdl2-ffi:+sdl-major-version+
		  sdl2-ffi:+sdl-minor-version+
		  sdl2-ffi:+sdl-patchlevel+)
	  (finish-output)

	  (sdl2:with-window (win :flags '(:shown :opengl :resizable))
	    (sdl2:with-gl-context (gl-context win)

	      ;; basic window/gl setup
	      (format t "Setting up window/gl.~%")
	      (finish-output)
	      (sdl2:gl-make-current win gl-context)
	      (init)

	      ;; (sdl2:hide-cursor)
	      (sdl2:set-relative-mouse-mode 1)
	      
	      (window-size-callback 800 600)
	      
	      (setf phy-plane (make-instance 'ode:physics-plane :normal '(0 1 0) :position 0 :bounce 1 :bounce-vel .01))

	      (finish-output)
	      
	      (sdl2:with-event-loop (:method :poll)

		(:mousemotion
		 (:xrel x :yrel y)

		 (unless cursor-enabled
		   (setf (clode:horizontal player-object) 
			 (mod (+ (clode:horizontal player-object) (/ x 10)) 360))
		 
		   (setf (clode:vertical player-object)
			 (min
			  (max (+ (clode:vertical player-object) (/ y 10))
			       -89)
			  89))))
		
		(:keydown
		 (:repeat repeat :keysym keysym)
		 (let ((scancode (sdl2:scancode-value keysym))
		       (sym (sdl2:sym-value keysym))
		       (mod-value (sdl2:mod-value keysym)))

		   (when (zerop repeat)
		     (cond
		       ((sdl2:scancode= scancode :scancode-q) (sdl2:set-relative-mouse-mode (if cursor-enabled
												(progn (setf cursor-enabled nil) 1)
												(progn (setf cursor-enabled t) 0))))
		       ((sdl2:scancode= scancode :scancode-w) (incf mouse-forward -1))
		       ((sdl2:scancode= scancode :scancode-s) (incf mouse-forward  1))
		       ((sdl2:scancode= scancode :scancode-d) (incf mouse-strafe  1))
		       ((sdl2:scancode= scancode :scancode-a) (incf mouse-strafe -1))
		       ((sdl2:scancode= scancode :scancode-e)
			(when (setf (clinch:enabled overlay-01)  (not (clinch:enabled overlay-01)))
			  (draw-overlay tex-overlay overlay-text)))
		       ((sdl2:scancode= scancode :scancode-space) (clode::jump player-object))))
		   ))
		;; (format t "EVENT: ~A Key sym: ~a, code: ~a, mod: ~a~%"
		;; 	   repeat
		;; 	   sym
		;; 	   scancode
		;; 	   mod-value)))
		
		
		(:keyup
		 (:keysym keysym)
		 (let ((scancode (sdl2:scancode-value keysym))
		       (sym (sdl2:sym-value keysym))
		       (mod-value (sdl2:mod-value keysym)))
		   (cond
		     ((sdl2:scancode= scancode :scancode-w) (incf mouse-forward  1))
		     ((sdl2:scancode= scancode :scancode-s) (incf mouse-forward -1))
		     ((sdl2:scancode= scancode :scancode-d) (incf mouse-strafe  -1))
		     ((sdl2:scancode= scancode :scancode-a) (incf mouse-strafe   1))
		     ((sdl2:scancode= scancode :scancode-escape) (sdl2:push-event :quit)))
		   ))
		;; (format t "Key sym: ~a, code: ~a, mod: ~a~%"
		;; 	   sym
		;; 	   scancode
		;; 	   mod-value)))
		
		(:windowevent (:event raw-event :data1 d1 :data2 d2)
			      (let ((event (autowrap:enum-key 'sdl2-ffi:sdl-window-event-id raw-event)))
				
				;;(format t "We had a window event: ~S (~S, ~S)~%" event d1 d2)
				(when (eql event :RESIZED)
				  (window-size-callback d1 d2))))
		
		
		(:idle ()

		       (main-loop)
		       
		       (sdl2:gl-swap-window win)
		       (clinch::update-swank))
		
		(:quit () t))

	      (clinch:unref root-node)

	      ;; the plane isn't part of the node system...needs to be removed manually.
	      (ode:unload phy-plane)
	      (ode:physics-uninit)

	      )))))))

