(ql:quickload :lispbuilder-sdl)
(ql:quickload :clinch)

(defvar viewport)
(defvar projection-matrix)

(defvar triangle)
(defvar triangle-point-buffer)
(defvar triangle-indices-buffer)

(defun init ()
  (setf viewport (make-instance 'clinch:viewport))
  

  (setf triangle-point-buffer 
	(make-instance 'clinch:buffer 
		       :Stride 3
		       :data '(   0.0  100.0 -1.0
			       -100.0 -100.0 -1.0
			       100.0 -100.0 -1.0)))

  (setf triangle-indices-buffer 
	(make-instance 'clinch:buffer :qtype :unsigned-int
		       :target :element-array-buffer
		       :Stride 1
		       :data '(0 1 2)))

  (setf triangle 
	(make-instance 'clinch:entity
		       :indexes triangle-indices-buffer 
		       :vertices triangle-point-buffer)))

  
(defun main-loop ()
  (gl:clear-color 0.0 0.0 1.0 0.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (clinch:render triangle)
  )

  

(defun clean-up ()
  (clinch:unload triangle-point-buffer)
  (clinch:unload triangle-indices-buffer)
  )

(defun window-size-callback (width height)

  (clinch::quick-set viewport 0 0 width height)
  (clinch::render viewport)
  
  (setf projection-matrix (clinch:make-orthogonal-transform width height .25 100))
  (print projection-matrix)

  (gl:matrix-mode :projection)
  (gl:load-matrix projection-matrix)
  (gl:matrix-mode :modelview)
  (gl:load-identity))


(defun start ()
  (sdl:with-init ()
    (sdl:window 400 300
		:flags sdl-cffi::sdl-opengl
		:resizable t
		:double-buffer t
		:title-caption "Tutorial 1"
		:icon-caption "Tutorial 1")
    (init)
    (window-size-callback 400 300)
    (setf (sdl:frame-rate) 60)
    (sdl:with-events ()
      (:quit-event () t)
      (:VIDEO-RESIZE-EVENT (:W W :H H) 
			   (window-size-callback w h))
      (:idle ()         
	     (main-loop)
	     (sdl:update-display)))
    (clean-up)))


