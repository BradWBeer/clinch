(ql:quickload :lispbuilder-sdl)
(ql:quickload :clinch)

(defun init ()
  )

(defun clean-up ()
  )


(defun main-loop ()
  )

(defun window-size-callback (width height)
  (declare (ignore width height)))

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


