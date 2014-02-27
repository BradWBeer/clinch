(ql:quickload :cl-glfw)
(ql:quickload :clinch)

(defvar viewport)
(defvar projection-matrix)

(defvar triangle)
(defvar triangle-point-buffer)
(defvar triangle-indices-buffer)


(defun init ()
  (setf viewport (make-instance 'clinch:viewport :disables nil :enables nil :blend-func nil))
  (glfw:set-window-size-callback #'window-size-callback)

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
  (clinch:render triangle))

  

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
  (declare (optimize (speed 3)))
  (glfw:do-window (:title "Tutorial 1"
			  :redbits 8
			  :greenbits 8
			  :bluebits 8
			  :alphabits 8
			  :depthbits 16)
      ((init))
    
    (main-loop))
  
  ;; End Program
  (clean-up))
