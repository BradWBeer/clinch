(ql:quickload :cl-glfw)
(ql:quickload :clinch)

(defun init ()
  )

(defun main-loop ()
  )

(defun clean-up ()
  )

(defun start ()
  (declare (optimize (speed 3)))
  (glfw:do-window (:title "Tutorial 1"
			  :redbits 8
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
