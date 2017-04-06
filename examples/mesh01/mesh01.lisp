;; This is working file as I test features...please don't use. Use tutorial05 instead. 

(ql:quickload :clinch)
(ql:quickload :clinch-freeimage)
(ql:quickload :clinch-classimp)
(use-package :clinch)

(defparameter scene-path
  (asdf:system-relative-pathname 'clinch 
				 "examples/assets/3d/chicken/chickenV3.dae"))

(defparameter *node* nil)
(defparameter *projection* nil)


(defun init-test ()
  (setf *node* (make-instance 'clinch:node :parent nil))
  (clinch:translate *node* (clinch:v! 0 0 -2))
  (clinch:add-child *node* (clinch::import-scene scene-path)))
		    

;; Next runs one time before the next on-idle.
(clinch:defevent clinch:*next* ()

  ;; Enable a few opengl features. 
  (gl:enable :blend :depth-test :line-smooth :point-smooth :texture-2d :cull-face)

  ;; Set the blending mode. 
  (%gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:polygon-mode :front-and-back :fill)

  (gl:clear-color 0 0 1 0)

  ;; run init once
  (init-test))


;; render
(clinch:defevent clinch:*on-idle* ()

  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (clinch:render *node* :projection *projection*))

;; Rotate and translate with mouse
(clinch:defevent clinch:*on-mouse-move* (win mouse state x y xrel yrel ts)
  ;;(format t "x:~A y:~A mouse:~A state:~A~%" x y mouse state)
  (case state
    (1 (clinch:rotate *node*
		      (q:from-fixed-angles (clinch:degrees->radians yrel) (clinch:degrees->radians xrel) 0.0)))
    
    (2 (clinch:translate *node* (clinch:v! (/ xrel 16) (/ yrel -16) 0)))))

;; on window resize
(clinch:defevent clinch:*on-window-resized* (win width height ts)
  (format t "Resized: ~A ~A~%" width height)
  
  (setf *projection* (clinch::make-perspective-transform (clinch:degrees->radians 45)
							 (/ width height) .1 1000)))
;; zoom in and out
(clinch:defevent clinch:*on-mouse-wheel-move* (win mouse x y ts)
  ;;(format t "win=~A mouse=~A x=~A y=~A ts=~A~%" win mouse x y ts)
  (clinch:translate *node* (clinch:v! 0 0 (/ y 1))))

(clinch:init :asynchronous t :init-controllers nil)

