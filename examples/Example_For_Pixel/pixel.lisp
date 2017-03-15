(ql:quickload :clinch)
(ql:quickload :clinch-cairo)
(ql:quickload :clinch-pango) 
(ql:quickload :clinch-freeimage)

(use-package :clinch)

(defparameter *weasle* nil)
(defparameter *weasle-node* nil)

;; Next runs one time before the next on-idle.
(clinch:defevent clinch:*next* ()

  ;; Enable a few opengl features. 
  ;; (gl:enable :blend :depth-test :line-smooth :point-smooth :texture-2d :cull-face)

  ;; ;; Set the blending mode. 
  ;; (%gl:blend-func :src-alpha :one-minus-src-alpha)
  ;; (gl:polygon-mode :front-and-back :fill)

  (gl:clear-color .5 .5 .5 0)

  (setf *weasle-node* (make-instance 'node))
  (setf *weasle*
	(make-quad-for-image "/home/warweasle/work/lisp/weasle.png"
			     :parent *weasle-node*))

  (add-child *root* *weasle-node*))


;; render

(clinch:defevent clinch:*on-idle* ()

  (gl:clear :color-buffer-bit :depth-buffer-bit)
  ;; (test-step (nth 0 *animations*) *ticks*)
  ;; (gl:clear :color-buffer-bit :depth-buffer-bit)
  ;; ;;(clinch:render entity :projection *projection*)
  ;; (clinch:render *node* :projection *projection*)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (clinch:render *root* :projection *ortho-projection*))

;; on window resize
(clinch:defevent clinch:*on-window-resized* (win width height ts)
  ;;(format t "Resized: ~A ~A~%" width height)
  
  ;; (setf *projection* (clinch::make-perspective-transform (clinch:degrees->radians 45)
  ;; 							 (/ width height) .1 1000))
  )


;; zoom in and out
(clinch:defevent clinch:*on-mouse-wheel-move* (win mouse x y ts)
  (format t "win=~A mouse=~A x=~A y=~A ts=~A~%" win mouse x y ts)
  
  (!S *root* (+ 1 (/ y 10)) (+ 1 (/ y 10)) 1))



(init :init-controllers nil)
