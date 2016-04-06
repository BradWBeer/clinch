;; We are using clinch and the clinch-cairo plugin
(ql:quickload :clinch)
(ql:quickload :clinch-cairo)

;; Makes life easier
(use-package :clinch)

;; Width and height of board
(defparameter *width* 20)
(defparameter *height* 20)

;; Two arrays for updating the life state. 
;; One keeps the results and then they are swapped.
(defparameter *front-array* nil)
(defparameter *back-array*  nil)

;; The projection to make the smallest dimension 1 one unit.
(defparameter *ortho-projection* nil)

;; The "quad" or the two triangles to draw the texture on.
(defparameter *quad* nil)

;; The texture to draw on.
(defparameter *texture* nil)

;; Return true or false.
(defun flip-coin ()
  (zerop (random 2)))

(defun reset-board (width height)
  (setf *front-array* (make-array (list width height) :element-type 'boolean)
	*back-array*  (make-array (list width height) :element-type 'boolean))

  (loop for i from 0 below width
     do (loop for j from 0 below height
	   do (setf (aref *front-array* i j) (flip-coin)))))

;; Get the value at position x y  
(defun get-value (arr x y)
  (when (and (>= x 0) 
	     (< x (array-dimension arr 0)))
    (when (and (>= y 0) 
	       (< y (array-dimension arr 1)))
      (aref arr x y))))

;; Get a list of the critter's neighbors.
(defun get-neighbors (arr x y)
  (list 
   (get-value arr (1- x) (1- y))
   (get-value arr (1- x) y)
   (get-value arr (1- x) (1+ y))

   (get-value arr x (1- y))
   (get-value arr x y)
   (get-value arr x (1+ y))
   
   (get-value arr (1+ x) (1- y))
   (get-value arr (1+ x) y)
   (get-value arr (1+ x) (1+ y))))

;; Update the second array from the first
(defun take-turn (src dest)
  (loop for i from 0 below (array-dimension src 0)
     do (loop for j from 0 below (array-dimension src 1)
	   do (setf (aref dest i j) 
		    (let ((n (count t (get-neighbors src i j))))
		      (cond 
			((< n 2) nil)
			((= n 2) (aref src i j))
			((= n 3) t)
			((> n 3) nil))))))
  dest)

;; Swap the two arrays.
(defun flip ()
  (let ((tmp *front-array*))
    (setf *back-array* tmp
	  *front-array* *back-array*)))

;; Draws the critters on the texture using cl-cairo2
(defun draw-texture (texture arr)

  ;; cairo requires a surface and a context to draw something. 
  ;; This macro creates both
  (with-context-for-texture (texture)
    (clear-cairo-context .95 .95 .95)

    ;; Set the width of the "pen"
    (cairo:set-line-width 2)

    ;; place each critter
    (let ((x-len (/ (width texture) (array-dimension arr 0)))
	  (y-len (/ (height texture) (array-dimension arr 1)))
	  (x-len/2 (/ (width texture) (array-dimension arr 0) 4))
	  (y-len/2 (/ (height texture) (array-dimension arr 1) 4)))

      (loop 
	 for i from 0 below (array-dimension arr 0)
	 do (loop for j from 0 below (array-dimension arr 1)
	       ;; Draw critter at x, y
	       do (let ((x (+ (* x-len i) (* x-len/2 2)))
			(y (+ (* y-len j) (* y-len/2 2))))
		    ;; When the array entry is true draw a filled circle.
		    (when (aref arr i j)
		      (cairo:set-source-rgb .1 .5 .1)
		      (cairo:arc x y (min x-len/2 y-len/2) 0 (* pi 2))
		      (cairo:fill-path))

		    ;; Always draw the edge.
		    (cairo:set-source-rgb 0 0 0)
		    (cairo:arc x y (min x-len/2 y-len/2) 0 (* pi 2))
		    (cairo:stroke))))))
  texture)

;; Next runs one time before the next on-idle.
(defevent *next* ()

  ;; Enable a few opengl features. 
  (gl:enable :blend :depth-test :line-smooth :point-smooth :texture-2d :cull-face)

  ;; Set the blending mode. 
  (%gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:polygon-mode :front-and-back :fill)

  ;; set the clear color for the background.
  (gl:clear-color 0.1 0.1 0.1 0)

  ;; make a 1x1 quad to display the texture.
  (setf *quad* (make-quad 1 1))
  
  ;; Initialize the board
  (reset-board *width* *height*))

;; Run when window is resized.
(defevent *on-window-resized* (win width height ts)
  (format t "Resized: ~A ~A~%" width height)

  ;; Delete the old texture
  (when *texture*
    (unload *texture*))

  ;; find the shortest side. Use that as unity to fit the square quad inside 
  ;; the window.
  (let ((min (min width height)))  

    ;; create a texture which matches the pixels on the screen.
    (setf *texture* (make-instance 'texture :width min :height min))

    ;; The shader needs to know about the new texture 
    ;; This call sets the "t1" uniform variable to the new texture.
    (setf (uniform *quad* "t1") *texture*)
    
    ;; Update the projection matrix.
    ;; This makes one dimension = 1 and the other >= 1
    (setf *ortho-projection*
	  (make-orthogonal-transform (/ width min)
				     (/ height min) 0 1000)))
  ;; Update the texture immediately 
  (draw-texture *texture* *front-array*))

;; Keep a timer
(let ((time 0))

  ;; Runs every v-synch by default
  (defevent *on-idle* ()
    
    ;; get the time min milliSeconds
    (let ((now (sdl2:get-ticks)))

      ;; Update 4 times a second    
      (when (>= (- now time) 1/2)

	;; Set up for next update
	(setf time now)

	;; Update the arrays...
	(take-turn *front-array* *back-array*)
	;; swap out the front and back arrays...
	(flip)
	;; Draw the texture...
	(draw-texture *texture* *front-array*)))

    ;; Clear the screen 
    (gl:clear :color-buffer-bit :depth-buffer-bit)
    
    ;; render the quad. 
    (render *quad* :projection *ortho-projection*)))

;; On keyDown reset the board.
(defevent *on-key-down* (win keysym state ts)
  (reset-board *width* *height*)
  (draw-texture *texture* *front-array*))

;; Start the applications
(init :asynchronous t :init-controllers nil :title "Conway's Game of Life")

