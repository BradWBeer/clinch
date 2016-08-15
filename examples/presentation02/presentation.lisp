(ql:quickload :clinch)
(ql:quickload :clinch-cairo)
(ql:quickload :clinch-pango)
(ql:quickload :clinch-freeimage)
(ql:quickload :easing)

(use-package :clinch)


;; Load the clinch freeimage plugin to load a file
;; I could have clinch-cairo too, since it has create-texture-from-png,
;; and I'm using a png file.
(ql:quickload :clinch-freeimage)

;; Var to hold the square entityl
(defparameter *entity*  nil)

;; var to hold the texture;
(defparameter *texture* nil)

;; var to hold the projection matrix
(defparameter *projection* nil)

;; Var to enable disable vertical rotation
(defparameter *vertical* nil) 

;; Var to set how fast the mouse moves
(defparameter *mouse-multiplier* 1/8) 

;; Var to set how fast the scroll moves
(defparameter *scroll-multiplier* 1/16) 

;; Where is the presentation?
(defparameter *page* 0)
(defparameter *distance* 0)

;; Where is the Move to presentation?
(defparameter *start-position* nil)
(defparameter *end-position* nil)
(defparameter *start-rotation* nil)
(defparameter *end-rotation* nil)

(defparameter *spacing* .1)
(defparameter *slide-width* 1)
(defparameter *slide-height* 1)

(defparameter *move-to-page* 0)

(defmacro as-slide  ((i w h) &body body)
  `(lambda (,i ,w ,h)
     (declare (ignorable ,i ,h ,w))
     ,@body))

(defmacro pp (text w h)
  (declare (ignorable h))
  `(pango:print-text `("span" (("font_desc" "Century Schoolbook L Roman bold 50"))
		       ,,text)
		     :width ,w
		     :alignment :pango_align_left))

(defmacro nl (w h)
  `(pp " " ,w ,h))


(defparameter *slides* nil)
(load "slides.lisp")

;; Initialize the test. 
(defun init-test ()

  (gl:disable :cull-face))


;; Next runs one time before the next on-idle.
(clinch:defevent clinch:*next* ()

  ;; Enable a few opengl features. 
  (gl:enable :blend :depth-test :line-smooth :point-smooth :texture-2d :cull-face)
  (%gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:polygon-mode :front-and-back :fill)

  (gl:clear-color 0 0 1 0)

  (make-slides 1 1)
  
  ;; Initialize 
  (init-test))


;; Main loop
(clinch:defevent clinch:*on-idle* ()

  (incf *move-to-page* (/ *delta-ticks* 1000))
  (let ((pos (easing:out-back *move-to-page*)))
    (when (< pos 1)
      (when (and *start-rotation*
		 *end-rotation*)
	(setf (rotation *root*) (q:slerp *start-rotation*
					 *end-rotation*
					 pos))
	    ;; (translation *root*) (v:lerp *start-position*
	    ;; 				 *end-position*
	    ;; 				 pos))))
	    )))

  
  ;; clear the screen
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (clinch:render *entity* :projection *ortho-projection*)
  (gl:clear :depth-buffer-bit)
   
  ;; render the root node which renders everything under it.
  (clinch:render *root* :projection *projection*))

(defun start-movement (i)
  (multiple-value-bind (rot pos)
      (get-position-for-slide i (length (loop for i from 0 to 5 collect i)) *slide-width*)
    (setf *start-rotation* (rotation *root*)
	  *end-rotation* (q:from-fixed-angles 0 rot 0)
	  *start-position* (translation *root*)
	  *end-position* (v! 0 0 pos)
	  *move-to-page* 0)
    (print
     (list
      *start-rotation*
      *end-rotation*
      *start-position*
      *end-position* 
      *move-to-page*))))


(defevent *on-key-down* (win keysym state ts)
  (cond ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-space)
	 (setf *vertical* (not *vertical*)))
	((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-right)
	 (print "RIGHT ARROW!")
	 (setf *page* (mod (1+ *page*) (length *slides*)))
	 (start-movement *page*))
	 
	((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-left)
	 (print "LEFT ARROW!")
	 (setf *page* (mod (1- *page*) (length *slides*)))
	 (start-movement *page*))))
	 

;; Handler for when the mouse moves...
(clinch:defevent clinch:*on-mouse-move* (win mouse state x y xrel yrel ts)
  ;;(format t "x:~A y:~A mouse:~A state:~A~%" x y mouse state)

  ;; state is an OR'ed array of press mouse buttons.
  (case state
    ;; Left mouse button down: rotate.
    (1 (clinch:rotate *root*
		      (if *vertical*
			  (q:from-fixed-angles (clinch:degrees->radians (* *mouse-multiplier* yrel)) 0 0)
			  (q:from-fixed-angles 0 (clinch:degrees->radians (* *mouse-multiplier* xrel)) 0))))
    
    ;; Mouse wheel down: translate.
    (2 (clinch:translate *root* (clinch:v! (/ xrel 2) (/ yrel -2) 0)))))

;; Resize the window and change the projection matrix.    
(clinch:defevent clinch:*on-window-resized* (win width height ts)
  (format t "Resized: ~A ~A~%" width height)

  (setf *projection* (clinch::make-perspective-transform (clinch:degrees->radians 45)
							 (/ width height) .1 1000))
  
  
  (setf *ortho-projection*
	(make-orthogonal-transform width height 0 1000))

  (when *texture* (unload *texture))
  
  (setf *texture* 
	(make-instance 'texture 
		       :width width
		       :height height))
  (when *entity* (unload *entity*))
  (setf *entity*
	(make-quad-for-texture *texture* :parent nil))
  (load-texture-from-file *texture* "/home/brad/work/lisp/iss040e081424.jpg")
  
  *texture*)


;; When the mouse wheel moves, translate forward or backward.
(clinch:defevent clinch:*on-mouse-wheel-move* (win mouse x y ts)
  ;;(format t "win=~A mouse=~A x=~A y=~A ts=~A~%" win mouse x y ts)
  (clinch:translate *root* (clinch:v! 0 0 (* y *scroll-multiplier*))))

;; Create the window and start the app.
;; controllers keep the window from restarting, 
;; so I removed them for now.
(clinch:init :init-controllers nil :title "Clinch Presentation")

(defun make-slide (width height deg dist)
  (let* ((n1   (make-instance 'node))
	 (n2   (make-instance 'node))
	 (tex (make-instance 'texture :width (* width 1000) :height (* height 1000)))
	 (e   (make-quad width height :texture tex :parent nil)))
    (clinch:with-context-for-mapped-texture (:texture tex)
	   (clear-cairo-context 0 0 0))

    (rotate n1 (q:from-fixed-angles 0 deg 0))
    (!t n2 0 0 dist t)
    (add-child *root* n1)
    (add-child n1 n2)
    (add-child n2 e)
    
    (values tex
	    e
	    n1
	    n2)))

(defun get-position-for-slide (i n width)
   (let* ((rad (coerce (* -2 (/ pi (length *slides*))) 'single-float))
	  (dist (coerce (/ (/ (+ (* 2 *spacing*) width) 2) (tan (/ rad 2))) 'single-float)))
     ;;(format t "get-pos-rads=~A~%" rad)
     (values (* i rad) dist)))


(defun make-slides (width height)
  (let* ((n (length *slides*))
	 (rad (coerce (* 2 (/ pi n)) 'single-float))
	 (dist (coerce (/ (/ (+ (* 2 *spacing*) width) 2) (tan (/ rad 2))) 'single-float)))
    (format t "RADS=~A~%" rad)

    (setf *distance* dist)
    
    (start-movement 0)
    (!0 *root*)
    (!t *root* 0 0 (- (- dist) -.01))
    ;;(!t *root* 0 0 -5.65 t)
    
    (loop
       for x from 1 to n
       for i from 0 below (* 2 pi) by rad
	 for s in *slides*
       do (progn 
	    (print (list x i dist))
	    (multiple-value-bind (tex e n1 n2)
		(make-slide width height i dist)
	      (render-slide tex x s))))))

(defun render-slide (tex num f)
  (clinch:fast-draw (:texture tex :width-var w :height-var h)
    (clinch:clear-cairo-context 0.9 0.9 0.9)
    (cairo:set-source-rgb .1 .1 .1)
    (cairo:move-to -10 0)
    
    (pango:print-text `("span" (("font_desc" "Century Schoolbook L Roman bold 40"))
			       ,(princ-to-string num))
		      :alignment :pango_align_right)
    (cairo:move-to 100 100)
    ;;(cairo:stroke)
    (print (list (- w 300) h num))
    (funcall f (- w 300) h num)))


		    
      
