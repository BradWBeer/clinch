;; is working file as I test features...please don't use. Use tutorial05 instead.

(ql:quickload :clinch)
(ql:quickload :clinch-cairo)
(ql:quickload :clinch-pango)
(ql:quickload :clinch-freeimage)
(use-package :clinch)

(defparameter *node* nil)
(defparameter *projection* nil)
(defparameter *q* nil)
(defparameter *buttons* nil)

(defclass button ()
  ((node :initform (make-instance 'node)
	 :initarg :node
	 :accessor node)
   (entity :initform nil
	   :initarg :entity
	   :accessor entity)
   (texture :initform nil
	    :initarg :texture
	    :accessor texture)
   (events :initform (make-hash-table)
	   :accessor events)))

(defparameter *click-button* 1)
(defparameter *drag-button* 3)

(defparameter *event-hash* (make-hash-table))
(defparameter *last-hover-target* nil)
(defparameter *last-mouse-down-target* nil)
(defparameter *dragging* nil)

(defmethod initialize-instance :after ((this button) &key width height events)
  (describe this))


(defun init-test ()
  (setf *node* (make-instance 'clinch:node :parent nil))
  (clinch:translate *node* (clinch:v! 0 0 -450))
  ;; (setf *q* (make-quad-and-texture 200 200))
  ;; (add-child *node* *q*)

  ;; Enable a few opengl features.
  (gl:enable :blend :depth-test :line-smooth :point-smooth :texture-2d :cull-face)

  ;; Set the blending mode.
  (%gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:polygon-mode :front-and-back :fill)
  (gl:clear-color .8 .8 .8 0)
  (make-button "Hello world" 600 400)
  )

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

(clinch:defevent clinch:*on-idle* ()

  (gl:clear :color-buffer-bit :depth-buffer-bit)
  ;;(clinch:render entity :projection *projection*)
  (clinch:update *node*)
  (clinch:render *node* :projection *projection*))

(defun map-event (f e)
  (unless (gethash e *event-hash*)
    (setf (gethash e *event-hash*) (make-hash-table)))

  (let ((ret))
    (maphash (lambda (k v)
	       (push (funcall f k v) ret))
	     (gethash e *event-hash*))
    ret))

(clinch:defevent clinch:*on-mouse-down* (win mouse x y button state clicks ts)

  ;;(format t "win: ~A mouse: ~A x: ~A y: ~A button: ~A state: ~A clicks: ~A ts: ~A~%" win mouse x y button state clicks ts)

  (let ((event `((:window . ,win)
		 (:mouse . ,mouse)
		 (:mouse-screen-position ,x . ,y)
		 (:button . ,button)
		 (:state . ,state)
		 (:clicks . ,clicks)
		 (:timestamp . ,ts)))
	(collision-hash (make-hash-table :test 'equal)))

    (map-event (lambda (k v)
		 (multiple-value-bind (collisions start ray) 
		     (memoized-check-intersect (car (children k)) k x y *viewport* *projection* collision-hash) 

		   (when collisions
		     (funcall v k collisions (acons :event :mouse-down
						    (acons :ray-start start
							   (acons :ray ray event))))
		     (when (= button *click-button*)
		       (setf *last-mouse-down-target* k)))))

	       :mouse-down)
		     
    (when (= button *drag-button*)
      (map-event (lambda (k v)
		   (multiple-value-bind (collisions start ray) 
		       (memoized-check-intersect (car (children k)) k x y *viewport* *projection* collision-hash) 
		     
		     (when collisions
		       (setf *dragging* k)
		       (funcall v k collisions (acons :event :mouse-drag-start
						      (acons :ray-start start
							     (acons :ray ray event)))))))
		         
		 :mouse-drag-start))))

    
(clinch:defevent clinch:*on-mouse-up* (win mouse x y button state clicks ts)
  
  ;;(format t "win: ~A mouse: ~A x: ~A y: ~A button: ~A state: ~A clicks: ~A ts: ~A~%" win mouse x y button state clicks ts)
    (let ((event `((:window . ,win)
		   (:mouse . ,mouse)
		   (:mouse-screen-position ,x . ,y)
		   (:button . ,button)
		   (:state . ,state)
		   (:clicks . ,clicks)
		   (:timestamp . ,ts)))
	  (collision-hash (make-hash-table :test 'equal)))
      
      (map-event (lambda (k v)
		   (multiple-value-bind (collisions start ray) 
		       (memoized-check-intersect (car (children k)) k x y *viewport* *projection* collision-hash)
		     (when collisions
		       (let ((e (acons :ray-start start
				       (acons :ray ray event))))
			 (when (and 
				(= *click-button* button)
				(eq *last-mouse-down-target* k))
			   (let ((h (gethash :mouse-click *event-hash*)))
			     (when h
			       (let ((f (gethash k h)))
				 (when f (funcall f k collisions  (acons :event :mouse-click e)))))))
			 
			 (funcall v k collisions (acons :event :mouse-up e)))
		       (setf *last-mouse-down-target* nil))))
		 
		 :mouse-up)
      (when (= button *drag-button*)
	(map-event (lambda (k v)
		     (multiple-value-bind (collisions start ray) 
			 (memoized-check-intersect (car (children k)) k x y *viewport* *projection* collision-hash)
		       (when (and collisions
				  (eq *dragging* k))
			 (funcall v k collisions (acons :event :mouse-drag-drop 
							(acons :ray-start start
							       (acons :ray ray event)))))))
		   :mouse-drag-drop))))
    

  ;; (loop for n in *buttons*
  ;;     collect (check-intersect (car (children n)) n x y *viewport* *projection*)))

;; Rotate and translate with mouse
(clinch:defevent clinch:*on-mouse-move* (win mouse state x y xrel yrel ts)
  ;;(format t "win: ~A mouse: ~A x: ~A y: ~A button: ~A state: ~A clicks: ~A ts: ~A~%" win mouse state x y xrel yrel ts)

  (let ((event `((:window . ,win)
		 (:mouse . ,mouse)
		 (:mouse-screen-position ,x . ,y)
		 (:state . ,state)
		 (:x-relative ,xrel)
		 (:y-relative . ,yrel)
		 (:timestamp . ,ts))) 
	(cached-collisions (make-hash-table :test 'equal)))

    (map-event (lambda (k v)
		 (multiple-value-bind (collisions start ray)
		     (memoized-check-intersect (car (children k)) k x y *viewport* *projection* cached-collisions)
		   (if collisions
		       (progn 
			 (unless (eq k *last-hover-target*)
			    (setf *last-hover-target* k)
			    (funcall v k collisions (acons :event :mouse-hover event)))
			 (funcall v k collisions (acons :event :mouse-move
							(acons :ray-start start
							       (acons :ray ray event)))))
		       (when (eq k *last-hover-target*)
			 (let ((f (gethash k (gethash :mouse-exit *event-hash*))))
			   (when f (funcall f k nil (acons :event :mouse-exit event)))
			   (setf *last-hover-target* nil))))))
			 
	       :mouse-move)))

    
  
  ;; (loop for n in *buttons*
  ;;    for q = (car (children n))
  ;;    if (some (lambda (x)
  ;; 		(not (null x)))
  ;; 	      (alexandria:flatten (check-intersect q n x y *viewport* *projection*)))
  ;;    do (draw-button q 
  ;; 		     "Hello World!" 		 
  ;; 		     :line-width 12
  ;; 		     :foreground '(.1 .1 .1 1)
  ;; 		     :fill '(0 0 0 .4)
  ;; 		     :line '(0 0 .9 1))
  ;;    else 
  ;;    do (draw-button q 
  ;; 		     "Hello World!" 		 
  ;; 		     :line-width 10
  ;; 		     :foreground '(.1 .1 .1 1)
  ;; 		     :fill '(0 0 0 .2)
  ;; 		     :line '(0 0 .8 1))))



;; on window resize
(clinch:defevent clinch:*on-window-resized* (win width height ts)
  (format t "Resized: ~A ~A~%" width height)

  (setf *projection* (clinch::make-perspective-transform (clinch:degrees->radians 45)
                                                         (/ width height) .1 1000)))

(clinch:defevent clinch:*on-key-down* (win keysym state ts)
  )


(clinch:defevent clinch:*text-editing* (win text ts)
  (format t "text editing: ~A ~A ~A~%" win text ts))

(clinch:defevent clinch:*on-text-input* (win text ts)
  (format t "on-text-event: ~A ~A ~A ~%" win (code-char text) ts))


;; zoom in and out
(clinch:defevent clinch:*on-mouse-wheel-move* (win mouse x y ts)
  ;;(format t "win=~A mouse=~A x=~A y=~A ts=~A~%" win mouse x y ts)

  (clinch:translate *node* (clinch:v! 0 0 (/ y .1))))

(clinch:init :asynchronous t :init-controllers nil)

(defun check-intersect (quad node x y viewport projection)
  (multiple-value-bind (origin ray) (unproject x y (width viewport) (height viewport) (m4:inverse projection))
    (let ((ret (clinch::ray-triangles-intersect (clinch::transform-points
						 (pullg (attribute quad "v"))
						 (clinch::current-transform node))
						(pullg (indexes quad))
						origin
						ray)))
      (when (some (lambda (x)
		    x)
		  ret)
	(values ret origin ray)))))

(defun memoized-check-intersect (quad node x y viewport projection hash)
  (let* ((key (list quad node x y viewport projection))
	 (ret (or (when hash 
		    (gethash key hash))
		  (setf (gethash key hash)
			(multiple-value-list 
			 (check-intersect quad node x y viewport projection))))))
    (values (first ret)
	    (second ret)
	    (third ret))))
  


(defun draw-inactive (quad text)
   (draw-button quad text
		 :line-width 10
		 :foreground '(.1 .1 .1 1)
		 :fill '(0 0 0 .2)
		 :line '(0 0 .8 1)))

(defun draw-hover (quad text)
   (draw-button quad text
		 :line-width 11
		 :foreground '(.0 .0 .0 1)
		 :fill '(0 0 0 .2)
		 :line '(0 0 .9 1)))

(defun draw-pressed (quad text)
   (draw-button quad text
		 :line-width 10
		 :foreground '(.9 .9 .9 1)
		 :fill '(1 1 1 .5)
		 :line '(0 0 .8 1)))

(defun event-handler (n c e)
  (format t "~A~%" e))

(defun make-event-handler (quad text)
  (lambda (n c e)
    (let ((event (cdr (assoc :event e))))
      (cond ((eq event :mouse-down)
    	     (draw-pressed quad text))
    	    ((eq event :mouse-up)    (draw-hover quad text))
	    ((eq event :mouse-hover) (draw-hover quad text))
	    ((eq event :mouse-exit) (draw-inactive quad text)))
      (unless nil ;;(eq event :mouse-move)
	(format t "~A~%" e)))))

(defun add-event-handler (event object func)
  (let ((hash (or (gethash event *event-hash*)
		  (setf (gethash event *event-hash*) (make-hash-table)))))
    (setf (gethash object hash) func)))


(defun make-button (text width height &key line-width line fill background foreground)
  (! (let* ((node (make-instance 'node))
	    (quad (make-quad-and-texture width height))
	    (f (make-event-handler quad text)))
       (draw-pressed quad text)
       (add-child node quad)
       ;;(setf (gethash node *gui-objects*)
       (map nil (lambda (x)
		  (add-event-handler x node f))
	    '(:mouse-click :mouse-hover :mouse-move :mouse-exit :mouse-down :mouse-up :mouse-drag-start :mouse-drag :mouse-drag-drop))
       (add-child *node* node)
       node)))

(defun get-last-button ()
  (car (children *node*)))

(defun remove-button (node)
  (maphash (lambda (k v)
	     (when (gethash node v)
	       (remhash node v)))
	   *event-hash*)
  (setf (enabled node) nil))

(defun delete-all-buttons ()
  (loop for i in (children *node*)
     do (remove-button i))
  (setf (children *node*) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun draw-button (quad text &key
                                (line-width 20)
                                (line '(.1 .1 .1 1))
                                (fill '(0 0 0 0))
                                (background '(0 0 0 0))
                                (foreground '(.1 .1 .1 1)))
  (with-context-for-mapped-texture (:texture (uniform quad "t1") :width-var w :height-var h)
    (let* ((aspect 1.0)
           (corner-radius (/ h 5))
           (radius (/ corner-radius aspect)))
      
      (apply #'clear-cairo-context background)
      (clinch:draw-rounded-rectangle  w h
			       :line-width line-width
			       :line line
			       :fill fill)

      (apply #'cairo:set-source-rgba foreground)
      (cairo:move-to 0 (/ h 3))
      (cairo:move-to 0 0)
      (position-text `("span" (("font_desc" "Mono 70")); "Century Schoolbook L Roman bold 50"))
                              ,text)
                     :width w
                     :height h
                     :%y .4
                     :alignment :pango_align_center))))




;; a = (sb-cga:vec* button-end-position entity-distance)
;; b-pos & b = buttons start and end 
;; d-pos and d-hat = screen position and direction

(defun rejection (a b)
  (let ((n (v3:normalize a)))
    (v3:- b (v3:*s n (v:dot n b)))))

(defun calculate-movement (d-pos d-hat a-pos a b-pos b)
  (let ((a-rel (v3:- a-pos d-pos))
	(b-rel (v3:- b-pos d-pos)))
    (v3:-
     (v3:+ b-rel
	   (v3:*s b
		  ;; find 'x'
		  (/ (v3:dot (v3:- (v3:+ a-rel a)
				   b-rel)
			     d-hat)
		     (v3:dot b d-hat))))
     (v3:+ a-rel a))))


;; Idea...
;; Start = original vector
;; Current = current vector
(defun calculate-movement (start end)
  (v3:- end
	(v3:*s start (v3:dot start end))))


	;; (if (eql mouse-state :dragging)
	    
	;;     (multiple-value-bind (d-pos d) (clinch:get-screen-direction (sb-cga:inverse-matrix lens))
	;;       (let ((trans (calculate-movement d-pos d a-pos a button-start-position button-end-position)))
		
	;; 	(map nil (lambda (node start-pos)
	;; 		   (setf (clinch:transform node)
	;; 			 (sb-cga:matrix* (sb-cga:translate (clinch:make-vector (elt trans 0) (elt trans 1) (elt trans 2)))
	;; 					 start-pos)))
	;; 	     (elt mouse-drag-values 5) mouse-drag-nodes-start-pos)))
