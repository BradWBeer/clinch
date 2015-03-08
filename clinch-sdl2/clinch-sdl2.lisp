;;;; clinch-sdl.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defclass window (element)
  ((title :accessor title
	  :initform NIL
	  :initarg :title)
   (width :initform 800
	  :initarg :width
	  :reader width)
   (height :initform 600
	   :initarg :height
	   :reader height)
   (key-up :initform nil
	   :initarg :key-up
	   :accessor key-up)
   (key-down :initform nil
	     :initarg :key-down
	     :accessor key-down)
   (clear-color :accessor clear-color
	       :initform nil
	       :initarg  :clear-color)
   (hWin)
   (gContext)))



(defmethod attribute :around ((this window) key)
  
  (cond
    ((or (eql 'window-width key)
	 (eql 'width key)) (values (width this) this))

    ((or (eql 'window-height key)
	 (eql 'height key))   (values (height this) this))
    (t (call-next-method))))


(defmethod (setf attribute) :around (new-val (this window) key)

  (unless (slot-value 'attribute this)
    (setf (slot-value 'attribute this) (make-hash-table)))
  
  (case key
    
    (window-width    (setf (width this)  new-val))
    (window-height   (setf (height this) new-val))
    (otherwise (call-next-method))))


(defmacro window ((&key (title "Clinch SDL2") (width 800) (height 600) fullscreen resizable)
			&body args)

  (let ((f-count (gensym))
	(start-time (gensym))
	(last-time (gensym)))    
    
    (multiple-value-bind (keys children) (clinch::split-keywords args)
    
      `(let* ((,f-count 0)
	      (*time*   (sdl2:get-ticks))
	      (*delta-time* (coerce 0 'single-float))
	      (,start-time   (/ *time* 1000))
	      (,last-time ,start-time)	      
	      (*frame-rate* (coerce 0 'single-float))
	      (*frame-count* 0)
	      (*parent* (make-instance 'window ,@keys))
	      (*root*   *parent*))	 

	 
	 (declare (optimize (speed 3)))
	 (let ((*local-stdout* *standard-output*))
	   (sdl2:with-init (:everything)
	     (let ((*standard-output* *local-stdout*))
	       (sdl2:with-window (win :title ,title :w ,width :h ,height :flags '(:shown :opengl :resizable))
		 (sdl2:with-gl-context (gl-context win)
		   (sdl2:gl-make-current win gl-context)
		   
		   (setf (slot-value *parent* 'hwin) win
			 (slot-value *parent* 'gContext) gl-context)
		   

		   ,@children

		   (init *parent*)
		   (window-resize-callback *parent* (width *parent*) (height *parent*))

		   (sdl2:with-event-loop (:method :poll)
		     (:quit () t)
	      
		     (:keydown
		      (:keysym keysym)
		      )
		     
		     (:keyup
		      (:keysym keysym)
		     )
		     
		     (:mousemotion
		      (:x x :y y :xrel xrel :yrel yrel :state state)
		      (format t "Mouse motion abs(rel): ~a (~a), ~a (~a)~%Mouse state: ~a~%"
		     	      x xrel y yrel state)
		      )
		     
		     (:controlleraxismotion
		      (:which controller-id :axis axis-id :value value)
		      (format t "Controller axis motion: Controller: ~a, Axis: ~a, Value: ~a~%"
		     	      controller-id axis-id value))
		     
		     (:controllerbuttondown
		      (:which controller-id)
		      )
		     
		     (:idle
		      
		      ;; figure out framerate...
		      (setf ,f-count (mod (incf *frame-count*) 100))
		      (setf *time*  (/ (sdl2:get-ticks) 1000))
		      (setf *delta-time* (coerce (- *time* ,last-time) 'single-float))
		      
		      (when (zerop ,f-count)
			
		      	(setf *frame-rate*
		      	      (coerce (/ (- *time* ,start-time) 100) 'single-float)
		      	      ,start-time *time*))
		      
		      (main-loop *parent*)
		      (gl:flush)
		      (sdl2:gl-swap-window win)
		     
		      (setf ,last-time *time*))
		     ))
		 
		 
		 
		 (clean-up *parent*)))))))))



(defmethod print-object ((this window) s)

  (format s "(window ")
  (when (title     this)    (format s ":title ~S " (title this)))
  (when (name     this)     (format s ":name ~S " (name this)))
  (when (id       this)      (format s ":id ~S "   (id this)))
  (when (slot-value this 'clear-color) (format s ":clear-color '~S" (clear-color this)))
  (when (children this) (format s "~{~%~S~}" (children this)))
  (format s ")"))


(defmethod init ((this window))
  (gl:enable :blend :depth-test :line-smooth :point-smooth :polygon-smooth :texture-2d :cull-face)
  (%gl:blend-func :src-alpha :one-minus-src-alpha))

(defmethod render ((this window) &key) 

  (when (before-render this)
    (funcall (before-render this) this))

  (with-slots ((w width)
	       (h height)) this
    
    (when (clear-color this)
      
      (gl:scissor 0 0 w h)
      
      (apply #'gl:clear-color (clear-color this))
      (gl:clear :color-buffer-bit :depth-buffer-bit)))
  
  (loop for e in (children this)
       do (render e))

  (when (after-render this)
    (funcall (after-render this) this)))


(defmethod window-resize-callback ((this window) width height)
  
  )

(defmethod main-loop ((this window))
  (render this))

(defmethod clean-up ((this window))

  )





