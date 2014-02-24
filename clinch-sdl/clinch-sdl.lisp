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
   (clear-color :accessor clear-color
	       :initform nil
	       :initarg  :clear-color)
   (garbage :initform (make-hash-table))))


(defmethod attribute :around ((this window) key)
  
  (cond
    ((or (eql 'window-width key)
	 (eql 'width key)) (values (width this) this))

    ((or (eql 'window-height key)
	 (eql 'height key))   (values (height this) this))
    (t (call-next-method))))


(defmethod (setf atribute) :around (new-val (this window) key)

  (unless (slot-value 'attribute this)
    (setf (slot-value 'attribute this) (make-hash-table)))
  
  (case key
    
    (window-width    (setf (width this)  new-val))
    (window-height   (setf (height this) new-val))
    (otherwise (call-next-method))))


(defmacro window (&body args)
  (multiple-value-bind (keys children) (clinch::split-keywords args)
    
    (let ((sym (gensym)))
      `(let* ((*parent* (make-instance 'window ,@keys)))
	 
	 (declare (optimize (speed 3)))
	 (sdl:with-init ()
	   (sdl:window (width *parent*) (height *parent*)
		       :flags sdl-cffi::sdl-opengl
		       :double-buffer t
		       :resizable t
		       :title-caption (title *parent*)
		       :icon-caption  (title *parent*))
	   ,@children
	   (init *parent*)
	   (window-resize-callback *parent* (width *parent*) (height *parent*))
	   (setf (sdl:frame-rate) 60)
	   
	   (sdl:with-events ()
	     (:quit-event () t)
	     (:VIDEO-RESIZE-EVENT (:W W :H H) 
				  (window-resize-callback *parent* w h))
	     (:idle ()         
		    (main-loop *parent*)
		    (sdl:update-display)))
	   (clean-up *parent*))))))



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
  
  (with-slots ((w width)
	       (h height)) this
    
    (when (clear-color this)
      (destructuring-bind (&optional (r 0) (g 0) (b 0) (a 1)) (clear-color this)
	(gl:scissor 0 0 w h)
	
	(gl:clear-color r g b a)
	(gl:clear :color-buffer-bit :depth-buffer-bit))))
  
  
  (loop for e in (children this)
       do (render e))
  )

(defmethod window-resize-callback ((this window) width height)
  
  )

(defmethod main-loop ((this window))
  (render this))

(defmethod clean-up ((this window))

  )



