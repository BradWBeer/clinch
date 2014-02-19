;;;; clinch-glfw.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defclass window (element)
  ((projection-matrix :initform nil
		      :initarg :projection-matrix
		      :accessor projection-matrix)
   (width :initform 800
	  :initarg :width
	  :reader width)
   (height :initform 600
	   :initarg :height
	   :reader height)
   (clear-color :accessor clear-color
	       :initform nil
	       :initarg  :clear-color)))


(defmethod attribute :around ((this window) key)
  
  (cond
    ((eql 'window-width key)    (values (width this) this))
    ((eql 'window-height key)   (values (height this) this))
    (t (call-next-method))))


(defmethod (setf atribute) :around (new-val (this window) key)

  (unless (slot-value 'attribute this)
    (setf (slot-value 'attribute this) (make-hash-table)))
  
  (case key
    
    (window-width    (setf (width this)  new-val))
    (window-height   (setf (height this) new-val))
    (otherwise (call-next-method))))


(defmacro window (&rest args)
  
  (multiple-value-bind (keys children) (clinch::split-keywords args)
    
    `(make-instance 'window ,@keys :children (list ,@children))))


(defmethod print-object ((this window) s)

  (format s "(window ")
  (when (name     this)     (format s ":name ~S " (name this)))
  (when (id       this)      (format s ":id ~S "   (id this)))
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

(defmethod start ((this window))
  (declare (optimize (speed 3)))
  (glfw:do-window (:title "Tutorial 5"
              :redbits 8
              :greenbits 8
              :bluebits 8
              :alphabits 8
              :depthbits 16)
      
      ((glfw:set-window-size-callback (lambda (width height) (window-resize-callback this width height)))
       (init this))
    
    (main-loop this))
  
  ;; End Program
  (clean-up this))

