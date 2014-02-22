;;;; node.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defclass viewport (element)
  ((x
    :initform 0
    :initarg :x
    :accessor x)
   (y
    :initform 0
    :initarg :y
    :accessor y)
   (width
    :initform 0
    :initarg :width)
   (height
    :initform 0
    :initarg :height)
   (clear-color
    :accessor clear-color
    :initform nil
    :initarg  :clear-color)))


(defmethod initialize-instance :after ((this viewport) &key))


(defmethod resize ((this viewport) x y w h)
  (setf (x this) x
	(y this) y
	(width this) w
	(height this) h)

  (gl:viewport x y w h))

(defmethod width ((this viewport))
  (with-slots ((w width)) this
    (if (zerop w)
	(attribute this 'window-width)
	w)))

(defmethod (setf width) (new-val (this viewport))
  (setf (slot-value this 'width) new-val))

(defmethod height ((this viewport))
  (with-slots ((w height)) this
    (if (zerop w)
	(attribute this 'window-height)
	w)))


(defmethod (setf height) (new-val (this viewport))
  (setf (slot-value this 'height) new-val))

(defmethod update ((this viewport) &key parent force)
  "Update this and child nodes if changed."
  
  )

(defmethod render ((this viewport) &key)

  (gl:enable :blend :depth-test :line-smooth :point-smooth :polygon-smooth :texture-2d :cull-face :scissor-test)
  (%gl:blend-func :src-alpha :one-minus-src-alpha)
  

  (with-accessors ((x x)
		   (y y)
		   (w width)
		   (h height)) this
    (format t "x: ~A y: ~A w: ~A h: ~A~%" x y w h)

    (gl:scissor x y w h)
    (gl:viewport x y w h)

    (when (clear-color this)
      (destructuring-bind (&optional (r 0) (g 0) (b 0) (a 1)) (clear-color this)
	(gl:scissor 0 0 w h)
	
	(gl:clear-color r g b a)
	(gl:clear :color-buffer-bit :depth-buffer-bit)))
  
    (loop for c in (children this)
       if (active c)
       do (render c :w (width this) :h (height this)))))

(defmethod quick-set ((this viewport) x y w h)
  (setf (x this) x
	(y this) y
	(width this) w
	(height this) h))

(defmethod add-child ((this viewport) child &key)
  (with-accessors ((children children)) this
    (unless (member child children)
      (setf children
	    (cons child children)))))


(defmethod print-object ((this viewport) s)

  (format s "(viewport ")
  (when (name     this)     (format s ":name ~S " (name this)))
  (when (id       this)      (format s ":id ~S "   (id this)))
  (when (x        this)      (format s ":x ~S "   (x this)))
  (when (y        this)      (format s ":y ~S "   (y this)))
  ;; (when (width this)         (format s ":width ~S "   (width this)))
  ;; (when (height   this)      (format s ":height ~S "   (height this)))
  ;; (when (clinch:transform   this)      (format s ":transform ~S "   (clinch:transform this)))
  ;;(when (camera   this)      (format s ":camera ~S "   (camera this)))
  
  (when (children this) (format s "~{~%~S~}" (children this)))
  (format s ")"))




(defmacro viewport (&rest args)
  
  (multiple-value-bind (keys children) (split-keywords args)
    
    `(make-instance 'viewport ,@keys :children (list ,@children))))


