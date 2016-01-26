;;;; viewport.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defclass viewport ()
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
    :initarg  :clear-color)
   (enables
    :accessor enables
    :initarg  :enables
    :initform '(:blend :depth-test :line-smooth :point-smooth :polygon-smooth :texture-2d :cull-face :scissor-test))
   (disables
    :accessor disables
    :initarg  :disables
    :initform nil)
   (blend-fun
    :accessor blend-func
    :initarg  :blend-func
    :initform '(:src-alpha :one-minus-src-alpha))
   (projection-transform
    :accessor projection-transform
    :initform nil
    :initarg  :projection-transform)
   (camera-node
    :accessor camera-node
    :initform nil
    :initarg  :camera-node))
  (:documentation "Creates a viewport. Maybe it can use a camera?"))


(defmethod initialize-instance :after ((this viewport) &key)
  "Creates a new viewport."
  )


(defmethod resize ((this viewport) x y w h)
  "Resize the viewport."
  (setf (x this) x
	(y this) y
	(width this) w
	(height this) h)

  (gl:viewport x y w h))

(defmethod width ((this viewport))
  "Get viewport width."
  (with-slots ((w width)) this
    w))

(defmethod (setf width) (new-val (this viewport))
  "Set viewport width."
  (setf (slot-value this 'width) new-val))

(defmethod height ((this viewport))
  "Get the viewport height."
  (with-slots ((h height)) this
    h))

(defmethod (setf height) (new-val (this viewport))
  "Set the viewport height."
  (setf (slot-value this 'height) new-val))

(defmethod render ((this viewport) &key projection)
  "Makes this viewport active."
  (with-accessors ((x x)
		   (y y)
		   (w width)
		   (h height)) this
    
    (gl:scissor x y w h)
    (gl:viewport x y w h)
    
    (when (clear-color this)
      (destructuring-bind (&optional (r 0) (g 0) (b 0) (a 1)) (clear-color this)
	(gl:scissor x y w h)
	
	(gl:clear-color r g b a)
	(gl:clear :color-buffer-bit :depth-buffer-bit)))
    ;;; Remove this cruft!!!
    (when (projection-transform this)
      (gl:matrix-mode :projection)
      (gl:load-matrix (projection-transform this))
      (gl:matrix-mode :modelview))))
    
(defmethod quick-set ((this viewport) x y w h)
  "A quick method to set all the values in the viewport." 
  (sdl2:in-main-thread ()
  (setf (x this) x
	(y this) y
	(width this) w
	(height this) h)))

;; (defmethod print-object ((this viewport) s)

;;   (format s "(viewport ")
;;   (when (name     this)     (format s ":name ~S " (name this)))
;;   (when (id       this)      (format s ":id ~S "   (id this)))
;;   (when (x        this)      (format s ":x ~S "   (x this)))
;;   (when (y        this)      (format s ":y ~S "   (y this)))
;;   (when (slot-value this 'width)         (format s ":width ~S "   (width this)))
;;   (when (slot-value this 'height)      (format s ":height ~S "   (height this)))
;;   (when (slot-value this 'clear-color)      (format s ":clear-color '~S "   (clear-color this)))
;;   ;; (when (clinch:transform   this)      (format s ":transform ~S "   (clinch:transform this)))
;;   ;;(when (camera   this)      (format s ":camera ~S "   (camera this)))
  
;;   ;; (when (children this) (format s "~{~%~S~}" (children this)))
;;   ;; (format s ")")

;;   )

