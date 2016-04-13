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
    :initarg :width
    :accessor width)
   (height
    :initform 0
    :initarg :height
    :accessor height)
   (clear-color
    :accessor clear-color
    :initform nil
    :initarg  :clear-color))
  (:documentation "Creates a viewport."))


(defmethod initialize-instance :after ((this viewport) &key)
  "Creates a new viewport."
  (with-accessors ((x x)
		   (y y)
		   (width width)
		   (height height)) this
    (when (and x y width height)
      (render this))))


(defmethod resize ((this viewport) x y w h)
  "Resize the viewport."
  (setf (x this) x
	(y this) y
	(width this) w
	(height this) h)

  (render this))

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
    (!!
      (gl:scissor x y w h)
      (gl:viewport x y w h)
      
      (when (clear-color this)
	(destructuring-bind (&optional (r 0) (g 0) (b 0) (a 1)) (clear-color this)
	  (gl:clear-color r g b a))))))
    
(defmethod quick-set ((this viewport) x y w h)
  "A quick method to set all the values in the viewport." 
  (setf (x this) x
	(y this) y
	(width this) w
	(height this) h)
  (render this))

(defmacro with-viewport ((vp) &body body)
  "A wrapper which sets and unsets a viewport."
  (let ((old (gensym)))
    `(let ((,old *viewport*)
	   (*viewport* ,vp))
       (render *viewport*)
       (unwind-protect
	    (progn ,@body)
	 (render ,old)))))

