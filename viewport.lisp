;;;; node.lisp

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
   (children
    :initform nil
    :initarg :children
    :accessor children)))

(defmethod resize ((this viewport) x y w h)
  (setf (x this) x
	(y this) y
	(width this) w
	(height this) h)

  (gl:viewport x y w h))

(defmethod render ((this viewport) &key)
  (with-accessors ((x x)
		   (y y)
		   (w width)
		   (h height)) this
    (gl:viewport x y w h)
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
  
  
	

