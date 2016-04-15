;;;; animation.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defclass animation () 
  ((frames :accessor frames
	   :initform nil
	   :initarg :frames)))


(defclass animator () 
  ((animation :accessor animation
	      :initform nil
	      :initarg :animation)
   (current-time :accessor current-time
		 :initform 0
		 :initarg :start)
   (paused :accessor paused
	   :initform t
	   :initarg :paused)
   (repeat :accessor repeat
	   :initform t
	   :initarg :repeat)
   (run-speed :accessor run-speed
	      :initform 1
	      :initarg :run-speed)
   (run-length :accessor run-length 
	       :initform nil
	       :initarg :run-length)
   (last-update-time :initform nil)))
		     

(defmethod get-animation-time (this))

(defmethod get-keyframe (this time &key))

(defmethod get-animation-time ((this list))
  (caar (last this)))

(defmethod get-animation-time ((this vector))
	   (car (aref this (1- (length this)))))

(defmethod get-animation-time ((this animation))
  (get-animation-time (frames this)))

(defmethod get-animation-time ((this animator))
  (get-animation-time (animation this)))

(defmethod get-keyframe ((this list) (time number) &key)
  (loop for (end . tex) in this
     if (<= time end) 
     do (return tex)
     finally (return tex)))

(defmethod get-keyframe ((this vector) (time number) &key)
  (loop for i from 0 below (length this)
     for (end . tex) = (aref this i)
     if (<= time end) 
     do (return tex)
     finally (return tex)))

(defmethod get-keyframe ((this animation) (time number) &key)
  (get-keyframe (frames this) time))

(defmethod get-keyframe ((this animator) time &key)
  (get-keyframe (animation this) time))

(defgeneric play (this &key))
(defgeneric stop (this &key))
(defgeneric pause (this &key))

;; Not yet implemented!
(defgeneric skip (this position &key))

(defmethod update ((this animator) &key (time *ticks*))
  (unless (paused this)
    (with-slots ((lt last-update-time)) this
      (unless lt (setf lt time))
      
      (with-accessors ((ct current-time)) this
	(let ((new-time (* (- time (or lt time))
			   (run-speed this))))
	  (setf lt time
		ct (if (repeat this)
		       (mod (+ ct new-time) (get-animation-time this))
		       (min (+ ct new-time) (get-animation-time this)))))))))

(defmethod render ((this animator) &key time)
  (update this)
  (get-keyframe this
		(or time
		    (current-time this))))

(defmethod play ((this animator) &key)
  (setf (paused this) nil))

(defmethod pause ((this animator) &key)
  (setf (paused this) t))

(defmethod stop ((this animator) &key)
  (pause this)
  (setf (current-time this) 0))
		      				   
  
  
