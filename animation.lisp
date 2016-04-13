;;;; animation.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defclass animation () 
  ((frames :accessor frames
	   :initform nil
	   :initarg :frames)
   (children :accessor children
	     :initform nil
	     :initarg :children)))

(defclass animator () 
  ((animation :accessor animation
	      :initform nil
	      :initarg :animation)
   (current-time :accessor current-time
		     :initform 0.0
		     :initarg :start)
   (paused :accessor paused
	   :initform nil
	   :initarg :paused)
   (repeat :accessor repeat
	   :initform t
	   :initarg :repeat)
   (run-speed :accessor run-speed
	      :initform 1
	      :initarg :run-speed)
   (run-length :accessor run-length 
	       :initform nil
	       :initarg :run-length)))

(defmethod get-animation-time (this))

(defmethod get-keyframe (this time &key))

(defmethod get-animation-time ((this list))
  (caar (last this)))

(defmethod get-animation-time ((this vector))
	   (car (aref this (1- (length this)))))

(defmethod get-animation-time ((this animation))
  (caar (last (frames this))))

(defmethod get-animation-time ((this animator))
  (caar (last (frames (animation this)))))

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
  (loop for (end . tex) in (frames this)
     if (<= time end) 
     do (return tex)
     finally (return tex)))

(defmethod get-keyframe ((this animator) time &key)
  (loop for (end . tex) in (frames (animation this))
     if (<= time end) 
     do (return tex)
     finally (return tex)))


(defgeneric play (this &optional delta-time))
(defgeneric stop (this &key))
(defgeneric pause (this &key))
(defgeneric skip (this position &key))

(defmethod play ((this animator) &optional (delta-time *delta-time*))
  (unless (paused this)
    (let* ((ct (incf (current-time this) (* delta-time (run-speed this))))
	   (len (get-animation-time this))
	   (r? (repeat this)))
      (get-keyframe (animation this)
		    (cond ((eql r? t) (mod ct len))
			  ((null r?)  (min ct len))
			  ((numberp r?) (min ct (* len r?)))
			  (t 0))))))
		      				   
  
  
