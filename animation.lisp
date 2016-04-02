;;;; animation.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defclass animation () 
  ((children :accessor children
	     :initform nil
	     :initarg :children)))

(defgeneric cycle (animation))
(defmethod cycle ((this animation))
  (reduce (lambda (a b)
	    (max (cycle a)
		 (cycle b)))
	  (children this)))

(defclass animator () 
  ((current-position :accessor current-position
		     :initform 0.0
		     :initarg :start-position)
   (repeat :accessor repeat
	   :initform t
	   :initarg :repeat)
   (run-speed :accessor run-speed
	  :initform 1
	  :initarg :run-speed)
   (run-length :accessor run-length 
	       :initform nil
	       :initarg :run-length)))

(defgeneric play (&key start-time length run-speed repeat))
(defgeneric stop (&key))
(defgeneric pause (&key))
(defgeneric skip (position &key))


