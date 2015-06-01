(in-package #:clode)

(defclass spring ()
  ())

(defclass physics-player ()
  ((head :initform nil
	 :initarg :head
	 :reader head)
   (legs :initform nil
	 :initarg :legs
	 :reader legs)
   (up :initform '(0 1 0)
       :initarg :up
       :reader up-vector)
   (direction :initform '(1 0 0)
	      :initarg :direction
	      :reader direction-vector)
   (max-speed :initform 1
	      :initarg :max-speed
	      :accessor max-speed)))
   
