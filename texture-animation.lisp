;;;; texture-animation.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defclass texture-animation (animation) 
  ((frames :accessor frames
	     :initform nil
	     :initarg :frames)))

(defclass texture-animator (animator) 
  ())



