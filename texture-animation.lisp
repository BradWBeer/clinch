;;;; texture-animation.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defclass texture-animation (animation) 
  ((frames :accessor frames
	     :initform nil
	     :initarg :frames)))

(defclass texture-animator (animator) 
  ())

(defmethod children ((this texture-animation))
  nil)

(defmethod get-animation-time ((this texture-animation))
  (with-slots ((texs frames)) this
    (when texs
      (caar (last texs)))))
	 
(defmethod get-current-frame ((this texture-animation) (time number))
  (loop for (end . tex) in (slot-value this 'frames)
     if (<= time end) 
     do (return tex)
     finally (return tex)))


(defmethod get-animation-time ((this list))
  (caar (last this)))

(defmethod get-current-frame ((this list) (time number))
  (loop for (end . tex) in this
     if (<= time end) 
     do (return tex)
     finally (return tex)))

