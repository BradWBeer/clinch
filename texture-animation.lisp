;;;; texture-animation.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defclass texture-animation (animation) 
  ((textures :accessor textures
	     :initform nil
	     :initarg :textures)))
   
(defmethod children ((this texture-animation))
  nil)

(defmethod get-animation-time ((this texture-animation))
  (with-slots ((texs textures)) this
    (when texs
      (caar (last texs)))))
	 
(defmethod get-current-frame ((this texture-animation) (time number))
  (loop for (end . tex) in (slot-value this 'textures)
     if (<= time end) 
     do (return tex)
     finally (return tex)))

