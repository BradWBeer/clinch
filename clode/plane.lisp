
(in-package #:clode)

(defclass physics-plane (physics-object)
  ((normal :initform '(0 1 0)
	   :initarg :normal
	   :reader normal)
   (offset :initform 0
	   :initarg :offset
	   :reader offset)))

(defmethod initialize-instance :after ((this physics-plane) &key (normal '(0 1 0)) (position 0))

  (when (body this)
    (error "physics-planes do not have physics-body objects"))

  (setf (slot-value this 'geometry) (create-plane (pspace this)
						  (first normal)
						  (second normal)
						  (third normal)
						  position)))
