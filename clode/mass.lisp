
(in-package #:clode)

(defclass physics-mass (refcount)
  ((pointer :initform (foreign-alloc 'dmass)
	    :initarg :pointer
	    :reader pointer)))

(defmethod unload ((this physics-mass) &key)
  (foreign-free (pointer this))
  (setf (slot-value this 'pointer) nil))

