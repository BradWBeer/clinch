(in-package #:clode)

(defclass clode-node (node)
  ((geometry
    :initform nil
    :initarg :geometry
    :reader geometry)))

(defmethod update :before ((this node) &key parent force)
  )
  ;;(setf (transform this) (ode:get-transform (body (geometry this)))))


