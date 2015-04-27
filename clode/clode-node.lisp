(in-package #:clode)

(defclass clode-node (node)
  ((geometry
    :initform nil
    :initarg :geometry
    :reader geometry)))

(defmethod changed? ((this clode-node))
  "Has this node changed and not updated?"
  t)


(defmethod update :before ((this clode-node) &key parent force)
  (with-slots ((geom geometry)) this
    (when geom 
      (setf (clinch:transform this) (ode:get-transform geom)))))

(defmethod initialize-instance :after ((this clode-node) &key parent)

  (with-slots ((geom geometry)) this
    (when geom (ref geom))))

(defmethod (setf geom)  ((new-geom physics-object) (this clode-node))
  (when new-geom (ref new-geom))
  
  (with-slots ((geom geometry)) this
    (when geom (unref geom))))

(defmethod unload :after ((this clode-node) &key)

  (with-slots ((geom geometry)) this
    (when geom (unref geom))))






