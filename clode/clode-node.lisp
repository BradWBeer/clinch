(in-package #:clode)

(defclass clode-node (node)
  ((geometry
    :initform nil
    :initarg :geometry
    :reader geometry)))

(defmethod changed? ((this clode-node))
  "Has this node changed and not updated?"
  t)


(defmethod set-transform-to-geom ((this clode-node) &key)
  (with-slots ((geom geometry)) this
    (when geom 
      (setf (clinch:transform this) (get-transform geom)))))


(defmethod update :before ((this clode-node) &key parent force)
  (when (enabled this)
    (with-slots ((geom geometry)) this
      (when geom 
	(setf (slot-value this 'clinch:transform) (ode:get-transform geom))))))


(defmethod initialize-instance :after ((this clode-node) &key parent)

  (with-slots ((geom geometry)) this
    (when geom (ref geom))))

(defmethod (setf geom)  ((new-geom physics-object) (this clode-node))
  (when new-geom (ref new-geom))
  
  (with-slots ((geom geometry)) this
    (when geom (unref geom))))

(defmethod (setf transform) :around ((other-node array) (this clode-node))
  "Inherited function for setting changed?"
  (with-slots ((geom geometry)
	       (tran transform)) this
    (when geom 
      
      (setf tran (ode:get-transform geom))
      
      (call-next-method)
      (geom-set-rotation (geometry geom) tran)
      (geom-set-position (geometry geom) (aref tran 12) (aref tran 13) (aref tran 14)))))
			 


(defmethod unload :after ((this clode-node) &key)

  (with-slots ((geom geometry)) this
    (when geom (unref geom))))






