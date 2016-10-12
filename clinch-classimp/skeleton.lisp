(in-package :clinch)

(defclass skeleton (bone)
  ((skeleton
    :accessor skeleton
    :initform nil
    :initarg :skeleton)
   (weights
    :accessor weights
    :initform nil
    :initarg :weights)
   ;; It might be wrong to keep these here, 
   ;; but I'm punting until I figure a better 
   ;; way.
   (bone-buffer
    :accessor bone-buffer
    :initform nil
    :initarg :bone-buffer)
   (weights-buffer
    :accessor weights-buffer
    :initform nil
    :initarg :weights-buffer)))


(defmethod generate-buffers ((this skeleton) &key)
  )

(defmethod generate-bone-buffer ((this skeleton) &key)

  )

