(in-package :clinch)

(defclass skeleton ()
  ((bones
    :accessor bones
    :initform nil
    :initarg :bones)
   (bone-weights
    :accessor bone-weights
    :initform nil
    :initarg :bone-weights)))

