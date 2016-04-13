;;;; entity.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defparameter *current-vao* 0)
(defclass vao ()
  ((id
    :reader id
    :initform nil
    :initarg :id)
   (changed 
    :reader changed?
    :initform t)
   (manual-updating
    :accessor manual-updating
    :initform nil
    :initarg :manual-updating)
   (current-value 
    :initform (make-hash-table :test 'equal))))

(defmethod initalize-instance ((this vao))
  (with-slots ((id id)) this
    (unless id
      (setf id (gl:gen-vertex-arrays 1)))))

(defmethod unload ((this vao) &key)
  "Release buffer resources."
  (trivial-garbage:cancel-finalization this)
  (remove-uncollected this)
  (!
   (when (slot-value this 'id)
     (gl:delete-buffers (list (id this)))))
  (setf (slot-value this 'id) nil))

(defmethod bind ((this vao))
  (with-slots ((id id)) this
    (when id
      (gl:bind-buffer :array-buffer id))))

(defmethod unbind ((this vao))
  (when (id this)
    (gl:bind-buffer :array-buffer 0)))

