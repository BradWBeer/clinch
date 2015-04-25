;;;; refcount.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defclass refcount ()
  ((refcount :reader refcount
	     :initform 0
	     :initarg :refcount)))

(defgeneric ref (this))
(defgeneric unref (this))

(defgeneric unload (this &key))
(defmethod unload (this &key))
(defmethod ref (this))
(defmethod unref (this))

(defmethod unload ((this refcount) &key))

(defmethod ref ((this refcount))
  (with-slots ((count refcount)) this

    (when count
      (incf count))))

(defmethod unref ((this refcount))
  (with-slots ((count refcount)) this

    (when count
      (decf count)
      
      (when (<= count 0)

	(setf count nil)
       	(unload this)))
    count))
  
