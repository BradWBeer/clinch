;;;; pixel-buffer.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defclass pixel-buffer (buffer)
  ((target :initform :pixel-unpack-buffer))
  (:documentation "Creates a buffer for quickly reading and writing to textures. This is separate from the texture's data."))
	   
