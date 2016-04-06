;;;; index-buffer.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defclass index-buffer (buffer)
  ((id
    :reader id
    :initform nil
    :initarg :id)
   (type
    :accessor qtype
    :initarg :qtype
    :initform :unsigned-int)
   (usage
    :accessor usage
    :initform :static-draw
    :initarg usage)
   (stride
    :reader Stride
    :initform 1
    :initarg :stride)
   (Vertex-Count
    :reader Vertex-Count
    :initform nil
    :initarg :count)
   (target
    :reader target
    :initform :element-array-buffer
    :initarg :target)
   (loaded
    :accessor loaded?
    :initform nil)
   (key :initform (gensym "buffer")
	:reader key))
  (:documentation "Creates and keeps track of GPU index buffer object (shared memory with gpu)."))


(defmethod draw-with-index-buffer ((this index-buffer) &key (mode :triangles))
  "Use this buffer as an index array and draw somthing."
  (gl:bind-buffer (target this) (id this))
  (%gl:draw-elements mode (Vertex-Count this)
		     (qtype this)
		     (cffi:null-pointer)))

(defmethod draw-with-ranged-index-buffer ((this index-buffer) &key (start 0) (end (vertex-count this)) (mode :triangles))
    "Use this buffer as an index array and draw somthing within start and end bounds."
  (gl:bind-buffer (target this) (id this))
  (cl-opengl-bindings:draw-range-elements mode start (vertex-count this) end (qtype this) (cffi:null-pointer)))
  
