;;;; buffer.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defun cffi-type->gl-type (type)
  (case type
    (:unsigned-char :unsigned-byte)
    (otherwise type)))

(defclass buffer ()
  ((id
    :reader id
    :initform nil
    :initarg :id)
   (type
    :accessor qtype
    :initarg :qtype
    :initform :float)
   (usage
    :accessor usage
    :initform :static-draw
    :initarg usage)
   (stride
    :reader Stride
    :initform 3
    :initarg :stride)
   (Vertex-Count
    :reader Vertex-Count
    :initform nil
    :initarg :count)
   (target
    :reader target
    :initform :array-buffer
    :initarg :target)
   (loaded
    :accessor loaded?
    :initform nil))
  (:documentation "Creates and keeps track of buffer object (shared memory with gpu, sort of). Base class of texture class."))


(defmethod initialize-instance :after ((this buffer) &key data)
  "Sets up a buffer instance.
      type:   cffi type NOTE: use :unsigned-int if you are creating an index buffer.
      id:     OpenGL buffer id
      vcount: vertex count (or number of tuples if not using vertexes)
      stride: The number of items in each vertex (or each tuple.) NOTE: use 1 if you are creating an index buffer. 
      target: OpenGL buffer target. If you use this, think about subclassing. For more info lookup glBindBuffer().
              NOTE: use :element-array-buffer if you are creating an index buffer.
      usage:  Tells OpenGL how often you wish to access the buffer. 
     loaded: Has data been put into the buffer. Buffers without data is just future storage, just be sure to set it before you use it."
  (with-slots ((type    type)
	       (id      id)
	       (vcount  vertex-count)
	       (target  target)
	       (stride  stride)
	       (usage   usage)
	       (loaded? loaded))  this

    ;; if they didn't give a vcount, see if we can derive one...
    (when (and (not vcount) (listp data))
      (setf vcount (length data)))
    
    (unless id
      (setf id (car (gl:gen-buffers 1))))
    
    (gl:bind-buffer target id)

    (cond
      ;; Raw CFFI data?
      ((cffi:pointerp data)

       (%gl:Buffer-Data target
			(* (get-size this) (cffi:foreign-type-size type))
			data
			usage)
       (setf loaded? t))
      
      ;; Data in List?
      (data

       (let ((p (cffi:foreign-alloc type :initial-contents data :count (get-size this))))
	      (unwind-protect
		   (progn (%gl:Buffer-Data target
					   (* (get-size this) (cffi:foreign-type-size type))
					   p
					   usage)
			  (setf loaded? t))
		
		(cffi:foreign-free p))))
      
      ;; No Data?
      (t

       (%gl:Buffer-Data target
			  (* (get-size this) (cffi:foreign-type-size type))
			  (cffi:null-pointer)
			  usage)
	 (setf loaded? nil)))))


(defmethod bind ((this buffer) &key )
  "Wrapper around glBindBuffer. Puts the buffer into play."
  (gl:bind-buffer (target this) (id this)))


(defmethod get-size ((this buffer) &key)
  "Calculates the number of VALUES (stride + vcount) this buffer contains."
  (* (stride this) (vertex-count this)))


(defmethod size-in-bytes ((this buffer))
  "Calculates how many bytes this buffer consists of."
  (* 
   (slot-value this 'vertex-count)
   (slot-value this 'stride)
   (cffi:foreign-type-size (slot-value this 'type))))


(defmethod bind-buffer-to-vertex-array ((this buffer))
  "Use buffer in shader for the vertex array: The built-in variable gl_Vertex."
  (gl:Enable-Client-State :VERTEX-ARRAY)
  (gl:bind-buffer (target this) (id this))
  (%gl:vertex-pointer (stride this) (qtype this) 0 (cffi:null-pointer)))


(defmethod bind-buffer-to-normal-array ((this buffer))
  "Use buffer in shader for the vertex array: The built-in variable gl_Vertex."
  (gl:Enable-Client-State :NORMAL-ARRAY)
  (gl:bind-buffer (target this) (id this))
  (%gl:normal-pointer (qtype this) 0 (cffi:null-pointer)))


(defmethod bind-buffer-to-attribute-array ((this buffer) (shader shader) name)
  "Bind buffer to a shader attribute."
  (let ((id (cddr (get-attribute-id shader name))))
    
    (gl:enable-vertex-attrib-array id)
    (gl:bind-buffer (target this) (id this))
    (gl:vertex-attrib-pointer id
			      (stride this)
			      (qtype this)
			      0 0 (cffi:null-pointer))))


(defmethod draw-with-index-buffer ((this buffer))
  "Use this buffer as an index array and draw somthing."
  (gl:bind-buffer (target this) (id this))
  (%gl:draw-elements :triangles (Vertex-Count this)
		     (qtype this)
		     (cffi:null-pointer)))


(defmethod map-buffer ((this buffer) &optional (access :READ-WRITE))
  "Returns a pointer to the buffer data. YOU MUST CALL UNMAP-BUFFER AFTER YOU ARE DONE!
   Access options are: :Read-Only, :Write-Only, and :READ-WRITE. NOTE: Using :read-write is slower than the others. If you can, use them instead."
  (gl:bind-buffer (target this) (id this))
  (gl:map-buffer (target this) access))

(defmethod unmap-buffer ((this buffer))
  "Release the pointer given by map-buffer. NOTE: THIS TAKES THE BUFFER OBJECT, NOT THE POINTER! ALSO, DON'T TRY TO RELASE THE POINTER."
  (gl:unmap-buffer (target this))
  (gl:bind-buffer (target this) 0))

(defmethod unload ((this buffer) &key)
  "Release buffer resources."
  (gl:delete-buffers (list (id this))))

(defmacro with-mapped-buffer ((name buffer &optional (access :READ-WRITE)) &body body)
  "Convenience macro for mapping and unmapping the buffer data.
   Name is the symbol name to use for the buffer pointer."
  `(let ((,name (clinch::map-buffer ,buffer ,access)))
     (unwind-protect
	  (progn ,@body)
       (clinch::unmap-buffer ,buffer))))
  
  