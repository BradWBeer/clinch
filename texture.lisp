;;;; texture.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defclass texture (buffer)
  ((tex-id
    :accessor tex-id
    :initform nil
    :initarg :tex-id)
   (width
    :reader width
    :initform nil
    :initarg :width)
   (height
    :reader height
    :initform nil
    :initarg :height)
   (type
    :accessor qtype
    :initarg :qtype
    :initform :unsigned-char)
   (internal-format
    :accessor internal-format
    :initform :rgba
    :initarg :internal-format)
   (data-format
    :accessor data-format
    :initform :bgra
    :initarg :format)
   (stride
    :reader Stride
    :initform 4
    :initarg :stride)
   (target
    :reader target
    :initform :pixel-unpack-buffer
    :initarg :target))
    (:documentation "Creates and keeps track of a texture object and its buffer (shared memory with gpu, sort of)."))



(defmethod initialize-instance :after ((this texture)
				       &key
					 (wrap-s :repeat)
					 (wrap-t :repeat)
					 (mag-filter :linear)
					 (min-filter :linear)
					 depth-texture-mode
					 texture-compare-mode
					 texture-compare-function)
    "Sets up a texture instance.
      type:   cffi type NOTE: use :unsigned-int if you are creating an index buffer.
      id:     OpenGL buffer id
      vcount: vertex count (or number of tuples if not using vertexes)
      stride: The number of values in each pixel.
      target: OpenGL buffer target. If you use this, think about subclassing. For more info lookup glBindBuffer().
      usage:  Tells OpenGL how often you wish to access the buffer. 
      loaded: Has data been put into the buffer. Buffers without data is just future storage, just be sure to set it before you use it.
      format: The OpenGL Format of the Color Data. blue-green-red-alpha is default and prefered for simplicity.
      wrap-s & wrap-t: Wrap texture vertical or horizontal.
      mag-filter & min-filter: Magnification an minimization method."

 
  (with-slots ((tex-id tex-id)
	       (w width)
	       (h height)
	       (this-target target)
	       (dtype type)
	       (eformat data-format)
	       (iformat internal-format)) this
    (sdl2:i-main-thread ()
    (unless tex-id (setf tex-id (car (gl:gen-textures 1))))
    
    (gl:bind-texture :texture-2d (tex-id this))
    (gl:tex-parameter :texture-2d :texture-wrap-s wrap-s)
    (gl:tex-parameter :texture-2d :texture-wrap-t wrap-t)
    (gl:tex-parameter :texture-2d :texture-mag-filter mag-filter)
    (gl:tex-parameter :texture-2d :texture-min-filter min-filter)

    (when depth-texture-mode (gl:Tex-Parameter :TEXTURE-2D :DEPTH-TEXTURE-MODE depth-texture-mode))
    (when texture-compare-mode (gl:Tex-Parameter :TEXTURE-2D :TEXTURE-COMPARE-MODE texture-compare-mode))
    (when texture-compare-function (gl:Tex-Parameter :TEXTURE-2D :TEXTURE-COMPARE-func texture-compare-function))
    
    (gl:bind-buffer (target this) (if (loaded? this)
				      (id this) 
				      0))
    (gl:tex-image-2d :texture-2d 0 iformat w h 0 eformat
		     (cffi-type->gl-type dtype)
		     (cffi:null-pointer))
    tex-id)))
    
    
(defmethod get-size ((this texture) &key)
  "Calculates the number of VALUES (stride * vcount) or (stride * width * height) this buffer contains."
  (* (stride this)
     (if (and (vertex-count this) (not (zerop (vertex-count this))))
	 (vertex-count this)
	 (* (width this) (height this)))))
     


(defmethod bind ((this texture) &key )
  "Wrapper around glBindBuffer. Puts the texture into play."
  (gl:bind-buffer (target this) (id this))
  (gl:bind-texture :texture-2d (tex-id this)))

(defmethod unbind ((this texture) &key )
  (gl:bind-buffer (target this) 0)
  (gl:bind-texture :texture-2d 0))


(defmethod map-buffer ((this texture) &optional (access :READ-WRITE))
  "Returns a pointer to the texture data. YOU MUST CALL UNMAP-BUFFER AFTER YOU ARE DONE!
   Access options are: :Read-Only, :Write-Only, and :READ-WRITE. NOTE: Using :read-write is slower than the others. If you can, use them instead."
  (bind this)
  (gl:map-buffer (target this) access))

(defmethod unmap-buffer ((this texture))
  "Release the pointer given by map-buffer. NOTE: THIS TAKES THE BUFFER OBJECT, NOT THE POINTER! ALSO, DON'T TRY TO RELASE THE POINTER."
  (gl:unmap-buffer (target this))
  (gl:bind-texture  :texture-2d (tex-id this))
  (gl:Tex-Image-2D :texture-2d 0 (internal-format this)  (width this) (height this) 0 :bgra (cffi-type->gl-type (qtype this)) (cffi:null-pointer))
  
  (gl:bind-texture :texture-2d 0)
  (gl:bind-buffer (target this) 0))

(defmethod map-buffer-asynchronous ((this texture) &optional (access :READ-WRITE) (start 0) (end (size-in-bytes this)))
  "Returns a pointer to the texture data. YOU MUST CALL UNMAP-BUFFER AFTER YOU ARE DONE!
   Access options are: :Read-Only, :Write-Only, and :READ-WRITE. NOTE: Using :read-write is slower than the others. If you can, use them instead."
  (sdl2:in-main-thread ()
  (bind this)
  (gl:map-buffer (target this) access)))

(defmethod unmap-buffer-asynchronous ((this texture))
  "Release the pointer given by map-buffer. NOTE: THIS TAKES THE BUFFER OBJECT, NOT THE POINTER! ALSO, DON'T TRY TO RELASE THE POINTER."
  (sdl2:in-main-thread ()
  (gl:unmap-buffer (target this))
  (gl:bind-texture  :texture-2d (tex-id this))
  (gl:Tex-Image-2D :texture-2d 0 (internal-format this)  (width this) (height this) 0 :bgra (cffi-type->gl-type (qtype this)) (cffi:null-pointer))
  
  (gl:bind-texture :texture-2d 0)
  (gl:bind-buffer (target this) 0)))


(defmethod bind-sampler ((this texture) shader name tex-unit)
  "Shaders pass information by using named values called Uniforms. Textures are passed using Samplers. This sets a texture to a sampler uniform" 
  (gl:active-texture (+ (cffi:foreign-enum-value '%gl:enum :texture0) tex-unit))
  (attach-uniform shader name tex-unit)
  (gl:bind-texture :texture-2d (tex-id this)))


(defmethod unload :before ((this texture) &key)
  (sdl2:in-main-thread ()
  (gl:delete-textures (list (tex-id this)))))


(defmacro with-mapped-texture ((name buffer &optional (access :READ-WRITE)) &body body)
  "Convenience macro for mapping and unmapping the texture data.
Name is the symbol name to use for the buffer pointer.
Just a passthrough to with-mapped-buffer, but I keep forgetting to use with-mapped-buffer."
  `(with-mapped-buffer (,name ,buffer ,access)
     ,body))
     

(defmacro texture (&body rest)

  `(make-instance 'texture ,@rest))
