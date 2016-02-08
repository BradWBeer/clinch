;;;; texture.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)  

(defclass texture ()
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
   (Vertex-Count
    :reader Vertex-Count
    :initform nil
    :initarg :count)
   (key :initform (gensym "texture")
	:reader key))
    (:documentation "Creates and keeps track of a texture object. Can be used with a pixelbuffer to speed things up."))
 


(defmethod initialize-instance :after ((this texture)
				       &key
					 (PBO nil)
					 (wrap-s :repeat)
					 (wrap-t :repeat)
					 (mag-filter :linear)
					 (min-filter :linear)
					 depth-texture-mode
					 texture-compare-mode
					 texture-compare-function
					 data)
    "Sets up a texture instance with gc finalizer. Do not depend on finalizers, release resources manually if you can.
      type:   cffi type NOTE: use :unsigned-int if you are creating an index buffer.
      id:     OpenGL buffer id
      vcount: vertex count (or number of tuples if not using vertexes)
      stride: The number of values in each pixel.
      usage:  Tells OpenGL how often you wish to access the buffer. 
      loaded: Has data been put into the buffer. Buffers without data is just future storage, just be sure to set it before you use it.
      format: The OpenGL Format of the Color Data. blue-green-red-alpha is default and prefered for simplicity.
      wrap-s & wrap-t: Wrap texture vertical or horizontal.
      mag-filter & min-filter: Magnification an minimization method."

 
  (with-slots ((tex-id tex-id)
	       (w width)
	       (h height)
	       (dtype type)
	       (eformat data-format)
	       (iformat internal-format)) this
    (sdl2:in-main-thread ()
    (unless tex-id (setf tex-id (car (gl:gen-textures 1))))

    (trivial-garbage:cancel-finalization this)
    (setf (gethash (key this) *uncollected*) this)
    (trivial-garbage:finalize this 
			      (let ((tex-id-value tex-id)
				    (key (key this)))
				(lambda () (sdl2:in-main-thread (:background t)
					     (remhash key *uncollected*)
					     (gl:delete-textures (list tex-id-value))))))
    
    (gl:bind-texture :texture-2d (tex-id this))
    (gl:tex-parameter :texture-2d :texture-wrap-s wrap-s)
    (gl:tex-parameter :texture-2d :texture-wrap-t wrap-t)
    (gl:tex-parameter :texture-2d :texture-mag-filter mag-filter)
    (gl:tex-parameter :texture-2d :texture-min-filter min-filter)

    (when depth-texture-mode (gl:Tex-Parameter :TEXTURE-2D :DEPTH-TEXTURE-MODE depth-texture-mode))
    (when texture-compare-mode (gl:Tex-Parameter :TEXTURE-2D :TEXTURE-COMPARE-MODE texture-compare-mode))
    (when texture-compare-function (gl:Tex-Parameter :TEXTURE-2D :TEXTURE-COMPARE-func texture-compare-function))

    (if PBO 
	(pushg this pbo)
	(data-from-pointer this data))
    tex-id)))
		  

(defmethod data-from-pointer ((this texture) pointer)
  (bind this)
  (gl:tex-image-2d :texture-2d 0
		   (internal-format this)
		   (width this)
		   (height this)
		   0
		   (data-format this)
		   (cffi-type->gl-type (qtype this))
		   pointer))

    
(defmethod get-size ((this texture) &key)
  "Calculates the number of VALUES (stride * vcount) or (stride * width * height) this buffer contains."
  (* (stride this)
     (if (and (vertex-count this) (not (zerop (vertex-count this))))
	 (vertex-count this)
	 (* (width this) (height this)))))

(defmethod size-in-bytes ((this texture))
  "Calculates how many bytes this buffer consists of."
  (* 
   (get-size this)
   (cffi:foreign-type-size (slot-value this 'type))))

(defmethod bind ((this texture) &key )
  "Wrapper around glBindBuffer. Puts the texture into play."
  (gl:bind-texture :texture-2d (tex-id this)))

(defmethod unbind ((this texture) &key )
  (gl:bind-texture :texture-2d 0))

(defmethod pushg ((this texture) (pbo pixel-buffer) &key)
  (! 
    (bind pbo)
    (bind this)
    (gl:Tex-Image-2D :texture-2d 0 (internal-format this)  (width this) (height this) 0 :bgra (cffi-type->gl-type (qtype this)) (cffi:null-pointer))
    
    (unbind this)
    (unbind pbo)))

(defmethod make-pbo-for-texture ((this texture) &key (usage :static-draw) (target :pixel-unpack-buffer))
  (!
    (make-instance 'pixel-buffer
		   :count (vertex-count this)
		   :qtype (qtype this)
		   :stride (stride this)
		   :usage usage
		   :target target)))
  
(defmethod pullg ((tex texture) &key)
  (let ((arr (cffi:make-shareable-byte-vector (size-in-bytes tex))))
    (cffi:with-pointer-to-vector-data (p arr)
      (!
	(bind tex)
	(%gl:get-tex-image :texture-2d
			   0 
			 (data-format tex)
			 (clinch::cffi-type->gl-type  (qtype tex))
			 p))
      arr)))

(defmethod pushg ((tex texture) (data array) &key)
  (cffi:with-pointer-to-vector-data (p data)
    (! 
      (bind tex)
      (gl:tex-image-2d :texture-2d
		       0
		       (clinch::internal-format tex)
		       (width tex)
		       (height tex)
		       0 
		       (data-format tex)
		       (clinch::cffi-type->gl-type (qtype tex))
		       p)))
  data)


(defmethod bind-sampler ((this texture) shader-program name tex-unit)
  "Shader-Programs pass information by using named values called Uniforms. Textures are passed using Samplers. This sets a texture-unit to a sampler uniform" 
  (gl:active-texture (+ (cffi:foreign-enum-value '%gl:enum :texture0) tex-unit))
  (attach-uniform shader-program name tex-unit)
  (gl:bind-texture :texture-2d (tex-id this)))


(defmethod unload ((this texture) &key)
  "Unloads the texture. Also cancels gc finalization."
  (trivial-garbage:cancel-finalization this)
  (remhash (key this) *uncollected*)
  (sdl2:in-main-thread ()
    (gl:delete-textures (list (tex-id this)))))

;; This needs to check if there is a pbo, and if not create one...For now, I'll just have to use a pushg.
;; (defmacro with-mapped-texture ((name buffer &optional (access :READ-WRITE)) &body body)
;;   "Convenience macro for mapping and unmapping the texture data.
;; Name is the symbol name to use for the buffer pointer.
;; Just a passthrough to with-mapped-buffer, but I keep forgetting to use with-mapped-buffer."
;;   `(with-mapped-buffer (,name ,buffer ,access)
;;      ,body))
