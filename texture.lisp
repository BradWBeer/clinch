;;;; texture.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defparameter *default-texture* nil)
(defparameter *identity-texture* nil)

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
    (add-uncollected this)
    (trivial-garbage:finalize this 
			      (let ((tex-id-value tex-id)
				    (key (key this)))
				(lambda () (sdl2:in-main-thread (:background t)
					     (gl:delete-textures (list tex-id-value))))))
    
    (gl:bind-texture :texture-2d (tex-id this))
    (gl:tex-parameter :texture-2d :texture-wrap-s wrap-s)
    (gl:tex-parameter :texture-2d :texture-wrap-t wrap-t)
    (gl:tex-parameter :texture-2d :texture-mag-filter mag-filter)
    (gl:tex-parameter :texture-2d :texture-min-filter min-filter)

    (when depth-texture-mode (gl:Tex-Parameter :TEXTURE-2D :DEPTH-TEXTURE-MODE depth-texture-mode))
    (when texture-compare-mode (gl:Tex-Parameter :TEXTURE-2D :TEXTURE-COMPARE-MODE texture-compare-mode))
    (when texture-compare-function (gl:Tex-Parameter :TEXTURE-2D :TEXTURE-COMPARE-func texture-compare-function))

    (cond (pbo (pushg this pbo))
	  ((cffi:pointerp data) (data-from-pointer this data))
	  ((listp data) (data-from-list this data))
	  (t (data-from-pointer this data)))
			  
	   
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

(defmethod data-from-list ((this texture) lst)
  (data-from-pointer this nil)

  (let ((data (pullg this)))
    (loop for x from 0 below (length lst)
       for i in lst
       do (setf (elt data x) i))
    (pushg this data)))
    
(defmethod get-size ((this texture) &key)
  "Calculates the number of VALUES (stride * vcount) or (stride * width * height) this buffer contains."
  (* (stride this)
     (* (width this) (height this))))

(defmethod size-in-bytes ((this texture))
  "Calculates how many bytes this buffer consists of."
  (* 
   (get-size this)
   (cffi:foreign-type-size (slot-value this 'type))))

(defmethod bind ((this texture) &key )
  "Wrapper around glBindBuffer. Puts the texture into play."
  (gl:bind-texture :texture-2d (tex-id this)))

(defmethod unbind ((this texture) &key )
  "Unbinds the texture."
  (gl:bind-texture :texture-2d 0))

(defmethod pushg ((this texture) (pbo pixel-buffer) &key)
  "Sets the texture data from a pixel buffer."
  (! 
    (bind pbo)
    (bind this)
    (gl:Tex-Image-2D :texture-2d 0 (internal-format this)  (width this) (height this) 0 :bgra (cffi-type->gl-type (qtype this)) (cffi:null-pointer))
    
    (unbind this)
    (unbind pbo)))

(defmethod make-pbo-for-texture ((this texture) &key (usage :static-draw) (target :pixel-unpack-buffer))
  "Returns a compatible pixel buffer for a texture."
  (!
    (make-instance 'pixel-buffer
		   :count (get-size this)
		   :qtype (qtype this)
		   :stride (stride this)
		   :usage usage
		   :target target)))
  
(defmethod pullg ((tex texture) &key data-format)
  "Gets the texture data as a vector array."
  (let ((arr (cffi:make-shareable-byte-vector (size-in-bytes tex))))
    (cffi:with-pointer-to-vector-data (p arr)
      (!
	(bind tex)
	(%gl:get-tex-image :texture-2d
			   0 
			   (or data-format (data-format tex))
			 (clinch::cffi-type->gl-type  (qtype tex))
			 p))
      arr)))

(defmethod pushg ((tex texture) (data array) &key)
  "Sets the texture data from a vector array."
  (cffi:with-pointer-to-vector-data (p data)
    (! 
      (gl:bind-buffer :pixel-unpack-buffer 0)		      
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
  (remove-uncollected this)
  (!
    (when (tex-id this)
      (gl:delete-textures (list (tex-id this))))))

(defmacro with-texture ((tex) &body body)
  `(let ((*texture* ,tex))
     ,@body))
     

(defmacro with-temporary-pbo ((var texture &key (usage :static-draw) (target :pixel-unpack-buffer)) &body body)
  "Creates a temporary pixel buffer for a texture."
  (let ((tex (gensym)))
    `(let* ((,tex ,texture)
	    (,var (make-pbo-for-texture ,tex :usage ,usage :target ,target)))
       (unwind-protect 
	    (progn ,@body)
	 (progn
	   (unload ,var))))))

(defmethod set-texture-color ((this texture) r g b &optional (a 1.0))
  (let ((data (pullg this)))
    (dotimes (i (length data))
      (setf (aref data i) 
	    (case (mod i 4)
	      (0 (ceiling (* b 255)))
	      (1 (ceiling (* g 255)))
	      (2 (ceiling (* r 255)))
	      (3 (ceiling (* a 255))))))
    (pushg this data)
    this))

(defun get-identity-texture ()
  "Creates/returns a 1x1 texture with the values (1.0, 1.0, 1.0, 1.0).
   This is a nice placeholder when you don't want a custom shader."
  (if *identity-texture*
      *identity-texture* 
      (setf *identity-texture*
	    (make-instance 'clinch:texture
			   :data   '(255 255 255 255)
			   :width  1
			   :height 1
			   :stride 4
			   :qtype  :unsigned-char))))
