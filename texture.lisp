;;;; texture.lisp

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
    :initform :unsigned-byte)
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
    :initarg :target)))



(defmethod initialize-instance :after ((this texture) &key
				       (format :bgra)
				       (wrap-s :repeat)
				       (wrap-t :repeat)
				       (mag-filter :nearest)
				       (min-filter :nearest))
  (with-slots ((tex-id tex-id)
	       (w width)
	       (h height)
	       (this-target target)
	       (dtype type)) this
    
    (unless tex-id (setf tex-id (car (gl:gen-textures 1))))
    
    (gl:bind-texture :texture-2d (tex-id this))
    (gl:tex-parameter :texture-2d :texture-wrap-s wrap-s)
    (gl:tex-parameter :texture-2d :texture-wrap-t wrap-t)
    (gl:tex-parameter :texture-2d :texture-mag-filter mag-filter)
    (gl:tex-parameter :texture-2d :texture-min-filter min-filter)
    
    (when (loaded? this)
      (gl:tex-image-2d :texture-2d 0 :rgba w h 0 format
		       (cffi-type->gl-type dtype)
		       (cffi:null-pointer))
      tex-id)))
    
    

(defmethod bind ((this texture) &key )
  (gl:bind-buffer (target this) (id this))
  (gl:bind-texture :texture-2d (tex-id this)))

(defmethod map-buffer ((this texture) &optional (access :READ-WRITE))
  (bind this)
  (gl:map-buffer (target this) access))

(defmethod unmap-buffer ((this texture))
  (gl:unmap-buffer (target this))
  (gl:bind-texture  :texture-2d (tex-id this))
  (gl:Tex-Image-2D :texture-2d 0 :rgba  (width this) (height this) 0 :bgra (cffi-type->gl-type (qtype this)) (cffi:null-pointer))
  
  (gl:bind-texture :texture-2d 0)
  (gl:bind-buffer (target this) 0))
	  

  

(defmethod bind-sampler ((this texture) shader name tex-unit)
  (gl:active-texture (+ (cffi:foreign-enum-value '%gl:enum :texture0) tex-unit))
  (attach-uniform shader name tex-unit)
  (gl:bind-texture :texture-2d (tex-id this)))


(defmacro with-loaded-32bit-map ((path &key width height bitvar widthvar heightvar) &body body)
  (let ((dib (gensym))
	(orig-w (if widthvar widthvar (gensym)))
	(orig-h (if heightvar heightvar (gensym)))
	(bits (if bitvar bitvar 'bits))
	(user-w (gensym))
	(user-h (gensym))
	(new-dib (gensym)))
    `(let* ((,user-w ,width)
	    (,user-h ,height)
	    (,dib (freeimage::get-32bit-dib ,path))
	    (,orig-w (freeimage:freeimage-getwidth ,dib))
	    (,orig-h (freeimage:freeimage-getheight ,dib))
	    (,bits))

       (if (or ,user-w ,user-h)
	   (let* ((new-dib (freeimage:freeimage-rescale ,dib ,user-w ,user-h)))
	     (freeimage:unload-dib ,dib)
	     (setf ,dib ,new-dib)
	     (setf ,orig-w ,user-w)
	     (setf ,orig-h ,user-h)))
       
       (setf ,bits (freeimage::freeimage-getbits ,dib))
       (freeimage:freeimage-flipvertical ,dib)
       ,@body

       (freeimage:unload-dib ,dib))))



(defmethod unload :after ((this texture) &key)
  (gl:delete-textures (list (tex-id this))))