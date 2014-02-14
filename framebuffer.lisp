;;;; framebuffer.lisp
;;;; Please see the licence.txt for the CLinch

(in-package #:clinch)


(defclass frame-buffer ()
  ((id :reader id
       :initform nil
       :initarg :id)
   (target  :accessor target
	    :initform :framebuffer
	    :initarg  :target)
   (color   :reader color-attachments   ;; this is a list!
	    :initform nil)
   (depth   :reader depth-attachments
	    :initform nil)
   (stencil :reader stencil-attachments
	    :initform nil)))



(defmethod initialize-instance :after ((this frame-buffer) &key
				       color-attachments
				       depth-attachment
				       stencil-attachment)

  (with-slots ((id id)
	       (color color)
	       (depth depth)
	       (stencil stencil)) this

    (unless id
      (setf id (car (gl:gen-framebuffers 1))))

    (when color-attachments
      (if (listp color-attachments)
	  (setf color color-attachments)
	  (setf color (list color-attachments))))

    (when depth-attachment
      (setf depth depth-attachment))

    (when stencil-attachment
      (setf stencil stencil-attachment)))

  (update this))

    

(defmethod update ((this frame-buffer) &key)

  (with-slots ((color color)
	       (depth depth)
	       (stencil stencil)) this
    
  
  (bind this)
  
  (loop
     for x from 0
     for attachment in color
       do (progn
	    (typecase attachment

	      (texture (gl:framebuffer-texture-2d (target this)
							  (+ x (cffi:FOREIGN-ENUM-VALUE  'cl-opengl-bindings::enum :color-attachment0))
							  :TEXTURE-2D
							  (tex-id attachment)
							  0))
	      (render-buffer (gl:framebuffer-renderbuffer (target this)
							  (+ x (cffi:FOREIGN-ENUM-VALUE  'cl-opengl-bindings::enum :color-attachment0))
							  :render-buffer
							  (renderbuffer-id attachment))))))
	    
  (when depth
    (typecase depth
      (texture (gl:framebuffer-texture-2d (target this)
					  :depth-attachment
					  :texture-2d
					  (tex-id depth)
					  0))
      (render-buffer (gl:framebuffer-renderbuffer (target this)
						  :depth-attachment
						  :render-buffer
						  (renderbuffer-id depth)))))

  (when stencil
    (typecase depth
      (texture (gl:framebuffer-texture-2d (target this)
					  :stencil-attachment
					  :texture-2d
					  (tex-id stencil)
					  0))
      (render-buffer (gl:framebuffer-renderbuffer (target this)
						  :stencil-attachment
						  :render-buffer
						  (renderbuffer-id stencil)))))))


(defmethod bind ((this frame-buffer) &key )
  "Wrapper around glBindFrameBuffer. Puts the Framebuffer into play."
  (gl:bind-framebuffer (target this) (id this)))

(defmethod unbind ((this frame-buffer) &key )
  "Wrapper around glBindFrameBuffer. Puts the Framebuffer into play."
  (gl:bind-framebuffer (target this) 0))

(defmethod unload ((this render-buffer) &key)
  )

(defmethod unload ((this shader) &key)
  "Unloads and releases all frame-buffer resources, also any renderbuffers"
  (with-slots ((id id)
	       (color color)
	       (depth depth)
	       (stencil stencil)) this

    (gl:Delete-Framebuffers (list id))
   
    (map nil
	 (lambda (x) (typecase x (render-buffer (unload x))))
	 color)

    (when depth (typecase depth (render-buffer (unload depth))))
    (when stencil (typecase depth (render-buffer (unload stencil))))

    (setf id      nil
	  color   nil
	  depth   nil
	  stencil nil)))
	
				       

