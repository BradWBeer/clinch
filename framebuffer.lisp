;;;; framebuffer.lisp
;;;; Please see the licence.txt for the CLinch

(in-package #:clinch)

(defconstant color-attachment-0 (cffi:foreign-enum-value 'cl-opengl-bindings:enum
							 :color-attachment0))
(defparameter *fbo* nil)

(defclass frame-buffer ()
  ((id :reader id
       :initform nil
       :initarg :id)
   (target  :accessor target
	    :initform :draw-framebuffer 
	    :initarg  :target)
   (color   :reader color-attachments
	    :initform nil)
   (depth   :reader depth-buffer
	    :initform nil
	    :initarg :depth-buffer)
   (stencil :reader stencil-attachments
	    :initform nil)
   (key :initform (gensym "framebuffer")
	:reader key))
  (:documentation "The Frame Buffer Object or FBO. Use this to render to textures."))


(defmethod initialize-instance :after ((this frame-buffer) &key
							     color-attachments
							     depth-attachment
							     stencil-attachment)
  "Creates an FBO with optional color attachments, depth-attachements and stencil attachments." ;; Check if color attachements is optional. !!!
  (!
    (with-slots ((id id)
		 (color color)
		 (depth depth)
		 (stencil stencil)) this

      (unless id
	(setf id (car (gl:gen-framebuffers 1))))


      (trivial-garbage:cancel-finalization this)
      (setf (gethash (key this) *uncollected*) this)
      
      (trivial-garbage:finalize this
				(let ((id-value id)
				      (key (key this)))

				  (lambda ()
				    (remhash key *uncollected*)
				    (sdl2:in-main-thread (:background t) 
				      (gl:delete-framebuffers (list id-value))))))
      (when color-attachments
	(if (listp color-attachments)
	    (setf color color-attachments)
	    (setf color (list color-attachments))))

      (when depth-attachment
	(setf (depth-buffer this) depth-attachment))

      (when stencil-attachment
	(setf stencil stencil-attachment)))

    (unbind this)))

(defmethod make-depth-texture ((this frame-buffer) width height &key
								 (internal-format :depth-component32)
								 (format :depth-component)
								 (qtype  :float)
								 (stride 1)
								 (depth-texture-mode :intensity)
								 (texture-compare-mode :compare-r-to-texture)
								 (texture-compare-function :lequal))
  (setf (depth-buffer this)
	(make-instance 'clinch:texture 
		       :width width 
		       :height height
		       :internal-format internal-format
		       :format format  
		       :qtype qtype
		       :stride stride
		       :depth-texture-mode depth-texture-mode
		       :texture-compare-mode texture-compare-mode
		       :texture-compare-function texture-compare-function)))  
								

(defmethod (setf depth-buffer) ((db texture) (this frame-buffer))
  "Set the depth buffer to use."
  (!
    (setf (slot-value this 'depth) db)
    (bind this)
    (bind db)
    
    (gl:framebuffer-texture-2d :DRAW-FRAMEBUFFER :depth-attachment :texture-2d (clinch::tex-id db) 0)
    (unbind db)
    (unbind this)))

(defmethod color-attachment ((this frame-buffer) name)
  "Returns a color-attachment by number."
  (cdr (assoc name (slot-value this 'color) :test #'equal)))

(defmethod (setf color-attachment) (new-value (this frame-buffer) name)
  "Sets an attachment's number"
  (with-slots ((colors color)) this

    (if (null new-value)
	(setf colors (remove-if (lambda (x) (equal (car x) name)) colors))
	(let ((item (assoc name colors :test #'equal)))
	  (if item
	      (setf (cdr item) new-value)
	      (setf colors (acons name new-value colors))))))
  new-value)


(defmethod make-color-texture ((this frame-buffer) index width height &key
								  (PBO nil)
								  (stride 4)
								  (qtype :unsigned-char)
								  (internal-format :rgba)
								  (format :bgra)
								  (wrap-s :repeat)
								  (wrap-t :repeat)
								  (mag-filter :linear)
								  (min-filter :linear)
								  texture-compare-mode
								  texture-compare-function)
  (!
    (let ((tex (make-instance 'clinch:texture
			    :PBO PBO
			    :width width
			    :height height
			    :stride stride
			    :qtype qtype
			    :internal-format internal-format
			    :format format
			    :wrap-s wrap-s
			    :wrap-t wrap-t
			    :mag-filter mag-filter
			    :min-filter min-filter
			    :texture-compare-mode texture-compare-mode
			    :texture-compare-function texture-compare-function)))
    
    (add-color-buffer this index tex)
    tex)))

(defmethod add-color-buffer ((this frame-buffer) index (tex texture))
  "Add a color buffer at position index."
  (!
    (bind this)
    (bind tex)
    (let ((attachment (+ color-attachment-0 (or index 0))))
      (gl:framebuffer-texture-2d :DRAW-FRAMEBUFFER attachment :texture-2d (tex-id tex) 0))
    (unbind tex)
    (setf (color-attachment this index) tex)
    this))

(defmethod bind ((this frame-buffer) &key )
  "Wrapper around glBindFrameBuffer. Puts the Framebuffer into play."
  (! (gl:bind-framebuffer (target this) (id this))
     (gl:draw-buffers
      (loop for (num . tex) in (color-attachments this)
	 collect (print (+ color-attachment-0 num))))))

(defmethod bind ((this null) &key)
  (! (gl:bind-framebuffer :draw-framebuffer 0)))

(defmethod unbind ((this frame-buffer) &key )
  "Wrapper around glBindFrameBuffer. Puts the Framebuffer into play."
  (! (gl:bind-framebuffer (target this) 0)))

(defmethod unload ((this frame-buffer) &key)
  "Unloads and releases all frame-buffer resources, also any renderbuffers"
  (with-slots ((id id)
	       (color color)
	       (depth depth)
	       (stencil stencil)) this

    (trivial-garbage:cancel-finalization this)
    (remhash (key this) *uncollected*)
    (sdl2:in-main-thread ()
      (gl:Delete-Framebuffers (list id))
      
      (setf id      nil
	    color   nil
	    depth   nil
	    stencil nil))))

(defmacro with-fbo ((fbo) &body body)
  "Convenience macro for useing and resetting an FBO."
  (let ((old-fbo (gensym))
	 (new-fbo (gensym)))
    `(!
       (let* ((,old-fbo *FBO*)
	      (,new-fbo ,fbo)
	      (*FBO* ,new-fbo))
	 (bind *FBO*)
	 (unwind-protect
	      (progn ,@body)
	   (bind ,old-fbo))))))
