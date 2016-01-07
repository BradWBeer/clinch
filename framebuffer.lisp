;;;; framebuffer.lisp
;;;; Please see the licence.txt for the CLinch

(in-package #:clinch)

(defconstant color-attachment-0 (cffi:foreign-enum-value 'cl-opengl-bindings:enum
							 :color-attachment0))


(defclass frame-buffer ()
  ((id :reader id
       :initform nil
       :initarg :id)
   (target  :accessor target
	    :initform :draw-framebuffer 
	    :initarg  :target)
   (color   :reader color-attachments   ;; this is a list!
	    :initform nil)
   (depth   :reader depth-buffer
	    :initform nil
	    :initarg :depth-buffer)
   (stencil :reader stencil-attachments
	    :initform nil)))



(defmethod initialize-instance :after ((this frame-buffer) &key
				       color-attachments
				       depth-attachment
				       stencil-attachment)
  (sdl2:in-main-thread ()
  (with-slots ((id id)
	       (color color)
	       (depth depth)
	       (stencil stencil)) this

    (unless id
      (setf id (car (gl:gen-framebuffers 1))))

    (trivial-garbage:cancel-finalization this)
    (trivial-garbage:finalize this 
			      (let ((id-value (id)))
				(lambda () (sdl2:in-main-thread () 
								(gl:delete-framebuffers (list id-value))))))					
    
    (when color-attachments
      (if (listp color-attachments)
	  (setf color color-attachments)
	  (setf color (list color-attachments))))

    (when depth-attachment
      (setf (depth-buffer this) depth-attachment))

    (when stencil-attachment
      (setf stencil stencil-attachment)))

  (update this)
  (unbind this)))

(defmethod (setf depth-buffer) ((db texture) (this frame-buffer))

  (sdl2:in-main-thread ()
  (setf (slot-value this 'depth) db)
  (bind this)
  (bind db)
  
  (gl:framebuffer-texture-2d :DRAW-FRAMEBUFFER :depth-attachment :texture-2d (clinch::tex-id db) 0)
  (unbind db)
  (unbind this)))

(defmethod add-color-buffer ((this frame-buffer) (tex texture) &optional (index 'number ))

  (sdl2:in-main-thread ()
  (bind this)
  (bind tex)
  (let ((attachment (+ color-attachment-0 (or index 0))))
    (gl:framebuffer-texture-2d :DRAW-FRAMEBUFFER attachment :texture-2d (tex-id tex) 0))
  (unbind tex)
  (unbind this))

;; Do I need this?
;; (defmethod update ((this frame-buffer) &key)

;;   (with-slots ((color color)
;; 	       (depth depth)
;; 	       (stencil stencil)) this
    
  
;;   (bind this)
  
;;   (loop
;;      for x from 0
;;      for attachment in color
;;        do (progn
;; 	    (typecase attachment

;; 	      (texture (gl:framebuffer-texture-2d (target this)
;; 							  (+ x (cffi:FOREIGN-ENUM-VALUE  'cl-opengl-bindings::enum :color-attachment0))
;; 							  :TEXTURE-2D
;; 							  (tex-id attachment)
;; 							  0))
;; 	      (render-buffer (gl:framebuffer-renderbuffer (target this)
;; 							  (+ x (cffi:FOREIGN-ENUM-VALUE  'cl-opengl-bindings::enum :color-attachment0))
;; 							  :render-buffer
;; 							  (renderbuffer-id attachment))))))
	    
;;   (when depth
;;     (typecase depth
;;       (texture (gl:framebuffer-texture-2d (target this)
;; 					  :depth-attachment
;; 					  :texture-2d
;; 					  (tex-id depth)
;; 					  0))
;;       (render-buffer (gl:framebuffer-renderbuffer (target this)
;; 						  :depth-attachment
;; 						  :render-buffer
;; 						  (renderbuffer-id depth)))))

;;   (when stencil
;;     (typecase depth
;;       (texture (gl:framebuffer-texture-2d (target this)
;; 					  :stencil-attachment
;; 					  :texture-2d
;; 					  (tex-id stencil)
;; 					  0))
;;       (render-buffer (gl:framebuffer-renderbuffer (target this)
;; 						  :stencil-attachment
;; 						  :render-buffer
;; 						  (renderbuffer-id stencil)))))))


(defmethod bind ((this frame-buffer) &key )
  "Wrapper around glBindFrameBuffer. Puts the Framebuffer into play."
  (gl:bind-framebuffer (target this) (id this)))

(defmethod unbind ((this frame-buffer) &key )
  "Wrapper around glBindFrameBuffer. Puts the Framebuffer into play."
  (gl:bind-framebuffer (target this) 0))

(defmethod unload ((this frame-buffer) &key)
  "Unloads and releases all frame-buffer resources, also any renderbuffers"

  (with-slots ((id id)
	       (color color)
	       (depth depth)
	       (stencil stencil)) this

    (trivial-garbage:cancel-finalization this)
    (sdl2:in-main-thread ()
    (gl:Delete-Framebuffers (list id))
   
    (setf id      nil
	  color   nil
	  depth   nil
	  stencil nil))))
	
				       

