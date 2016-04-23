;;;; clinch-cairo.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:cairo)
(defvar *surface* nil)
(export '*surface*)
(in-package #:clinch)


(defun clear-cairo-context (&optional (r 1) (g 1) (b 1) (a 1) (context cairo:*context*))
  "Clearing a cairo context is tricky with transparency. This is a convenience function to clear the contex."
  (cairo:save context)
  (cairo:set-source-rgba r g b a context)
  (cairo:set-operator :source context)
  (cairo:paint context)
  (cairo:restore context))

;; working, but put need to group the calls which require the main thread for speed.
(defmacro with-surface-for-texture ((&optional texture pbo &key (rw :write-only)
					       (cairo-format :argb32)
					       (surface-var 'cairo::*surface*)
					       (width-var   (gensym))
					       (height-var  (gensym))
					       (bits-var    (gensym)))
				    &body body)
  "Takes a texture object, maps its data and creates a cairo:surface for it then destroys the surface when done.
       texture:      A texture object to operate on.
       rw:           Either :write-only (default), :read-only, or :read-write. Use the most restrictive for greater speed.
       cairo-format: The CAIRO format for the buffer. Defaults to :argb32.
       surface-var:  The local variable name for the created surface. Defaults to *surface*
       width-var:    The local variable name for the surface's width.
       height-var:   The local variable name for the surface's height.
       bits-var:     The local variable name for the texture's raw cffi data."
  
  (let ((m-texture (gensym))
	(m-pbo (gensym))
	(m-tmp-pbo? (gensym)))
    `(let* ((,m-texture (or ,texture *texture* (get-default-texture)))
	    (,m-tmp-pbo? nil)
	    (,m-pbo (or ,pbo
			(let ((tmp (make-pbo-for-texture ,m-texture)))
			  (when (not tmp)
			    (error "Could not map the PBO for cairo!"))
			  (setf ,m-tmp-pbo? t)
			  tmp)))
	    (,bits-var (map-buffer ,m-pbo ,rw))
	    (,width-var  (clinch:width  ,m-texture))
	    (,height-var (clinch:height ,m-texture))
	    (,surface-var (cairo:create-image-surface-for-data ,bits-var
							       ,cairo-format
							       ,width-var
							       ,height-var
							       (* ,width-var (stride ,m-texture)))))

       (if (or (not ,bits-var)
	       (cffi:null-pointer-p ,bits-var))
	   (error "Could not map the PBO for cairo!")
	   (unwind-protect 
		(progn ,@body)
	     (progn
	       (cairo:destroy ,surface-var)
	       (unmap-buffer ,m-pbo)
	       (pushg ,m-texture ,m-pbo)
	       (when ,m-tmp-pbo? (unload ,m-pbo))))))))


(defmacro draw ((&key texture
		      (surface-var 'cairo::*surface*)
		      (width-var   (gensym))
		      (height-var  (gensym))
		      (bits-var    (gensym))
		      (cairo-format :argb32))
		&body body)
  "Takes a texture object, maps its data and creates a cairo:surface for it then destroys the surface when done.
       texture:      A texture object to operate on.
       surface-var:  The local variable name for the created surface. Defaults to *surface*
       width-var:    The local variable name for the surface's width.
       height-var:   The local variable name for the surface's height.
       bits-var:     The local variable name for the texture's raw cffi data."

  (let ((m-texture (gensym))
	(m-arr (gensym)))
    
    `(let* ((,m-texture (or ,texture *texture* (get-default-texture)))
	    (,m-arr (pullg ,m-texture))
	    (,width-var  (clinch:width  ,m-texture))
	    (,height-var (clinch:height ,m-texture)))
       
       (cffi:with-pointer-to-vector-data (,bits-var ,m-arr)
	 (let ((,surface-var (cairo:create-image-surface-for-data
			      ,bits-var
			      ,cairo-format
			      ,width-var
			      ,height-var
			      (* ,width-var (stride ,m-texture)))))
	   (cairo:with-context-from-surface (,surface-var)
	     ,@body)))
       (pushg ,m-texture ,m-arr)
       ,m-arr)))


(defmacro with-context-for-texture ((&optional texture pbo &key (rw :write-only)
					       (cairo-format :argb32)
					       (surface-var 'cairo::*surface*)
					       (context-var 'cairo::*context*)
					       (width-var (gensym))
					       (height-var (gensym)))
				    &body body)
  "Takes a texture object, maps its data and creates a cairo:surface AND a context for it then destroys the surface and context it when done.
       texture:      A texture object to operate on.
       rw:           Either :write-only (default), :read-only, or :read-write. Use the most restrictive for greater speed.
       cairo-format: The CAIRO format for the buffer. Defaults to :argb32.
       surface-var:  The local variable name for the created surface. Defaults to *surface*
       width-var:    The local variable name for the surface's width.
       height-var:   The local variable name for the surface's height.
       bits-var:     The local variable name for the texture's raw cffi data."
  `(with-surface-for-texture (,texture ,pbo
				       :rw ,rw
				       :cairo-format ,cairo-format
				       :surface-var  ,surface-var
				       :width-var    ,width-var
				       :height-var   ,height-var)
     (let ((,context-var (cairo:create-context ,surface-var)))
       (unwind-protect
	    (progn ,@body))
       (cairo:destroy ,context-var))))

(defmacro fast-draw ((&optional texture pbo &key (rw :write-only)
				(format :argb32)
				(surface-var 'cairo::*surface*)
				(context-var 'cairo::*context*)
				(width-var (gensym))
				(height-var (gensym)))
		     &body body)
  `(with-context-for-texture (,texture ,pbo 
				       :rw ,rw
				       :cairo-format ,format
				       :surface-var ,surface-var
				       :context-var ,context-var
				       :width-var ,width-var
				       :height-var ,height-var)
     ,@body))

(defmethod make-texture-from-png (path)
  "Load the png image into a texture buffer."
  (cairo:with-png-surface (path surf)
    (let ((bits (cairo:image-surface-get-data surf :pointer-only t))
	  (w (cairo:image-surface-get-width surf))
	  (h (cairo:image-surface-get-height surf)))
      
      (make-instance 'clinch:texture :width w :height h :stride 4 :data bits :qtype :unsigned-char))))

(defgeneric make-quad-for-png (tex-data &key center parent))
(defmethod make-quad-for-png ((path string) &key width height (center :center) parent)
  "Creates a quad entity for a png file."
  (let ((texture (make-texture-from-png path)))
    (make-quad (or width (width texture))
	       (or height (height texture))
	       :center center
	       :texture texture
	       :parent parent)))
