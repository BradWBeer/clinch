;;;; clinch-cairo.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:cairo)
(defvar *surface* nil)
(export '*surface*)
(in-package #:clinch)


(defun clear-cairo-context (&optional (r 1) (g 1) (b 1) (a 1) (context cairo:*context*))
  (cairo:save context)
  (cairo:set-source-rgba r g b a context)
  (cairo:set-operator :source context)
  (cairo:paint context)
  (cairo:restore context))


(defmacro with-surface-for-texture ((texture &key (rw :write-only)
					     (cairo-format :argb32)
					     (surface-var 'cairo:*surface*)
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

  `(clinch:with-mapped-buffer (,bits-var ,texture ,rw)
     (let* ((,width-var  (clinch:width  ,texture))
	    (,height-var (clinch:height ,texture))
	    (,surface-var (cairo:create-image-surface-for-data ,bits-var
							       ,cairo-format
							       ,width-var
							       ,height-var
							       (* ,width-var (stride ,texture)))))
       (unwind-protect 
	    (progn ,@body)
	 (cairo:destroy ,surface-var)))))


(defmacro with-context-for-texture ((texture &key (rw :write-only)
					     (cairo-format :argb32)
					     (surface-var 'cairo:*surface*)
					     (context-var 'cairo:*context*)
					     (width-var)
					     (height-var))
				    &body body)
  "Takes a texture object, maps its data and creates a cairo:surface AND a context for it then destroys the surface and context it when done.
       texture:      A texture object to operate on.
       rw:           Either :write-only (default), :read-only, or :read-write. Use the most restrictive for greater speed.
       cairo-format: The CAIRO format for the buffer. Defaults to :argb32.
       surface-var:  The local variable name for the created surface. Defaults to *surface*
       width-var:    The local variable name for the surface's width.
       height-var:   The local variable name for the surface's height.
       bits-var:     The local variable name for the texture's raw cffi data."
  `(with-surface-for-texture (,texture
			      :rw ,rw
			      :cairo-format ,cairo-format
			      :surface-var  ,surface-var
			      :width-var    ,width-var
			      :height-var   ,height-var)
     (let ((,context-var (cairo:create-context ,surface-var)))
	 (unwind-protect
	      (progn ,@body))
	 (cairo:destroy ,context-var))))

