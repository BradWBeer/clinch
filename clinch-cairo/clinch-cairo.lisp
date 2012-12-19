;;;; clinch-cairo.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)

(defmacro with-context-for-texture ((texture &key (rw :write-only)
					     (cairo-format :argb32)
					     (surface-var)
					     (width-var)
					     (height-var))
				    &body body)
  (let ((bits    (gensym))
	(surface (or surface-var (gensym)))
	(w       (or width-var  (gensym)))
	(h       (or height-var (gensym))))

    `(let ((,w (width ,texture))
	   (,h (height ,texture)))
       (clinch:with-mapped-buffer (,bits ,texture ,rw)
	 (let ((,surface (cairo:create-image-surface-for-data ,bits ,cairo-format ,w ,h (* ,w (stride ,texture)))))
	   (unwind-protect 
		(cairo:with-context ((cairo:create-context ,surface))
		  (progn ,@body))		     
	     (cairo:destroy ,surface)))))))
