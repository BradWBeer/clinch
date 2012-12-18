(in-package #:clinch)


(defmacro with-loaded-32bit-map ((path &key width height bitvar widthvar heightvar) &body body)
  "Loads a bitmap into memory using FreeImage.
     width: If you want a specific width, set this.
     height: If you want a specific height, set this.
     bitvar: The variable name for the mapped data pointer.
     widthvar: The variable name of the width (if width is set then equal to width)
     heightvar: The variable name of the height (if height is set then equal to height)

     body:    Your code."
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


