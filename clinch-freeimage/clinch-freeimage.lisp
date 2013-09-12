(in-package #:clinch)


(defun create-texture-from-file (path &key width height)
  (freeimage:with-loaded-32bit-map (path
   				    :width     width
   				    :height    height
   				    :bitvar    bits
   				    :widthvar  w
   				    :heightvar h)
    (make-instance 'clinch:texture
		   :bits   bits
		   :width  w
		   :height h
		   :stride 4
		   :count  (* w h)
		   :qtype  :unsigned-char
		   :target :pixel-unpack-buffer)))




(defmethod load-texture-from-file ((this texture) path &key)
  (freeimage:with-loaded-32bit-map (path
   				    :width     (width this)
   				    :height    (height this)
   				    :bitvar    src
   				    :widthvar  w
   				    :heightvar h)
    
    (bind this)
    (%gl:Buffer-Data (target this)
		     (* (get-size this) (cffi:foreign-type-size (qtype this)))
		     src
		     (usage this))
    (setf (loaded? this) t)))


(defun load-animation (path)
  (let* ((atype (freeimage_getfiletype path 0))
	 (multi-bitmap (cffi:with-foreign-objects ((bool :int))
			 (setf (cffi:mem-aref bool :int) 0)
			 (freeimage_openmultibitmap atype path false true false bool)))
	 (page-count (FreeImage_GetPageCount multi-bitmap)))
    (loop for i from 0 to (1- page-count)
       collect (let* ((page (freeimage_lockpage multi-bitmap i))
		      (time (* 10
			       (cffi:with-foreign-object (data :pointer)
				 (freeimage_getmetadata :FIMD-ANIMATION page "FrameTime" data)
				 (cffi:mem-aref (FreeImage_GetTagValue (cffi:mem-aref data :pointer)) :int32))))
		      (bitmap (freeimage_convertto32bits page)))
		 (FreeImage_FlipVertical bitmap)

		 (list (load-2d-image nil
				      (freeimage_getbits bitmap)
				      (freeimage_getwidth bitmap)
				      (freeimage_getheight bitmap))
		       time)))))




