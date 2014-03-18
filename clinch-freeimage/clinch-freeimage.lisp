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
  (let* ((atype (freeimage::freeimage-getfiletype path 0))
	 (multi-bitmap (freeimage::freeimage-openmultibitmap (cffi:foreign-enum-value 'freeimage::free-image-format atype) path 0 1 0 0))
	 (page-count (Freeimage::Freeimage-GetPageCount multi-bitmap)))
    (loop with total-time = 0
       for i from 0 to (1- page-count)
       collect (let* ((page (freeimage::freeimage-lockpage multi-bitmap i))
		      (time (cffi:with-foreign-object (data :pointer)
			      (freeimage::freeimage-getmetadata (cffi:foreign-enum-value 'freeimage::FREE-IMAGE-MDMODEL :FIMD-ANIMATION)  page "FrameTime" data)
			      (cffi:mem-aref (Freeimage::Freeimage-GetTagValue (cffi:mem-aref data :pointer)) :int32)))
		      (bitmap (freeimage::freeimage-convertto32bits page)))
		 (Freeimage::Freeimage-FlipVertical bitmap)

		 (list (make-instance 'texture
				      :data (freeimage::freeimage-getbits bitmap)
				      :width (freeimage::freeimage-getwidth bitmap)
				      :height (freeimage::freeimage-getheight bitmap))
		       (incf total-time (/ time 1000)))))))




