(in-package #:clinch)

(defun create-texture-from-file (path &key width height)
  (freeimage:with-loaded-32bit-map (path
   				    :width     width
   				    :height    height
   				    :bitvar    bits
   				    :widthvar  w
   				    :heightvar h)
    (make-instance 'clinch:texture
		   :data   bits
		   :width  w
		   :height h
		   :stride 4
		   :count  (* w h)
		   :qtype  :unsigned-char)))

(defmethod create-quad-for-image ((path string) &key width height (center :center))
  (let ((texture (create-texture-from-file path :width width :height height)))
    (make-quad (width texture)
	       (height texture)
	       :center center
	       :texture texture)))

(defmethod create-quad-for-image ((texture texture) &key width height (center :center))
  (make-quad (width texture)
	     (height texture)
	     :center center
	     :texture texture))


(defmethod load-texture-from-file ((this texture) path &key resize)
  
  (freeimage:with-loaded-32bit-map (path
   				    :width     (unless resize (width this))
   				    :height    (unless resize (height this))
   				    :bitvar    src
   				    :widthvar  w
   				    :heightvar h)
    (!
      (when resize
       (setf (slot-value this 'width) w
	     (slot-value this 'height) h))
      (data-from-pointer this src))))


;; (defmethod load-pbo-from-file ((this pixel-buffer) path &key)
;;   (freeimage:with-loaded-32bit-map (path
;;    				    :width     (width this)
;;    				    :height    (height this)
;;    				    :bitvar    src
;;    				    :widthvar  w
;;    				    :heightvar h)

;;     (data-from-pointer this src)))


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




