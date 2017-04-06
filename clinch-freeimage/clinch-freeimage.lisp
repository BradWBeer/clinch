(in-package #:clinch)

(defun make-texture-from-file (path &key width height)
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
		   :qtype  :unsigned-char)))

(defgeneric make-quad-for-image (tex-data &key width height center parent))

(defmethod make-quad-for-image ((path pathname) &key width height (center :center) (parent *node*))
  (make-quad-for-image (namestring path) :width width  :height height :center center :parent parent))

(defmethod make-quad-for-image ((path string) &key width height (center :center) (parent *node*))
  (let ((texture (make-texture-from-file path :width width :height height)))
    (values (make-quad (width texture)
		       (height texture)
		       :center center
		       :texture texture
		       :parent parent)
	    texture)))

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

(defmacro mix-buffer-colors (src dest)
  (let ((src-name (gensym))
	(dst-name (gensym))
	(alp-name (gensym)))
    `(let* ((,dst-name ,dest)
	    (,src-name ,src)
	    (,alp-name (cffi:mem-aref ,src-name :uint8 3)))
       (dotimes (i 4)
	 (incf (cffi:mem-aref ,dst-name :uint8 i) 
	       (* (cffi:mem-aref ,src-name :uint8 i) 
		  ,alp-name))))))


;; (defun load-fast-animation (path)
;;   ;; load the multi-bitmap
;;   (let* ((atype (freeimage::freeimage-getfiletype path 0))
;; 	 (multi-bitmap (freeimage::freeimage-openmultibitmap (cffi:foreign-enum-value 'freeimage::free-image-format atype) path 0 1 0 0))
;; 	 (page-count (Freeimage::Freeimage-GetPageCount multi-bitmap))
;; 	 (total-time 0))
;;     ;; get the first page. It will set the size of the texture.
;;     (let* ((page (freeimage::freeimage-lockpage multi-bitmap i))
;; 	   (width (freeimage::freeimage-getwidth page))
;; 	   (height (freeimage::freeimage-getheight page))
;; 	   (bitmap (freeimage::freeimage-convertto32bits page))
;; 	   (time (cffi:with-foreign-object (data :pointer)
;; 		   (freeimage::freeimage-getmetadata 
;; 		    (cffi:foreign-enum-value 'freeimage::FREE-IMAGE-MDMODEL :FIMD-ANIMATION)  page "FrameTime" data)
;; 		   (cffi:mem-aref (Freeimage::Freeimage-GetTagValue (cffi:mem-aref data :pointer)) :int32))))
;;       (Freeimage::Freeimage-FlipVertical bitmap)
;;       (let ((PBO)
;; 	    (texture-1))
;; 	(! (setf texture-1 (make-instance 'texture
;; 					  :width (freeimage::freeimage-getwidth bitmap)
;; 					  :height (freeimage::freeimage-getheight bitmap))

;; 		 PBO (make-pbo-for-texture texture-1)))

(defclass image-animation (animation) ())
(defclass image-animator (animator) ())

;; notes!!!: Change the output to ((time . texture) ... with time in ms.
(defun load-animation-as-vector (path)
  (let* ((atype (freeimage::freeimage-getfiletype path 0))
	 (multi-bitmap (freeimage::freeimage-openmultibitmap (cffi:foreign-enum-value 'freeimage::free-image-format atype) path 0 1 0 freeimage:GIF-PLAYBACK))
	 (page-count (Freeimage::Freeimage-GetPageCount multi-bitmap)))
    (coerce
     (loop with total-time = 0
	for i from 0 to (1- page-count)
	collect (let* ((page (freeimage::freeimage-lockpage multi-bitmap i))
		       (time (cffi:with-foreign-object (data :pointer)
			       (freeimage::freeimage-getmetadata (cffi:foreign-enum-value 'freeimage::FREE-IMAGE-MDMODEL :FIMD-ANIMATION)  page "FrameTime" data)
			       (cffi:mem-aref (Freeimage::Freeimage-GetTagValue (cffi:mem-aref data :pointer)) :int32)))
		       (bitmap (freeimage::freeimage-convertto32bits page)))
		  (Freeimage::Freeimage-FlipVertical bitmap)
		  
		  (cons (incf total-time time)
			(! (make-instance 'texture
					  :data (freeimage::freeimage-getbits bitmap)
					  :width (freeimage::freeimage-getwidth bitmap)
					  :height (freeimage::freeimage-getheight bitmap))))))
     'vector)))


(defun load-animation (path)
  (make-instance 'animation
		 :frames (load-animation-as-vector path)))

;; !!!! This is a temporary. I will change this to create an animation and then use an animator.  

(defun make-animation-and-quad (path &key (parent clinch:*node*) width height (center :center) shader-program)
  (let* ((a (load-animation path))
	 (o (make-instance 'animator :animation a))
    	 (q (make-quad-for-texture (cdr (aref (frames a) 0))
				   :parent parent
				   :width width
				   :height height
				   :center center
				   :shader-program shader-program)))
	

    (setf (uniform q "t1") o)
    (values q
	    o
	    a)))

;; (defun load-height-map (path)
;;   (freeimage:with-loaded-24bit-map (path
;; 				    :bitvar bits 
;; 				    :widthvar w 
;; 				    :heightvar h)
;;     (values
     
;;      (loop for x from 0 below (* w h 3) by 3
;; 	append (multiple-value-bind (a b)
;; 		   (floor (/ x 3) w)
;; 		 (list (float b)
;; 		       (/ (cffi:mem-aref bits :unsigned-char x) 
;; 			  255.0)
;; 		       (float a))))
;;      w h)))
