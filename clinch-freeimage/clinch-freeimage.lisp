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

    

      
      

