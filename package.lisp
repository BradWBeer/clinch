;;;; package.lisp
;;;; Please see the licence.txt for the CLinch 

(defpackage #:clinch
  (:use #:cl)
  (:export 
	   

		      #:shader #:name #:program #:frag-shader #:vert-shader #:attributes #:uniforms #:use-shader #:get-uniform-id #:get-attribute-id #:attach-uniform #:unload #:bind-static-values-to-attribute
		      
		      #:buffer #:id #:type #:stride #:vertex-count #:target #:loaded #:set-raw-data #:size-in-bytes #:bind-buffer-to-vertex-array #:bind-buffer-to-attribute-array #:draw-with-index-buffer #:bind-buffer-to-attrib #:map-buffer #:unmap-buffer #:unload #:bind #:btype

		      #:texture #:tex-id #:width #:height #:bind-sampler #:with-loaded-32bit-map #:unload

		      #:node #:parent #:children #:make-foreign-array #:make-identity-matrix #:list->matrix #:copy-foreign-array #:fill-foreign-array #:cached-matrix #:matrix #:get-current-matrix #:update-current-matrix #:read-gl-matrix #:get-current-gl-matrix #:use-matrix #:save-matrix  #:print-node-data #:m*m #:mT #:det #:m-1 

		      #:entity #:shader #:indexes #:render #:make-render-func #:slow-render 

		      #:make-pipeline #:pipeline-get-loop #:pipeline-get-init #:pipeline-get-uninit
		      
		      #:camera #:active #:pipeline

		      #:viewport #:x #:y #:width #:height #:cameras #:resize #:quick-set

		      #:print-text #:with-paragraph		      
))

