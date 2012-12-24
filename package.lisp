;;;; package.lisp
;;;; Please see the licence.txt for the CLinch 

(defpackage #:clinch
  (:use #:cl)
  (:export 
 
   #:shader #:name #:program #:frag-shader #:vert-shader #:attributes #:uniforms #:use-shader #:get-uniform-id #:get-attribute-id #:attach-uniform #:unload #:bind-static-values-to-attribute
   
   #:buffer #:id #:qtype #:usage #:stride #:vertex-count #:target #:loaded #:get-size #:size-in-bytes #:bind-buffer-to-vertex-array #:bind-buffer-to-attribute-array #:draw-with-index-buffer #:map-buffer #:unmap-buffer #:unload #:with-mapped-buffer
   
   #:texture #:tex-id #:width #:height #:data-format #:stride #:target #:bind #:map-buffer #:unmap-buffer #:bind-sampler  #:unload 
   #:transform #:degrees->radians #:qtype #:transform #:transform->list #:set-identity-transform #:qreset #:m* #:transpose #:determinate #:invert #:scale #:translate #:rotate #:make-orthogonal-transform #:make-frustum-transform  #:make-perspective-transform #:get-current-gl-matrix #:use-transform #:use-projection-transform #:M*V 

   #:node #:children #:current-transform #:changed? #:update #:render 


   #:make-foreign-array #:make-identity-matrix #:list->matrix #:copy-foreign-array #:fill-foreign-array #:cached-matrix #:matrix #:get-current-matrix #:update-current-matrix #:read-gl-matrix #:get-current-gl-matrix #:use-matrix #:save-matrix  #:print-node-data #:m*m #:mT #:det #:m-1 
   
   #:entity #:shader #:indexes #:render #:make-render-func #:slow-render 
   
   #:make-pipeline #:pipeline-get-loop #:pipeline-get-init #:pipeline-get-uninit #:run-loop #:run-init #:run-uninit
     
   #:viewport #:x #:y #:width #:height #:add-child #:resize #:quick-set
   
   #:print-text #:with-paragraph
   #:clear-cairo-context
   #:with-surface-for-texture
   #:with-context-for-texture

   
   ))

