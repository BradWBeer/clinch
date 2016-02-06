;;;; package.lisp
;;;; Please see the licence.txt for the CLinch 

(defpackage #:clinch
  (:use #:cl)
  ;;(:shadow )
  (:import-from :rtg-math.vectors :v!)
  (:export
   #:*uncollected*
   #:defevent
   #:*controllers*
   #:*haptic*

   #:!
   #:!
   #:v!
   #:pullg
   #:!RESET
   #:ROTATION
   #:SCALE
   #:TRANSLATION
   #:CHILDREN
   #:TRANSFORM 
   #:TRANSLATE 
   #:ROTATE 
   #:RESIZE 
   #:N*
   #:decompose-transform
   
   #:init
   #:uninit
   #:*next*
   #:*ON-WINDOW-SIZE-CHANGED*
   #:*ON-WINDOW-RESIZED*
   #:*ON-WINDOW-HIDDEN*
   #:*ON-WINDOW-EXPOSED*
   #:*ON-WINDOW-MOVED*
   #:*ON-WINDOW-MINIMIZED*
   #:*ON-WINDOW-MAXIMIZED*
   #:*ON-WINDOW-RESTORED*
   #:*ON-WINDOW-ENTER*
   #:*ON-WINDOW-LEAVE*
   #:*ON-WINDOW-FOCUS-GAINED*
   #:*ON-WINDOW-FOCUS-LOST*
   #:*ON-WINDOW-CLOSE*
   #:*ON-KEY-DOWN*
   #:*ON-KEY-UP*
   #:*ON-MOUSE-MOVE*
   #:*ON-MOUSE-DOWN*
   #:*ON-MOUSE-UP*
   #:*ON-MOUSE-CLICK*
   #:*ON-MOUSE-DOUBLE-CLICK*
   #:*ON-MOUSE-WHEEL-MOVE*
   #:*ON-CONTROLLER-BUTTON-DOWN*
   #:*ON-CONTROLLER-BUTTON-UP*
   #:*ON-CONTROLLER-ADDED*
   #:*ON-CONTROLLER-REMOVED*
   #:*ON-CONTROLLER-REMAPPED*
   #:*ON-CONTROLLER-AXIS-MOVE*
   #:*ON-IDLE*
   #:*ON-QUIT*

   #:unload-all-uncollected
   #:shader-program #:name #:program #:frag-shader #:vert-shader #:attributes #:uniforms #:use-shader-program #:get-uniform-id #:get-attribute-id #:attach-uniform #:unload #:bind-static-values-to-attribute
   
   #:buffer #:index-buffer #:id #:qtype #:usage #:stride #:vertex-count #:target #:loaded #:get-size #:size-in-bytes #:bind-buffer-to-vertex-array #:bind-buffer-to-attribute-array #:draw-with-index-buffer #:map-buffer #:unmap-buffer #:unload #:with-mapped-buffer #:get-buffer-data
   
   #:texture #:tex-id #:width #:height #:data-format #:stride #:target #:bind #:map-buffer #:unmap-buffer #:with-mapped-buffer #:bind-sampler  #:unload #:with-mapped-texture
   #:transform #:make-vector #:transform-point #:ray-triangle-intersect? #:make-matrix #:degrees->radians #:radians->degrees #:d->r #:r->d #:qtype #:transform #:transform->list #:m* #:transpose #:determinate #:inverse #:scale #:translate #:rotate #:make-orthogonal-transform #:make-frustum-transform  #:make-perspective-transform #:unproject #:get-screen-direction 

   #:node #:children #:changed? #:render 

   #:enabled
   #:make-foreign-array #:make-identity-matrix #:list->matrix #:copy-foreign-array #:fill-foreign-array #:cached-matrix #:matrix #:get-current-matrix #:update-current-matrix #:read-gl-matrix #:get-current-gl-matrix #:use-matrix #:save-matrix  #:print-node-data #:m*m #:mT #:det #:m-1 
   
   #:entity #:render-values #:indexes #:render #:make-render-func #:slow-render #:render-value #:ray-entity-intersect?
   
   #:make-pipeline #:pipeline-get-loop #:pipeline-get-init #:pipeline-get-uninit #:run-loop #:run-init #:run-uninit
     
   #:viewport #:x #:y #:width #:height #:add-child #:remove-child #:resize #:quick-set
   
   #:print-text #:with-paragraph
   #:clear-cairo-context
   #:with-surface-for-texture
   #:with-context-for-texture

   #:frame-buffer #:unbind
   #:clear-color #:attribute #:init #:clean-up #:enable #:disable #:blend-func #:window-width #:window-height #:*parent*

   ))

