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
   #:*texture*
   #:*entity*

   ;; run in main (opengl) thread. Returns when done.
   #:!
   
   ;; run in main (opengl) thread but returns immediately.
   #:!!
   
   ;; Make a vector from the arguments
   #:v!
   
   ;; shortcut to reset an object
   #:!0
   
   ;; shortcuts to reset node translation, rotation and scaling respectively.
   #:!t0 #:!r0 #:!s0  

   ;; shortcuts to set or perform node translation, rotation and scaling respectively.
   #:!t #:!r #:!s

   ;; shortcuts to pullg and pushg respectively (setf (!> obj) value) works too.
   #:!> #:!<

   #:pullg #:pushg    
   #:!reset #:!reset-translation #:!reset-rotation #:!reset-scaling
   #:rotation
   #:scaling
   #:scale
   #:translation
   #:children
   #:transform 
   #:translate 
   #:rotate 
   #:resize 
   #:n*
   #:decompose-transform
   
   #:init
   #:uninit
   #:*root*
   #:*window*
   #:*context*
   #:fbo*
   #:*projection*
   #:*ortho-projection*
   #:*viewport*
   #:*ticks* #:*delta-ticks*
   #:*next*
   #:*on-window-size-changed*
   #:*on-window-resized*
   #:*on-window-hidden*
   #:*on-window-exposed*
   #:*on-window-moved*
   #:*on-window-minimized*
   #:*on-window-maximized*
   #:*on-window-restored*
   #:*on-window-enter*
   #:*on-window-leave*
   #:*on-window-focus-gained*
   #:*on-window-focus-lost*
   #:*on-window-close*
   #:*on-key-down*
   #:*on-key-up*
   #:*on-mouse-move*
   #:*on-mouse-down*
   #:*on-mouse-up*
   #:*on-mouse-click*
   #:*on-mouse-double-click*
   #:*on-mouse-wheel-move*
   #:*on-controller-button-down*
   #:*on-controller-button-up*
   #:*on-controller-added*
   #:*on-controller-removed*
   #:*on-controller-remapped*
   #:*on-controller-axis-move*
   #:*on-idle*
   #:*on-quit*

   #:unload-all-uncollected
   #:shader-program #:name #:get-generic-single-texture-shader #:shader-source #:program #:shader-compile #:frag-shader #:vert-shader #:attributes #:uniforms #:use-shader-program #:get-uniform-id #:get-attribute-id #:attach-uniform #:unload #:bind-static-values-to-attribute
   
   #:buffer #:index-buffer #:id #:qtype #:usage #:stride #:vertex-count #:target #:loaded #:get-size #:size-in-bytes #:bind-buffer-to-vertex-array #:bind-buffer-to-attribute-array #:draw-with-index-buffer #:draw-with-ranged-index-buffer #:map-buffer #:unmap-buffer #:unload #:with-mapped-buffer #:get-buffer-data
   
   #:texture #:tex-id #:width #:height #:data-format #:stride #:target #:bind #:map-buffer #:unmap-buffer #:bind-sampler  #:unload #:bind-with-pbo #:unbind-with-pbo #:with-temporary-pbo #:set-texture-color #:get-default-texture #:get-identity-texture
   #:transform #:make-vector #:transform-point #:ray-triangle-intersect? #:make-matrix #:degrees->radians #:radians->degrees #:d->r #:r->d #:qtype #:transform #:transform->list #:m* #:transpose #:determinate #:scale #:translate #:rotate #:make-orthogonal-transform #:make-frustum-transform  #:make-perspective-transform #:unproject #:get-screen-direction #:data-from-pointer #:make-pbo-for-texture #:make-quad #:make-quad-for-texture
   #:+pi+
   #:ensure-float

   #:get-keyframe #:get-animation-time #:animation #:animator #:frames #:current-position #:repeat #:run-speed #:run-length
   #:play #:paused #:pause #:stop #:skip
   #:update 
   #:texture-animation #:get-current-frame

   #:node #:children #:changed? #:render #:traverse-node

   #:enabled
   #:make-foreign-array #:make-identity-matrix #:list->matrix #:copy-foreign-array #:fill-foreign-array #:cached-matrix #:matrix #:get-current-matrix #:update-current-matrix #:read-gl-matrix #:get-current-gl-matrix #:use-matrix #:save-matrix  #:print-node-data #:m*m #:mT #:det #:m-1 
   
   #:entity #:render-values #:indexes #:render #:make-render-func #:slow-render #:render-value #:ray-entity-intersect?
   
   #:make-pipeline #:pipeline-get-loop #:pipeline-get-init #:pipeline-get-uninit #:run-loop #:run-init #:run-uninit
     
   #:viewport #:x #:y #:width #:height #:add-child #:remove-child #:resize #:quick-set
   
   #:print-text #:with-paragraph
   #:clear-cairo-context
   #:with-surface-for-texture
   #:with-context-for-texture

   #:frame-buffer #:unbind #:depth-buffer #:make-depth-texture #:color-attachment #:make-color-texture #:with-fbo
   #:clear-color #:attribute #:uniform #:init #:clean-up #:enable #:disable #:window-width #:window-height #:*parent*

   ))

