;;;; package.lisp

(defpackage #:clinch
  (:use #:cl)
  (:export 
	   
;; #:RESET-CLIP 
;; #:PIXEL-BASED-P #:GET-SOURCE
;;                       #:TRANS-MATRIX-YX #:WIDTH
;;                       #:TRANS-MATRIX-XY #:GET-LINE-WIDTH
;;                       #:PATTERN-MESH-SET-CONTROL-POINT
;;                       #:SET-DASH #:SURFACE-FLUSH
;;                       #:TRANSFORM-POINT #:XLIB-IMAGE-CONTEXT
;;                       #:TEXT-PATH #:SET-SOURCE-RGB
;;                       #:TABLE-ANTIALIAS #:CURVE-TO
;;                       #:GET-SCALED-FONT #:MASK-SURFACE
;;                       #:TEXT-HEIGHT #:GET-OPERATOR
;;                       #:SURFACE-WRITE-TO-PNG
;;                       #:PATTERN-MESH-MOVE-TO
;;                       #:TRANS-MATRIX-SCALE
;;                       #:PATTERN-MESH-SET-CORNER-COLOR-RGBA
;;                       #:SET-SOURCE #:SET-SOURCE-COLOR
;;                       #:FONT-OPTIONS-GET-HINT-METRICS
;;                       #:PATTERN-GET-SURFACE #:FONT-ASCENT
;;                       #:SET-LINE-CAP #:PAINT-WITH-ALPHA
;;                       #:TEXT-EXTENTS #:CREATE-PDF-SURFACE
;;                       #:ARC-NEGATIVE #:TABLE-PATTERN-TYPE
;;                       #:SELECT-FONT-FACE #:GLYPH-ARRAY-FILLED
;;                       #:GET-TRANS-MATRIX #:LINE-TO
;;                       #:TRANS-MATRIX-X0
;;                       #:IMAGE-SURFACE-CREATE-FROM-PNG-STREAM
;;                       #:FILL-EXTENTS #:REFERENCE-COUNT
;;                       #:FILL-PRESERVE #:MAKE-TRANS-MATRIX
;;                       #:SYNC-UNLOCK
;;                       #:PATTERN-MESH-GET-CORNER-RGBA
;;                       #:STROKE-PRESERVE #:TEXT-Y-BEARING
;;                       #:SCALED-FONT-GET-SCALE-MATRIX
;;                       #:SET-SOURCE-RGBA #:CREATE-RGB-PATTERN
;;                       #:TRANS-MATRIX-Y0
;;                       #:FT-SCALED-FONT-UNLOCK-FACE
;;                       #:FT-SCALED-FONT-LOCK-FACE
;;                       #:SCALED-FONT-FACE
;;                       #:IMAGE-SURFACE-GET-STRIDE #:FONT-DESCENT
;;                       #:CREATE-FONT #:CREATE-FONT-OPTIONS
;;                       #:PATTERN-MESH-GET-PATCH-COUNT
;;                       #:SCALED-FONT-GET-CTM #:TABLE-STATUS
;;                       #:CREATE-SVG-SURFACE #:GTK2-XLIB-CONTEXT
;;                       #:FONT-OPTIONS-HASH #:TABLE-FONT-SLANT
;;                       #:POINTER #:PATTERN-MESH-LINE-TO
;;                       #:PUSH-GROUP #:CLIP
;;                       #:TABLE-LINE-CAP #:SHOW-PAGE
;;                       #:TABLE-FORMAT #:FONT-HEIGHT
;;                       #:FONT-OPTIONS-MERGE
;;                       #:PATTERN-MESH-BEGIN-PATCH
;;                       #:CREATE-CONTEXT #:DESTROY
;;                       #:SCALED-FONT-GET-FONT-MATRIX
;;                       #:FONT-OPTIONS-SET-HINT-METRICS
;; 		      ;#:ROTATE
;;                       #:FONT-OPTIONS-GET-SUBPIXEL-ORDER
;;                       #:CLIP-EXTENTS #:FONT-MAX-X-ADVANCE
;;                       #:SET-OPERATOR
;;                       #:CREATE-IMAGE-SURFACE-FOR-DATA
;;                       #:WITH-CONTEXT #:CREATE-IMAGE-SURFACE
;;                       #:FONT-OPTIONS-SET-HINT-STYLE
;;                       #:CREATE-PATTERN-FOR-SURFACE
;;                       #:GET-TEXT-EXTENTS
;;                       #:CREATE-IMAGE-SURFACE-FOR-ARRAY
;;                       #:WITH-SYNC-LOCK #:DEG-TO-RAD
;;                       #:SHOW-TEXT
;;                       #:PATTERN-MESH-GET-CONTROL-POINT
;;                       #:CREATE-PS-SURFACE #:TEXT-X-ADVANCE
;;                       #:TRANS-MATRIX-TRANSLATE
;;                       #:GLYPH-ARRAY-SET-GLYPH
;;                       #:PATTERN-GET-EXTEND #:PATTERN-SET-EXTEND
;;                       #:PATTERN-ADD-COLOR-STOP-RGB
;;                       #:POP-GROUP-TO-SOURCE
;;                       #:CREATE-PDF-CONTEXT
;;                       #:PATTERN-ADD-COLOR-STOP-RGBA
;;                       #:IMAGE-SURFACE-CREATE-FROM-PNG
;;                       #:TABLE-EXTEND #:PAINT
;;                       #:SURFACE-FINISH #:CREATE-SCALED-FONT
;;                       #:FILL-PATH #:USER-TO-DEVICE
;;                       #:CREATE-SVG-CONTEXT #:TABLE-LINE-JOIN
;;                       #:MAKE-GLYPH-ARRAY #:SURFACE-MARK-DIRTY
;;                       #:GLYPH-ARRAY #:SCALED-FONT-EXTENTS
;;                       #:STROKE #:PATTERN-GET-COLOR-STOP-COUNT
;;                       #:SET-FILL-RULE #:TEXT-X-BEARING
;;                       #:MASK #:SET-SOURCE-SURFACE
;;                       #:GLYPH-ARRAY-RESET-FILL
;;                       #:GET-FONT-MATRIX #:POP-GROUP
;;                       #:MOVE-TO #:SCALED-FONT-GET-TYPE
;;                       ;;#:SCALE 
;; 		      #:WITH-PNG-SURFACE
;;                       #:RECTANGLE #:WITH-PNG-FILE
;;                       #:TRANS-MATRIX-DISTANCE
;;                       #:PATTERN-GET-TYPE
;;                       #:PATTERN-GET-LINEAR-POINTS
;;                       #:GET-ANTIALIAS #:VERSION
;;                       #:IN-STOKE #:PATTERN-GET-RADIAL-CIRCLES
;;                       #:TABLE-OPERATOR #:GET-LINE-CAP
;;                       #:TRANS-MATRIX-ROTATE
;;                       #:TABLE-SUBPIXEL-ORDER
;;                       #:TRANS-MATRIX-INIT-SCALE
;;                       #:PATTERN-MESH-CURVE-TO
;;                       #:GET-FONT-OPTIONS #:CREATE-COLOR-PATTERN
;;                       #:FONT-OPTIONS #:DEVICE-TO-USER-DISTANCE
;;                       #:WITH-PATTERNS #:GET-FILL-RULE
;;                       #:SYNC-RESET #:SET-MITER-LIMIT
;;                       #:WITH-RADIAL-PATTERN #:TABLE-HINT-STYLE
;;                       #:TRANS-MATRIX-INIT-ROTATE
;;                       #:TABLE-FONT-WEIGHT #:SET-FONT
;;                       #:IMAGE-SURFACE-GET-FORMAT
;;                       #:FONT-OPTIONS-COPY #:PATTERN-GET-FILTER
;;                       #:CREATE-MESH-PATTERN #:SCALED-FONT
;;                       #:HEIGHT #:IN-FILL
;;                       #:IMAGE-SURFACE-GET-WIDTH
;;                       #:PATTERN-GET-RGBA #:RESET-TRANS-MATRIX
;;                       #:PATTERN-GET-MATRIX
;;                       #:CREATE-SIMILAR-IMAGE
;;                       #:IMAGE-SURFACE-GET-HEIGHT #:CLOSE-PATH
;;                       #:PATTERN #:SET-SCALED-FONT
;;                       #:GET-FONT-EXTENTS #:SURFACE
;;                       #:USER-TO-DEVICE-DISTANCE
;;                       #:FONT-OPTIONS-SET-ANTIALIAS #:TEXT-WIDTH
;;                       #:CLIP-PRESERVE #:SET-TRANS-MATRIX
;;                       #:REL-MOVE-TO #:GET-FONT-FACE
;;                       #:IMAGE-SURFACE-GET-DATA
;;                       #:TRANS-MATRIX-MULTIPLY #:SAVE
;;                       #:PATTERN-GET-COLOR-STOPS
;;                       #:PATTERN-GET-COLOR-STOP-RGBA
;;                       #:CREATE-XCB-SURFACE
;;                       #:CREATE-LINEAR-PATTERN
;;                       #:GET-CURRENT-POINT
;;                       #:TRANS-MATRIX-INIT-TRANSLATE
;;                       #:USER-FONT-FACE
;;                       #:FONT-OPTIONS-SET-SUBPIXEL-ORDER
;;                       #:FONT-OPTIONS-GET-HINT-STYLE
;;                       #:SET-LINE-WIDTH #:XCB-SURFACE-SET-SIZE
;;                       ;;#:TRANSLATE
;; 		      #:GET-TARGET
;;                       #:CREATE-RADIAL-PATTERN
;;                       #:TRANS-MATRIX-INVERT #:GLYPH-ARRAY-ADD
;;                       #:CREATE-SURFACE-FROM-FOREIGN
;;                       #:SHOW-GLYPHS #:GLYPH-EXTENTS
;;                       #:SET-FONT-MATRIX
;;                       #:SCALED-FONT-TEXT-EXTENTS
;;                       #:TABLE-HINT-METRICS #:SYNC-LOCK
;;                       #:FONT-OPTIONS-GET-ANTIALIAS
;;                       #:TEXT-Y-ADVANCE #:GET-LINE-JOIN
;;                       #:CONTEXT #:DEVICE-TO-USER
;;                       #:GET-DASH #:CREATE-RGBA-PATTERN
;;                       #:ARC #:PATTERN-SET-MATRIX
;;                       #:TABLE-FILL-RULE #:*CONTEXT*
;;                       #:IMAGE-SURFACE-CREATE-FROM-PNG-CALLBACK
;;                       #:SET-FONT-SIZE
;;                       #:WITH-FT-SCALED-FACE-LOCKED
;;                       #:GET-MITER-LIMIT
;;                       #:PATTERN-ADD-COLOR-STOP #:SET-FONT-FACE
;;                       #:PATTERN-SET-FILET #:TRANS-MATRIX-YY
;;                       #:PATTERN-MESH-END-PATCH #:REL-LINE-TO
;;                       #:SYNC #:WITH-LINEAR-PATTERN
;;                       #:TRANS-MATRIX-XX
;;                       #:SCALED-FONT-GLYPH-EXTENTS
;;                       #:GLYPH-ARRAY-COUNT #:RESTORE
;;                       #:FONT-OPTIONS-EQUAL #:SET-LINE-JOIN
;;                       #:NEW-SUB-PATH #:COPY-PAGE
;;                       #:CREATE-PS-CONTEXT #:FONT-MAX-Y-ADVANCE
;;                       ;;#:TRANSFORM
;; 		      #:REL-CURVE-TO
;;                       #:TABLE-FILTER #:TRANS-MATRIX-P
;;                       #:FONT-FACE #:NEW-PATH
;;                       #:PATTERN-MESH-SET-CORNER-COLOR-RGB
;;                       #:SET-FONT-OPTIONS #:FREETYPE-FONT-FACE
;;                       #:SET-ANTIALIAS

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

