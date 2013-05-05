(in-package #:clinch)

(cl:defpackage :clinch-cairo
  (:export #:clear-cairo-context
	   #:with-surface-for-texture
	   #:with-context-for-texture
	   #:create-texture-from-png
	   ))