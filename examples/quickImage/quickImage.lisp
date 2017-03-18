(ql:quickload :clinch)
(ql:quickload :clinch-freeimage)
(use-package :clinch)

(init :init-controllers nil)

;; Run this in the main thread
(! (make-quad-for-image 
    (asdf:system-relative-pathname 'clinch 
				   "examples/quickImage/comic-with-shark.jpg")))
