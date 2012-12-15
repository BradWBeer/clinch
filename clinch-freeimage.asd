(asdf:defsystem #:clinch-freeimage
  :depends-on (cffi clinch freeimage)
  :version "0.0.1"
  :components ((:module clinch-freeimage
			:pathname "clinch-freeimage"
			:components
			((:file "package")
			 (:file "clinch-freeimage")))))
			       