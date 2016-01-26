(asdf:defsystem #:clinch-pango
  :depends-on (cffi cl-pango cl-cairo2 clinch clinch-cairo xmls)
  :version "0.0.1"
  :serial t
  :components ((:module clinch-pango
			:pathname "clinch-pango"
			:components
			((:file "package")
			 (:file "clinch-pango")))))
			       
