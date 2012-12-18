(asdf:defsystem #:clinch-pango
  :depends-on (cffi pango cl-cairo2 clinch clinch-cairo xmls)
  :version "0.0.1"
  :components ((:module clinch-pango
			:pathname "clinch-pango"
			:components
			((:file "package")
			 (:file "clinch-pango")))))
			       