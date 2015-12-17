(asdf:defsystem #:clinch-slime
  :depends-on (cffi clinch swank)
  :version "0.0.1"
  :serial t
  :components ((:module clinch-slime
			:pathname "clinch-slime"
			:components
			((:file "package")
			 (:file "clinch-slime")))))
			       
