(asdf:defsystem #:clinch-gui
  :depends-on (cffi cl-cairo2 cl-pango clinch)
  :version "0.0.1"
  :serial t
  :components ((:module clinch-cairo
			:pathname "clinch-gui"
			:components
			((:file "package")
			 (:file "clinch-gui")))))
			 
