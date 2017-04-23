(asdf:defsystem #:clinch-gui
  :depends-on (cffi cl-pango cl-cairo2 clinch clinch-cairo xmls)
  :version "0.0.1"
  :serial t
  :components ((:module clinch-gui
			:pathname "clinch-gui"
			:components
			((:file "package")
			 (:file "sequences")
			 (:file "editor")
			 
			 ;;(:file "clinch-gui")))))
			 ))))
			       
