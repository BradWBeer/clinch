(asdf:defsystem #:clinch-sdl2
  :depends-on (cffi clinch :sdl2)
  :version "0.0.1"
  :serial t
  :components ((:module clinch-sdl2
			:pathname "clinch-sdl2"
			:components
			((:file "package")
			 (:file "clinch-sdl2")))))
			       
