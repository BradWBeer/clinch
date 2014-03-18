(asdf:defsystem #:clinch-sdl
  :depends-on (cffi clinch :lispbuilder-sdl)
  :version "0.0.1"
  :serial t
  :components ((:module clinch-sdl
			:pathname "clinch-sdl"
			:components
			((:file "package")
			 (:file "clinch-sdl")))))
			       
