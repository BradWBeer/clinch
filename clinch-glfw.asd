(asdf:defsystem #:clinch-glfw
  :depends-on (cffi clinch cl-glfw)
  :version "0.0.1"
  :serial t
  :components ((:module clinch-glfw
			:pathname "clinch-glfw"
			:components
			((:file "package")
			 (:file "clinch-glfw")))))
			       
