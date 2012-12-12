;;;; clinch.asd

(asdf:defsystem #:clinch
  :serial t
  :description "Describe CLinch here"
  :author "Brad Beer (WarWeasle)"
  :license "BSD"
  :depends-on (#:cl-cairo2
               #:xmls
               #:cl-opengl
	       #:trivial-garbage)
  :components ((:module pango
                        :pathname "pango"
                        :components ((:file "package")
				     (:file "library")
				     (:file "pango")))
	       (:module freeimage
			:pathname "freeimage"
			:components ((:file "package")
				     (:file "library")
				     (:file "freeimage")))
	       (:file "package")
	       (:file "transform")
	       (:file "shaders")
	       (:file "buffer")
	       (:file "texture")
	       (:file "node")
	       (:file "entity")
	       (:file "viewport")
	       (:file "pipeline")
	       (:file "text")
	       (:file "drawing")))


