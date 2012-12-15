(asdf:defsystem #:pango
  :depends-on (cffi cl-cairo2)
  :version "0.0.1"
  :components ((:module pango
			:pathname "pango"
			:components
			((:file "package")
			 (:file "library")
			 (:file "pango")))))
			       