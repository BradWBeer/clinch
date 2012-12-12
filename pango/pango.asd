(asdf:defsystem #:pango
  :depends-on (cffi)
  :version "0.0.1"
  :components ((:file "package")
	       (:file "library")
	       (:file "pango"
		      :depends-on ("package"))))