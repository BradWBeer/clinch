(asdf:defsystem #:freeimage
  :depends-on (cffi)
  :version "0.0.1"
  :components ((:file "package")
  	       (:file "library")
  	       (:file "freeimage"
  		      :depends-on ("package"))))