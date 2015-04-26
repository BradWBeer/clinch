;;;; clode.asd

(asdf:defsystem #:clode
  :description "Describe clode here"
  :author "Brad Beer (WarWeasle)"
  :license "Specify license here"
  :depends-on (#:cffi #:sb-cga #:clinch)
  :serial t
  :components ((:module clode
			:pathname "clode"
			:components
			((:file "package")
			 (:file "library")
			 (:file "bindings")
			 (:file "clode")
			 (:file "shapes")))))


