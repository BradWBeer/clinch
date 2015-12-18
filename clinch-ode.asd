;;;; clinch-ode.asd

(asdf:defsystem #:clinch-ode
  :description "Describe clode here"
  :author "Brad Beer (WarWeasle)"
  :license "Specify license here"
  :depends-on (#:cffi #:sb-cga #:clinch #:cl-ode)
  :serial t
  :components ((:module clode
			:pathname "clode"
			:components
			((:file "ode-node")
			 (:file "player")))))


