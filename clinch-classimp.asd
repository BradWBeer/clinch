(asdf:defsystem #:clinch-classimp
  :depends-on (cffi classimp clinch)
  :version "0.0.1"
  :serial t
  :components ((:module clinch-classimp
			:pathname "clinch-classimp"
			:components
			((:file "package")
			 (:file "node")
			 (:file "bone")
			 (:file "node-animation")
			 (:file "skeleton")
			 (:file "clinch-classimp")))))


			       
