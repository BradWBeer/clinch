;;;; clinch.asd
;;;; Please see the licence.txt for the CLinch 

(asdf:defsystem #:clinch
  :serial t
  :description "Describe CLinch here"
  :author "Brad Beer (WarWeasle)"
  :license "MIT"
  :version "0.1"
  :depends-on (#:cl-opengl
	       #:trivial-garbage
	       #:bordeaux-threads
	       #:trivial-channels
	       #:sdl2
	       #:cl-game-math
	       #:swank)
  :components ((:file "package")
	       (:file "clinch")
	       (:file "transform")
	       (:file "node")
	       (:file "shaders")
	       (:file "shader-program")
	       (:file "buffer")
	       (:file "texture")
	       (:file "entity")
	       (:file "viewport")
	       (:file "framebuffer")
	       (:file "window")))


