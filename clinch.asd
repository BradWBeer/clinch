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
	       #:cl-game-math)
  :components ((:file "package")
	       ;;(:file "progv") ;; Take this out until I need it.
	       (:file "clinch")
	       (:file "transform")
	       ;;(:file "vector") ;; REMOVE THIS!!!
	       (:file "node")
	       (:file "shader")
	       (:file "buffer")
	       (:file "texture")
	       (:file "entity")
	       (:file "viewport")
	       (:file "framebuffer")
	       (:file "window")))


