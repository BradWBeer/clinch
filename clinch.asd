;;;; clinch.asd
;;;; Please see the licence.txt for the CLinch 

(asdf:defsystem #:clinch
  :serial t
  :description "An OpenGL Game Engine."
  :author "Brad Beer (WarWeasle)"
  :license "MIT"
  :version "0.5"
  :depends-on (#:cl-opengl
	       #:trivial-garbage
	       #:bordeaux-threads
	       #:trivial-channels
	       #:sdl2
	       #:rtg-math
	       #:swank)
  :components ((:file "package")
	       (:file "threads")
	       (:file "clinch")
	       (:file "gc")
	       (:file "transform")
	       (:file "node")
	       (:file "shaders")
	       (:file "shader-program")
	       (:file "default-shaders")
	       (:file "buffer")
	       (:file "index-buffer")
	       (:file "pixel-buffer")
	       (:file "texture")
	       (:file "2d-node")
	       (:file "entity")
	       (:file "viewport")
	       (:file "framebuffer")
	       (:file "animation")
	       ;;(:file "texture-animation")
	       (:file "window")
	       (:file "shapes")))


