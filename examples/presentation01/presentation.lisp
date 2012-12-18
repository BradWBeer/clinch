(ql:quickload :cl-cairo2)
(use-package :cairo)
(ql:quickload :clinch)

;; clinch-pango for drawing text. 
(ql:quickload :clinch-pango)

;; GLFL for windowing and I/O
(ql:quickload :cl-glfw)


;; String for the Vertex Shader
;;   t1    is the texture sampler
;;   tc1   are the texture coordinates
;;   v_tc1 is the texture coordinates passed to fragment shader
(setf vert-source
      "
#version 120

uniform sampler2D t1;
attribute vec2 tc1;
varying vec2 v_tc1;
varying vec3   normal;
        void main() {
            gl_Position = ftransform(); //gl_ModelViewProjectionMatrix * gl_Vertex;
            normal = gl_NormalMatrix * gl_Normal;
            v_tc1 = tc1;
        }")

;; String for the Fragment Shader
;;   t1    is the texture sampler
;;   v_tc1 is the texture coordinates from the fragment shader
(setf frag-source
      "
#version 120
uniform sampler2D t1;
varying vec2 v_tc1;
varying vec3  normal;
        void main() {
            gl_FragColor = texture2D(t1, v_tc1) * max(.4, dot(normal, vec3(0, 0, 1)));
        }")

(defun degrees->radians (x)
  (/ (* x pi) 180))


(defun clear-mapped-texture (&optional (r 1) (g 1) (b 1) (a 1) (context *context*))
  (save context)
  (set-source-rgba r g b a context)
  (set-operator :source context)
  (paint context)
  (restore context))



(defmacro qtext (&rest text)
  `(progn (clinch:print-text '("span" (("font_desc" "Century Schoolbook L Roman 20"))
			,@text)
		      :width (* w 3/4))
	  (rel-move-to  0 10)))

(defmacro style-qtext (style &rest text)
  `(progn (clinch:print-text '("span" ,style
			,@text)
		      :width (* w 3/4))
	  (rel-move-to  0 10)))


(let* ((shader)
       (viewport (make-instance 'clinch:viewport))
       (lens   (make-instance 'clinch:transform))
       (camera (make-instance 'clinch:node))
       (hub    (make-instance 'clinch:node))
       (page)
       (vertexes)
       (indexes)
       (tex-coord)
       (normals)
       (texture)
       (entity)
       (pipeline)
       (diff)
       (rad-diff)
       (location 0))

  
  (defun draw-slide (texture slide &optional (w 800) (h 800))
    (clinch:with-mapped-buffer (bits texture :write-only)
      (let ((surf (cairo:create-image-surface-for-data bits :argb32 w h (* w 4))))
	(unwind-protect 
	     (cairo:with-context ((cairo:create-context surf))
	       (clear-mapped-texture)
	       (funcall (nth slide presentation) (1+ slide) w h)
	       )		     
	  (cairo:destroy surf)))))
      
      
      


  (defun window-key-press (key action)
    (format t "window-key-press ~s ~s~%" key action)
    (when (and (= action 1)
	       (member key '(:left :right #\space)))
	  (draw-slide texture (mod (case key
				     (:left (decf location))
				     (:right (incf location))
				     (#\space (setf location 0)))
				   (length presentation)))))
  
  (defun create-rectangle (texture &optional (node hub))
    (make-instance 'clinch:entity
		   :parent node
		   :indexes indexes
		   :shader shader
		   :values `((:attribute "tc1" ,tex-coord)
			     (:attribute "t1" ,texture)
			     (:vertices ,vertexes)
			     (:normals ,normals)
			     )))


  ;; On resize set the camera transform and load it.
  (defun window-size-callback (width height)
    (format t "Resize called with: w=~A h=~A~%" width height)
    (clinch::quick-set viewport 0 0 width height)
    (clinch::render viewport)

    (setf lens (clinch::make-perspective-transform (/ (* 65 pi) 360) (/ width height) .5 100))
    (clinch::use-projection-transform lens)
    (gl:load-identity))


  ;; The start point...    
  (defun start ()
    (declare (optimize (speed 3)))
    (glfw:do-window (:redbits 8
			      :greenbits 8
			      :bluebits 8
			      :alphabits 8
			      :depthbits 16
			      :opengl-version-major 3
			      :opengl-version-minor 1)
	((print "init")
	 ;; set the window event handlers 
	 (glfw:set-window-size-callback 'window-size-callback)
	 (glfw:set-key-callback  'window-key-press)

	 (clinch:qreset camera)
	 (clinch:qreset hub)
	 (clinch:translate hub 0 0 -.8 t) 
	 (clinch:update hub)
	 

	 ;; create the shader. Note how uniforms and attributes are set
	 (setf shader (make-instance 'clinch:shader
	 			     :name "Shader01"
	 			     :vertex-shader-text vert-source
	 			     :fragment-shader-text frag-source
				     :uniforms '(("t1" :int))
				     :attributes '(("tc1" :float))
				     ))
	 
	 
	 ;; create buffers....
	 (setf vertexes (make-instance 'clinch:buffer 
				       :Stride 3
				       :data '(-0.5 -0.5 0.0
					       0.5 -0.5 0.0
					       -0.5  0.5 0.0
					       0.5  0.5 0.0)))
	 (setf indexes  (make-instance 'clinch:buffer :qtype :unsigned-int
				       :target :element-array-buffer
				       :Stride 1
				       :data '(0 1 2 2 1 3)))
	 (setf tex-coord (make-instance 'clinch:buffer
					:stride 2
					:data '(0.0 1.0
						1.0 1.0
						0.0 0.0
						1.0 0.0)))
	 (setf normals (make-instance 'clinch:buffer 
				      :Stride 3
				      :data '(0.0 0.0 1.0
					      0.0 0.0 1.0
					      0.0 0.0 1.0
					      0.0 0.0 1.0)))

	 (setf diff (/ 360 (length presentation)))
	 (setf rad-diff (degrees->radians diff))
	 
	 (setf texture (make-instance 'clinch:texture
				      :width 800
				      :height 800
				      :stride 4
				      :count (* 800 800)
				      :qtype :unsigned-char
				      :target :pixel-unpack-buffer))
	      
	 (setf entity (create-rectangle texture))
	 (setf location 0)
	 (draw-slide texture 0)

	 (let ((rot (coerce (* 2 pi (/ 1 360)) 'single-float)))

	   (setf pipeline (clinch:make-pipeline  
			   :init ((gl:load-identity)
				  (gl:clear-color 0.0 0.0 0.0 0.0)
				  (gl:clear :color-buffer-bit :depth-buffer-bit)
				  (gl:enable :blend :depth-test :line-smooth :point-smooth :polygon-smooth :texture-2d)
				  (%gl:blend-func :src-alpha :one-minus-src-alpha))
			   
			   :loop ((gl:clear :color-buffer-bit :depth-buffer-bit)
				  ;;(clinch:rotate hub rot 0 1 0 t)
				  ;;(clinch:update hub)
				  (clinch:render hub)))))
	 (clinch:run-init pipeline)
	 )
      ;; Main loop
      (clinch:run-loop pipeline (clinch:width viewport) (clinch:height viewport)) 
      )
    ;; End Program
    (clinch:unload vertexes)
    (clinch:unload indexes)
    (clinch:unload tex-coord)
    (clinch:unload normals)
    (clinch:unload shader)
    (print "closed")))


(setf presentation
      (list
       
       (lambda (i w h)
	 (move-to (/ w 8) (/ w 16))
	 (rel-move-to 83 125)
	 	 (clinch:print-text '("span" nil
			       ("span" (("font_desc" "Century Schoolbook L Roman bold 150") ("fgcolor" "#FF0000"))
				"Q")
			       ("span" (("font_desc" "Century Schoolbook L Roman bold 150") ("fgcolor" "#00FF00"))
				"I")
			       ("span" (("font_desc" "Century Schoolbook L Roman bold 150") ("fgcolor" "#0000FF"))
				"X"))
			     :width (* w 3/4)))  
       (lambda (i w h)
	 (move-to (/ w 8) (/ w 16))
	 (qtext ("b" nil "1. ")
		"It's the year 2012, and computers are a pain to use.")
	 (qtext ("b" nil "2. ")
		"I'm tired of needing a new application for every different task.")
	 (qtext ("b" nil "3. ")
		"I'm tired of needing a new data format for every different task.")
	 (qtext ("b" nil "4. ")
		"I'm tired of not being able to connect and use my data in other applications.")
	 (qtext ("b" nil "5. ")
		"I'm tired of not being able to share my data with others.")
	 (qtext ("b" nil "6. ")
		"I'm tired of needing markup, native-code, JIT-Code, scripting languages, database languages, domain specific languages, etc...")
	 (qtext ("b" nil "7. ")
		"I want to use the full power of the computer.")
	 (qtext ("b" nil "8. ")
		"I want the computer to meet me "
		("b" nil "more")
		" than half way."))

       (lambda (i w h)
	 (move-to (/ w 8) (/ w 16))
	 (style-qtext (("font_desc" "Century Schoolbook L Roman 40"))
		      ("b" nil "What is the big idea?"))
	 (rel-move-to 50 0)
	 (qtext ("b" nil "1. ")
		"All data can be held/transmitted/edited in a hierarchical data format.")
	 (qtext ("b" nil "2. ")
		"All data can be displayed in 3D or 2D.")
	 (qtext ("b" nil "3. ")
		"All user data is composed of these types")
	 (rel-move-to 50 20)
	 (qtext ("b" nil "a. ")
		"Text")
	 (qtext ("b" nil "b. ")
		"Vector Graphics")
	 (qtext ("b" nil "c. ")
		"Raster Graphics")
	 (qtext ("b" nil "d. ")
		"2D images on 3D \"mesh\"")
	 (qtext ("b" nil "e. ")
		"\"Raw\" Data"))

       (lambda (i w h)
	 (move-to (/ w 8) (/ w 16))
	 (style-qtext (("font_desc" "Century Schoolbook L Roman 30"))
		      ("b" nil "Everything else is just these types! (think structures)"))
	 (rel-move-to 0 50)	 
	 (qtext "Next we add change over time. (Think Video Player interface)")
	 (rel-move-to 50 20)
	 (qtext ("b" nil "* ")
		"Video -> Vector/Raster changing over time")
	 (qtext ("b" nil "* ")
		"3D Animation -> 3d Meshes changing over time")
	 (qtext ("b" nil "* ")
		"Web Page -> Text, vector, raster, and raw (audio) in a hierarchical format")
	 (qtext ("b" nil "* ")
		"Video Game -> Everything in a complex hierarchy of changes and user input."))

       (lambda (i w h)
	 (move-to (/ w 16) (/ w 16))
	 (rel-move-to 0 125)
	 (clinch:print-text '("span" nil
		       ("span" (("font_desc" "Century Schoolbook L Roman bold 150") ("fgcolor" "#FF0000"))
		       "(")
		       ("span" (("font_desc" "Century Schoolbook L Roman bold 150") ("fgcolor" "#0000FF"))
		       "LISP")
		       ("span" (("font_desc" "Century Schoolbook L Roman bold 150") ("fgcolor" "#FF0000"))
			")"))
		       :width (* w 3/4)))

      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman 30"))
		     ("b" nil "S-Expressions"))
	(rel-move-to 0 50)	 
	(qtext "A Notation for nested list (tree-structured) data")
	(rel-move-to 0 50)	 
	(style-qtext (("font_desc" "DejaVu Sans Mono 40")) "(Hello (world))"))

      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman 30"))
		     ("b" nil "S-Expressions"))
	(rel-move-to 0 50)	 
	(qtext "A Notation for nested list (tree-structured) data")
	(rel-move-to 0 50)	 
	(style-qtext (("font_desc" "DejaVu Sans Mono 40")) "(Hello (world))")
	(rel-move-to 0 50) 
	(qtext "Evaluates an S-Expression (or just expression) by using the first value as the function name."))

      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman 30"))
		     ("b" nil "Everything returns a value: even IF STATEMENTS!" ))
	(rel-move-to 0 50)	 
	(style-qtext (("font_desc" "DejaVu Sans Mono 30"))
		     "(if (awesome? 'Qix)\"
    \"Yes it is!\"
    \"No it is not!\"))")
      (style-qtext (("font_desc" "DejaVu Sans Mono 30")) " ")
      (style-qtext (("font_desc" "DejaVu Sans Mono 30")) "returns => \"Yes it is!\""))
      
      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman 30"))
		     ("b" nil "Lists within lists, the gateway to all understanding..." ))
	(rel-move-to 0 50)	 
   	(style-qtext (("font_desc" "DejaVu Sans Mono 30")) " ")
	(qtext "First it must evaluate the awesome? function, then the if, finally the print."))

      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 30"))  
	"'  <- What is this?")
	(style-qtext (("font_desc" "Century Schoolbook L Roman 30"))  " ")
	(style-qtext (("font_desc" "Century Schoolbook L Roman 30"))  "(quote blah blah blah)")
	(style-qtext (("font_desc" "Century Schoolbook L Roman 30"))  " ")
	(style-qtext (("font_desc" "Century Schoolbook L Roman 30"))  "Ok, that doesn't help."))

      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 30"))  
		     "' = (quote ...) => just returns what is behind it without evaluation.")
	(style-qtext (("font_desc" "Century Schoolbook L Roman 30"))  " ")
	(style-qtext (("font_desc" "Century Schoolbook L Roman 30"))  "simple.")
	(style-qtext (("font_desc" "Century Schoolbook L Roman 30"))  " ")
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 30"))  "You now know lisp!"))
      
      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 30"))  
		     "\"When making something, always start with a problem.\"")
	(rel-move-to 0 50)
	(qtext "I started with two:")
	(qtext " ")
	(qtext ("b" nil "1. ") "Aircraft carrier traffic")
	(qtext " ")
	(qtext ("b" nil "2. ") "Writers")
	(qtext " ")
	(style-qtext (("font_desc" "Century Schoolbook L Roman 20"))  "(also I secretly want to make video games, don't tell anyone.)"))

      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 30"))  
		     "Aircraft carriers")
	(rel-move-to 0 50)
	(qtext "Aircraft moving on a small runway, they must direct traffic.")
	(qtext " ")
	(qtext ("b" nil "What do they use now?"))
	(qtext " ")
	(qtext "Card-board cutouts and a closed-circuit TV."))

      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 30"))  
		     "Writers")
	(rel-move-to 0 50)
	(qtext "No two writers are the same. They have vastly different, individual creative processes.")
	(qtext " ")
	(qtext "There are dozens of software products to help writers, depending on their needs.")
	(qtext " ")
	(qtext "They all take a different data format."))
	
      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 30"))  
		     ("i" nil "Finally, ")
		     "What is QIX?")
	(rel-move-to 0 50)
	(qtext ("b" nil "1. ")
	       ("s" nil "Universal Media Editor")
	       " - What is a universal media editor? Do astronauts use it?")
	(qtext " ")
	(qtext ("b" nil "2. ")
	       ("s" nil "3Dmacs")
	       " - Only programmers know what Emacs is. Makes people think of Big Macs and I don't want to get sued.")
	(qtext " ")
	(qtext ("b" nil "3. ")
	       ("s" nil "BlendEditor")
	       "- Yuck!")
	(qtext " ")
	(qtext ("b" nil "4. Qix")
	       "- \"What is Qix?\" No preconceptions."))

      (lambda (i w h)
		(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 30"))
		     "Let me axe you again, what is Qix?")
	(qtext "Start with a 3D version of xml markup:")
	(qtext " ")
	(style-qtext (("font_desc" "DejaVu Sans Mono 15"))
		     "<rectangle id=\"dynode\">
  <material id=\"my-new-mat\">
     <texture id=\"myTexRes\" 
              width=\"400\" 
              height=\"400\">
        <clear r=\"0\" g=\"0\" b=\"0\"/>
        <rgb r=\"0\" g=\"0\" b=\"0\"/>
        <move-to x=\"0\" y=\"0\"/>
        <line-to x=\"400\" y=\"400\"/>
        <move-to x=\"400\" y=\"0\"/>
        <line-to x=\"0\" y=\"400\"/>
        <line-width width=\"80\"/>
        <stroke/>
     </texture>
  </material>
</rectangle>"))

      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 20"))
		     "There is a problem....I don't know page width!")
	(style-qtext (("font_desc" "DejaVu Sans Mono 15"))
		     "<rectangle id=\"dynode\">
  <material id=\"my-new-mat\">
     <texture id=\"myTexRes\" 
              width=\"400\" 
              height=\"400\">
        <clear r=\"0\" g=\"0\" b=\"0\"/>
        <rgb r=\"0\" g=\"0\" b=\"0\"/>
        <move-to x=\"0\" y=\"0\"/>
	      <script>
              myTexRes.append(
                 new node({type: "line-to", 
                           x: myTexRes.width, 
                           y: myTexRes.height}));
              myTexRes.append(
                 new node({type: "move-to", 
                           x: myTexRes.width, 
                           y: 0}));
              myTexRes.append(
                 new node({type: "line-to", 
                           x: 0, 
                           y: myTexRes.height}));
              myTexRes.append(
                 new node({type: "line-width", 
                           width: myTexRes.width * .2}));
	      </script>
        <stroke/>
     </texture>
  </material>
</rectangle>"))

      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 40"))
		     "It went off the page didn't it?"))
	


      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 30"))
		     "Maybe there is a better way?")
	(qtext "Let's start with a list markup...")
	(qtext " ")
	(style-qtext (("font_desc" "DejaVu Sans Mono 15"))
		     "(with-mesh (make-rectangle :id \"dynode\")
   (with-mesh-material 
      (make-material :id \"my-new-mat\")
         (with-material-texture 
            (make-texture 400 400 :id myTexRes)
               (clear-mapped-texture 0 0 0 0)
	          (set-source-rgb 0 0 0)
	          (move-to 0 0)
	          (line-to 400 400)
	          (move-to 400 0)
	          (line-to 0 400)
	          (set-line-width 80)
	          (stroke))))"))

      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 30"))
		     "Ok, let's make it dynamic...")
	(qtext "wait, it already is!")
	(qtext " ")
	(style-qtext (("font_desc" "DejaVu Sans Mono 15"))
		     "(with-mesh (make-rectangle :id \"dynode\")
   (with-mesh-material 
      (make-material :id \"my-new-mat\")
         (with-material-texture 
            (make-texture 400 400 :id myTexRes)

              (let ((w (get-width this))
                    (h (get-height this)))

                  (clear-mapped-texture 0 0 0 0)
	             (set-source-rgb 0 0 0)
	             (move-to 0 0)
	             (line-to w h)
	             (move-to w 0)
	             (line-to 0 h)
	             (set-line-width (* w .2))
	             (stroke))))"))
          
      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 25"))
		     "One language to rule them all...")
	(qtext " ")
	(qtext "Lisp is used as both markup and scripting.")
	(qtext " ")
	(qtext "Only the \"scripting\" is compiled to machine code.")
	(qtext " ")
	(qtext "Also add event handling: key-press, mouse-click, etc..."))
      
      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 25"))
		     "So what's the big deal?")
	(qtext " ")
	(qtext "Let's imagine we have in interface for importing 3D objects and positioning them in 3D.")
	(qtext " ")
	(qtext "Now I can import a few models of aircraft and a carrier, place them where they need to go, and you are no longer using cardboard cutouts.")
	(qtext " ")
	(qtext "Video games? That's almost too easy!"))      

      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 25"))
		     "PARAGRAPH Function")
	(qtext " ")
	(style-qtext
	 (("font_desc" "DejaVu Sans Mono 20"))
	 "(\"Paragraph\"
   (\"span\" ((\"font_desc\" 
               \"Sans Mono 20\"))
   (\"b\" nil \"All your base)
   \"Are belong to \"
   (\"u\" nil \"us!\")))")

	(qtext " ")

	(style-qtext (("font_desc" "DejaVu Sans Mono 20"))
		     ("b" nil "All your base")
		     " are belong to "
		     ("u" nil "us!")))

      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 25"))
		     "You now have formatted text and graphics...")
	(qtext " ")
	(qtext "So what about the writers?")
	(qtext " ")
	(qtext "Well, since we can put text, graphics, and other data together and put multiple \"pages\" together the same way.")
	(qtext " ")
	(qtext "Writers can organize their data as "
	       ("b" nil "they")
	       " need it.")
	(qtext " ")
	(qtext "What ever the format, you can describe it as a data tree in Lisp and display it in Qix.")
	(qtext " ")
	(qtext "Your data format and viewer is already there, and is infinitely extendable!"))

      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 25"))
		     "Lisp Editor?")
	(qtext " ")
	(style-qtext (("font_desc" "DejaVu Sans Mono 20"))
		     "(print (eval (read stream)))")
	(qtext " " )
	(qtext "or...")
	(qtext "The Read, Eval, Print Loop.")
	(qtext " ")
	(qtext "or..")
	(qtext "the " ("b" nil "REPL")))

      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 25"))
		     "Version Control & Saving")
	(qtext " ")
	(qtext "Qix saves things as files, but add a recursive tree diff algorithm...

And we have added version control and 
undo/redo functionality.

It also fits with the \"Video Control Interface\" so you can see changes over time.

Perhaps we can even create a useful 3D display of multiple files branching."))
      
      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 25"))
		     "Data vs Display")
	(qtext " ")
	(qtext "False separation as the display code is handled the same as the data.

I'm still working on how to share display functions between different data.

Which brings us to..."))
      
      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 25"))
		     "GUI")
	(qtext " ")
	(qtext "Yes, I plan to make a GUI.

Will it be normal \"windows\" GUI...maybe?

I will use the term \"block\" for a 3D window or mesh.

It " ("b" nil "will") " have buttons."))

      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 25"))
		     "Interfacing with blocks")
	(qtext " ")
	(qtext "Look at the edge of a window. What does it do?

The entire edge is a single button which resizes the window.

The top just moves it. 

Again, is there a better way?"))

      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 25"))
		     "Halos")
	(qtext " ")
	(qtext "A halo is a group of buttons (I like round ones) which each do something different.

  Open, close, hide, minimize, etc
  Move the block x/y/z
  Resize width/height/depth 
  Rotate x/y/z 
  Change from text edit to mesh edit.
  Open the drawing menu

The list is endless."))

      (lambda (i w h)
	(let* ((surf2 (cairo:image-surface-create-from-png (concatenate 'string 
									(directory-namestring
									 (asdf:system-relative-pathname :clinch "clinch.asd")) 
									"examples/presentation01/halo.png")))
	       (w2    (image-surface-get-width surf2))
	       (h2    (image-surface-get-height surf2)))
	  
	  (scale (/ 700 w2) (/ 700 h2))
	  (set-source-surface surf2 22 15)
	  (paint)		   
	  (destroy surf2)))


      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 25"))
		     "Command Line Functionality")
	(qtext " ")
	(qtext "Even the most ardent CLI users will admit problems with the CLI.")
	(qtext ("b" nil "1. ")
	       "No one remembers every command.")
	(qtext ("b" nil "2. ") "No one remembers all the command line switches.")
	(qtext ("b" nil "3. ") "You have to redo the entire chain of commands when you make a change.")
	(qtext ("b" nil "4. ") "Moving between GUI data and CLI data is annoying.")
	(qtext ("b" nil "5. ") "It's difficult to do anything other than a straight pipe.")
	(qtext " ")
	(qtext "(Yes, I know about tee and named pipes but that's " ("i" nil "hard") ".)"))

      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 25"))
		     "What if I can hold my data in a block?")
	(qtext "
Ok. I put my data in a block.

Then I can select the block's menu and get a \"right-click menu\" of organized functions I can use.

Let's select sort->unique")
	(qtext "(Like the CLI \"sort -u\" command)

This creates a new block with a line connecting it to the source data.

You now have a block of sorted items."))
	
      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 25"))
		     "What if I can hold my data in a block?")
	(qtext "
Or you could just type it...

" ("b" nil "A Hybrid approch!"))
	(qtext " ")
	(qtext "Imagine not needing to choose."))
      
      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 25"))
		     "What if I can hold my data in block?")
	(qtext "
Now you can change the data.

The sort updates when you are done.

You can even hide the connections & chain multiple items together in a spiderweb to create a real-time data evaluator.

Put it on top of your text document to see if you are using too many words. 

Hmm...this is starting to get useful."))


      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 25"))
		     "What if I can hold my data in block?")
	(qtext "
Now you can change the data.

The sort updates when you are done.

You can even hide the connections & chain multiple items together in a spiderweb to create a real-time data evaluator.

Put it on top of your text document to see if you are using too many words. 

Hmm...this is starting to get useful.

Build your own spreadsheet, anyone?"))

      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 25"))
		     "Sharing Data")
	(qtext "
What if we could give remote users read/write access to our data objects?

What if multiple people could edit a single app at the same time? 

   " ("b" nil "Collaborative Development") "

What each user had his own space in an app? 

   " ("b" nil "Social Application") "

What if I just sent you a blob of data?

   " ("b" nil "Email Replacement")))
	
      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 25"))
		     "What is Lisp Missing?")
	(qtext " ")
	(qtext "A \"safe\" mode for executing foreign code, like javascript is made for.")
	(qtext " ")
	(qtext "Time/Effort/Designers/" ("b" nil "You")))
      
      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 40"))
		     "Questions/Comments?"))

      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 40"))
		     "Technical Weeds")

	(qtext "
Using Horde3D...

I want to replace with simple OpenGL and later DirectX."))

      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 40"))
		     "Architecture")
	(qtext "
Uniforms       -> Per "Object" Values

Vertex Buffers -> Holds per vertex data

Pixel Buffers  -> Holds texture data

Shaders (Vertex and Fragment)  -> Code to take above data and render it

Material       -> Common Shader,  Uniforms, and Pixel Buffers (for reuse)

Pipeline       -> The render loop, what to do and in what order."))

      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 40"))
		     "How is this different?")
	(qtext "
It's lisp, all the way down. (except shaders)

Lisp is more flexible than the xml used in Horde3D

Pipeline is convoluted in XML, should really be a language anyway.

My experience is there will be many other advantages to removing a layer of indirection/metaphor."))

      (lambda (i w h)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Serif bold 125"))
		     "Thank you!"))
      ))
