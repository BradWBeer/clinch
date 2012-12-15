;; NOT YET PORTED FROM QIX TO CLINCH!!!!!!!

(error "NOT YET PORTED FROM QIX TO CLINCH!!!!!!!")

(ql:quickload :cl-glfw)
(ql:quickload :horde3d)
(ql:quickload :cl-cairo2)
(ql:quickload :bordeaux-threads)
(ql:quickload :alexandria)
(ql:quickload :pango)
(ql:quickload :xmls)
(use-package :cairo)

(setf hub-dist 9)

(setf *queue* nil)
(setf *time* nil)
(setf *old-time* nil)
(setf *delta-time* nil)
(setf moving nil)
(setf rot 0)
(setf position 0)
(setf speed .01)
(setf dist 10.21)
(setf move-d nil)

(setf degrees nil
      radians nil
      tex nil
      mat nil
      node nil
      w nil
      h nil
      w/2 nil
      h/2 nil)

(defmacro print-text (text &key (width nil) (wrap :pango_wrap_word))
  `(with-paragraph (:width ,width :wrap ,wrap)
     (save)
     (pango:pango_layout_set_markup *layout* (xmls:toxml
					      ,text) -1)
    
     (pango:pango_cairo_update_layout (slot-value *context* 'pointer) *layout*)
     (pango:pango_cairo_show_layout (slot-value *context* 'pointer) *layout*)
     (restore)
     ;(print (nth-value 1 (pango:get-layout-line-extents *layout*)))
     (cairo:rel-move-to 0 (nth-value 1 (pango:get-layout-size *layout*)))
     ))





(defun degrees->radians (x)
  (/ (* x pi) 180))

(defmacro add-to-queue (&body body)
  `(setf *queue*
	 (push
	  (lambda ()
	    ,@body)
	  *queue*)))
		  
(defmacro with-foreign-alloc ((var type &optional (contents nil)) &body body)
  "Bind VAR to a pointer to COUNT objects of TYPE during BODY.
The buffer has dynamic extent and may be stack allocated."
  `(let ((,var (cffi:foreign-alloc ,type :initial-contents ,contents :count (if ,contents
										(length ,contents)
										1))))
     (unwind-protect 
	  (progn ,@body)
       (cffi:foreign-free ,var))))

(defmacro with-foreign-allocs (bindings &body body)
  (if bindings
      `(with-foreign-alloc ,(car bindings)
         (with-foreign-allocs ,(cdr bindings)
           ,@body))
      `(progn ,@body)))

(defun make-rectangle-geometry (w h name)
  (let* ((h+ (/ h 2))
	 (h- (- h+))
	 (w+ (/ w 2))
	 (w- (- w+)))
  (h3d:create-geometry-resource name 4 6
				(list w- h- 0 
				      w+ h- 0
				      w- h+ 0
				      w+ h+ 0)
				'(0 1 2 2 1 3)
				'(0 0 1 0 0 1 0 0 1 0 0 1) 
				nil nil
				'(0 0
				  1 0
				  0 1
				  1 1) nil)))

(defun create-rectangle (name material &key (w 1) (h 1) (node h3d:+root-node+))
  (h3d:Add-Mesh-Node
   (h3d:Add-Model-Node node (concatenate 'string name "-dynamic-model-node") 
		       (make-rectangle-geometry w h (concatenate 'string name "-dynamic-geometry-resource")))
   name material 0 6 0 3))

(defun create-dynamic-texture (name w h)
  (h3d:create-texture name w h :texture-bgra8 :No-texture-mipmaps))

(defmacro with-mapped-texture ((texture-resource &key (data-name 'data) (surface-name '*surface*)) &body body)
  (let ((gen-w (gensym))
	(gen-h (gensym)))
	
  `(let* ((,gen-w (get-texture-resource-width ,texture-resource))
	  (,gen-h (get-texture-resource-height ,texture-resource))
	  (,data-name (%h3d:map-resource-stream ,texture-resource :texture-image-element 0 :texture-image-pixel-stream 0 1))
	  (,surface-name (cairo:create-image-surface-for-data ,data-name :argb32 ,gen-w ,gen-h (* ,gen-w 4))))
     
     (with-context ((create-context ,surface-name))
       (unwind-protect
	    (progn ,@body)
	 (h3d:unmap-resource-stream ,texture-resource)))
     ,texture-resource)))

(defmacro with-paragraph ((&key (layout '*layout*)  (context '*context*) width wrap) &body body)
  (let ((gwidth (gensym))
	(gwrap (gensym)))

  `(let ((,layout (pango:pango_cairo_create_layout
		   (slot-value ,context 'pointer)))
	 (,gwidth (* pango:pango_scale ,width))
	 (,gwrap ,wrap))
     
     (when (and ,gwidth ,gwrap)
       (pango:pango_layout_set_wrap ,layout ,gwrap)
       (pango:pango_layout_set_width ,layout ,gwidth))

     (unwind-protect 
	  (progn ,@body)
       (pango:g_object_unref ,layout)))))


(defun clear-mapped-texture (&optional (r 1) (g 1) (b 1) (a 1) (context *context*))
  (save context)
  (set-source-rgba r g b a context)
  (set-operator :source context)
  (paint context)
  (restore context))

(defun get-texture-resource-width (tex)
  (h3d:resource-parameter tex :texture-image-element 0 :texture-image-width))

(defun get-texture-resource-height (tex)
  (h3d:resource-parameter tex :texture-image-element 0 :texture-image-height))

(defun create-dynamic-material (texture name)
  (let ((mat (h3d:add-resource :material name 0))
	(str (concatenate 'string
			  "<Material><Shader source=\"shaders/model.shader\" /><ShaderFlag name=\"_F01_Skinning\" /><ShaderFlag name=\"_F05_AlphaTest\" /><Sampler name=\"albedoMap\" map=\"" (h3d:get-resource-name texture)  "\" /></Material>")))
    (cffi:with-foreign-string (pstr str)       
      (h3d:load-resource mat pstr (length str)))
    mat))


(defun window-size-callback (w h)
  
  ;; Resize viewport
  (setf (h3d:node-parameter camera :camera-viewport-x) 0)
  (setf (h3d:node-parameter camera :camera-viewport-y) 0)
  (setf (h3d:node-parameter camera :camera-viewport-width) w)
  (setf (h3d:node-parameter camera :camera-viewport-height) h)
  
  ;; Set virtual camera parameters
  (h3d:setup-camera-view camera 45.0 (float (/ w h)) 0.1 1000.0)
  (h3d:resize-pipeline-buffers hdr-pipeline w h)
  (h3d:resize-pipeline-buffers fwd-pipeline w h))

(setf count 0)  
(setf skyR nil)
(setf sky nil)
(setf light-mat nil)
(setf hdr-pipeline nil)
(setf fwd-pipeline nil)
(setf mat nil)
(setf dynode nil)
(setf light nil)
(setf texR nil)
(setf hub nil)
(setf diff nil)
(setf rad-diff nil)


(defmacro qtext (&rest text)
  `(progn (print-text '("span" (("font_desc" "Century Schoolbook L Roman 20"))
			,@text)
		      :width (* w 3/4))
	  (rel-move-to  0 10)))

(defmacro style-qtext (style &rest text)
  `(progn (print-text '("span" ,style
			,@text)
		      :width (* w 3/4))
	  (rel-move-to  0 10)))


(setf present
      (list
       
       (lambda (i)
	 (move-to (/ w 8) (/ w 16))
	 (rel-move-to 83 125)
	 	 (print-text '("span" nil
			       ("span" (("font_desc" "Century Schoolbook L Roman bold 150") ("fgcolor" "#FF0000"))
				"Q")
			       ("span" (("font_desc" "Century Schoolbook L Roman bold 150") ("fgcolor" "#00FF00"))
				"I")
			       ("span" (("font_desc" "Century Schoolbook L Roman bold 150") ("fgcolor" "#0000FF"))
				"X"))
			     :width (* w 3/4)))  
       (lambda (i)
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

       (lambda (i)
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

       (lambda (i)
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

       (lambda (i)
	 (move-to (/ w 16) (/ w 16))
	 (rel-move-to 0 125)
	 (print-text '("span" nil
		       ("span" (("font_desc" "Century Schoolbook L Roman bold 150") ("fgcolor" "#FF0000"))
		       "(")
		       ("span" (("font_desc" "Century Schoolbook L Roman bold 150") ("fgcolor" "#0000FF"))
		       "LISP")
		       ("span" (("font_desc" "Century Schoolbook L Roman bold 150") ("fgcolor" "#FF0000"))
			")"))
		       :width (* w 3/4)))

      (lambda (i)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman 30"))
		     ("b" nil "S-Expressions"))
	(rel-move-to 0 50)	 
	(qtext "A Notation for nested list (tree-structured) data")
	(rel-move-to 0 50)	 
	(style-qtext (("font_desc" "DejaVu Sans Mono 40")) "(Hello (world))"))

      (lambda (i)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman 30"))
		     ("b" nil "S-Expressions"))
	(rel-move-to 0 50)	 
	(qtext "A Notation for nested list (tree-structured) data")
	(rel-move-to 0 50)	 
	(style-qtext (("font_desc" "DejaVu Sans Mono 40")) "(Hello (world))")
	(rel-move-to 0 50) 
	(qtext "Evaluates an S-Expression (or just expression) by using the first value as the function name."))

      (lambda (i)
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
      
      (lambda (i)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman 30"))
		     ("b" nil "Lists within lists, the gateway to all understanding..." ))
	(rel-move-to 0 50)	 
   ;; 	(style-qtext (("font_desc" "DejaVu Sans Mono 30"))
   ;; 		     "(print 
   ;; (if (awesome? 'Qix)\"
   ;;  \"Yes it is!\"
;;    \"No it is not!\")))")
	(style-qtext (("font_desc" "DejaVu Sans Mono 30")) " ")
	(qtext "First it must evaluate the awesome? function, then the if, finally the print."))

      (lambda (i)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 30"))  
	"'  <- What is this?")
	(style-qtext (("font_desc" "Century Schoolbook L Roman 30"))  " ")
	(style-qtext (("font_desc" "Century Schoolbook L Roman 30"))  "(quote blah blah blah)")
	(style-qtext (("font_desc" "Century Schoolbook L Roman 30"))  " ")
	(style-qtext (("font_desc" "Century Schoolbook L Roman 30"))  "Ok, that doesn't help."))

      (lambda (i)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 30"))  
		     "' = (quote ...) => just returns what is behind it without evaluation.")
	(style-qtext (("font_desc" "Century Schoolbook L Roman 30"))  " ")
	(style-qtext (("font_desc" "Century Schoolbook L Roman 30"))  "simple.")
	(style-qtext (("font_desc" "Century Schoolbook L Roman 30"))  " ")
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 30"))  "You now know lisp!"))
      
      (lambda (i)
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

      (lambda (i)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 30"))  
		     "Aircraft carriers")
	(rel-move-to 0 50)
	(qtext "Aircraft moving on a small runway, they must direct traffic.")
	(qtext " ")
	(qtext ("b" nil "What do they use now?"))
	(qtext " ")
	(qtext "Card-board cutouts and a closed-circuit TV."))

      (lambda (i)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 30"))  
		     "Writers")
	(rel-move-to 0 50)
	(qtext "No two writers are the same. They have vastly different, individual creative processes.")
	(qtext " ")
	(qtext "There are dozens of software products to help writers, depending on their needs.")
	(qtext " ")
	(qtext "They all take a different data format."))
	
      (lambda (i)
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

      (lambda (i)
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

      (lambda (i)
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

      (lambda (i)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 40"))
		     "It went off the page didn't it?"))
	


      (lambda (i)
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

      (lambda (i)
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
          
      (lambda (i)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 25"))
		     "One language to rule them all...")
	(qtext " ")
	(qtext "Lisp is used as both markup and scripting.")
	(qtext " ")
	(qtext "Only the \"scripting\" is compiled to machine code.")
	(qtext " ")
	(qtext "Also add event handling: key-press, mouse-click, etc..."))
      
      (lambda (i)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 25"))
		     "So what's the big deal?")
	(qtext " ")
	(qtext "Let's imagine we have in interface for importing 3D objects and positioning them in 3D.")
	(qtext " ")
	(qtext "Now I can import a few models of aircraft and a carrier, place them where they need to go, and you are no longer using cardboard cutouts.")
	(qtext " ")
	(qtext "Video games? That's almost too easy!"))      

      (lambda (i)
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

      (lambda (i)
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

      (lambda (i)
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

      (lambda (i)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 25"))
		     "Version Control & Saving")
	(qtext " ")
	(qtext "Qix saves things as files, but add a recursive tree diff algorithm...

And we have added version control and 
undo/redo functionality.

It also fits with the \"Video Control Interface\" so you can see changes over time.

Perhaps we can even create a useful 3D display of multiple files branching."))
      
      (lambda (i)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 25"))
		     "Data vs Display")
	(qtext " ")
	(qtext "False separation as the display code is handled the same as the data.

I'm still working on how to share display functions between different data.

Which brings us to..."))
      
      (lambda (i)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 25"))
		     "GUI")
	(qtext " ")
	(qtext "Yes, I plan to make a GUI.

Will it be normal \"windows\" GUI...maybe?

I will use the term \"block\" for a 3D window or mesh.

It " ("b" nil "will") " have buttons."))

      (lambda (i)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 25"))
		     "Interfacing with blocks")
	(qtext " ")
	(qtext "Look at the edge of a window. What does it do?

The entire edge is a single button which resizes the window.

The top just moves it. 

Again, is there a better way?"))

      (lambda (i)
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

      (lambda (i)
	(let* ((surf2 (cairo:image-surface-create-from-png "/home/brad/work/lisp/qix/halo.png"))
	       (w2    (image-surface-get-width surf2))
	       (h2    (image-surface-get-height surf2)))
	  
	  (scale (/ 700 w2) (/ 700 h2))
	  (set-source-surface surf2 22 15)
	  (paint)		   
	  (destroy surf2)))


      (lambda (i)
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

      (lambda (i)
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
	
      (lambda (i)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 25"))
		     "What if I can hold my data in a block?")
	(qtext "
Or you could just type it...

" ("b" nil "A Hybrid approch!"))
	(qtext " ")
	(qtext "Imagine not needing to choose."))
      
      (lambda (i)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 25"))
		     "What if I can hold my data in block?")
	(qtext "
Now you can change the data.

The sort updates when you are done.

You can even hide the connections & chain multiple items together in a spiderweb to create a real-time data evaluator.

Put it on top of your text document to see if you are using too many words. 

Hmm...this is starting to get useful."))


      (lambda (i)
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

      (lambda (i)
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
	
      (lambda (i)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 25"))
		     "What is Lisp Missing?")
	(qtext " ")
	(qtext "A \"safe\" mode for executing foreign code, like javascript is made for.")
	(qtext " ")
	(qtext "Time/Effort/Designers/" ("b" nil "You")))
      
      (lambda (i)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 40"))
		     "Questions/Comments?"))

      (lambda (i)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 40"))
		     "Technical Weeds")

	(qtext "
Using Horde3D...

I want to replace with simple OpenGL and later DirectX."))

      (lambda (i)
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

      (lambda (i)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Century Schoolbook L Roman bold 40"))
		     "How is this different?")
	(qtext "
It's lisp, all the way down. (except shaders)

Lisp is more flexible than the xml used in Horde3D

Pipeline is convoluted in XML, should really be a language anyway.

My experience is there will be many other advantages to removing a layer of indirection/metaphor."))

      (lambda (i)
	(move-to (/ w 8) (/ w 16))
	(style-qtext (("font_desc" "Serif bold 125"))
		     "Thank you!"))
      ))

(defun char-callback (char event)
  (format t "char-callback ~A ~A~%" char event))
  
(defun make-mover (start finish time)
  (let* ((now (glfw:get-time))
	 (end (+ now time)))
    (lambda (x)
      (cond ((< x now) start)
	    ((> x end) finish)
	    (t (+ start
		  (*(- finish start)
		    (/ (- x now) time))))))))
		    

	       
(defun key-callback (key event)
  (when (= 0 event)
    (case key
      (:right ;(setf moving -1)
       (setf moving (make-mover (* position diff) (* (decf position) diff) .5)))
      
      (:f1 (setf position 0)
	   (setf moving (lambda (x)
			  0)))

      (:left  ;(setf moving 1)
       (setf moving (make-mover (* position diff) (* (incf position) diff) .51)))))

  (case key
    (:up (if (= event 1)
	     (setf move-d (- speed))
	     (setf move-d nil)))
    
    (:down (if (= event 1)
	       (setf move-d speed)
	       (setf move-d nil))))
  
  (format t "key-callback ~A ~A~%" key event))

(defun mouse-button-callback (button event)
    (format t "mouse-button-callback ~A ~A~%" button event))


(defun mouse-pos-callback (x y)
    (format t "mouse-pos-callback ~A ~A~%" x y))


(defun start ()	      


  (glfw:do-window (:title "Qix Presentation" 
			  :redbits 8
			  :greenbits 8
			  :bluebits 8
			  :alphabits 8
			  :depthbits 16)
      
      ((format t "h3d:init returned : ~A~%" (h3d:init))

       (setf moving (lambda (x)
		      0))
       (setf rot 0)
       (setf position 0)
       (setf move-d nil)
       (setf dist 10)

       (h3d:set-option :load-textures 1)
       (h3d:set-option :texture-compression 0)
       (h3d:set-option ::fast-animation 0)
       (h3d:set-option :load-textures 1)
       (h3d:set-option :max-anisotropy 4)
       (h3d:set-option :shadow-map-size 2048)
       (h3d:set-option :max-log-level 2048)

       (setf hdr-pipeline (h3d:add-resource :pipeline "pipelines/hdr.pipeline.xml"))
       (setf fwd-pipeline (h3d:add-resource :pipeline "pipelines/forward.pipeline.xml"))

       (setf light-mat (h3d:add-resource :material "materials/light.material.xml"))


       (setf skyR (h3d:Add-Resource :Scene-Graph "models/skybox/skybox.scene.xml"))
       (h3d:load-resources-from-disk "/home/brad/work/external/Horde3D_SDK_1.0.0_Beta5/Horde3D/Binaries/Content")

       

       (setf sky (h3d:add-nodes h3d:+ROOT-NODE+ skyR))
       (h3d:set-node-transform sky 0 0 0 0 0 0 210 50 210)
       (h3d:set-node-flags sky :no-cast-shadow t)

       (setf light (h3d:add-light-node h3d:+root-node+ "Light1" 0 "LIGHTING" "SHADOWMAP"))
       (h3d:set-node-transform light 0 5 13 -60 0 0 1 1 1 )
       (setf (h3d:node-parameter light :light-radius) 30)
       (setf (h3d:node-parameter light :light-fov) 90)
       (setf (h3d:node-parameter light :light-shadow-map-count) 1)
       (setf (h3d:node-parameter light :light-shadow-map-bias) 1)
       (setf (h3d:node-parameter light :light-color :component 0) 1.0)
       (setf (h3d:node-parameter light :light-color :component 1) 1.0)
       (setf (h3d:node-parameter light :light-color :component 2) 1.0)
       (setf (h3d:node-parameter light :light-color-multiplier) 1.0)


       (setf texR (create-dynamic-texture "myTexRes" 800 800))
       (let* ((w (get-texture-resource-width texR))
	      (h (get-texture-resource-height texR))
	      (w/2 (/ w 2))
	      (h/2 (/ h 2)))
	 (with-mapped-texture (texR)
	   (clear-mapped-texture 0 0 0 0)
	   (set-source-rgb 0 0 0)
	 (move-to 0 0)
	 (line-to w h)
	 (move-to w 0)
	 (line-to 0 h)
	 (set-line-width (* w .2))
	 (stroke)
	 (rectangle 0 0 w/2 h/2)
	 (set-source-rgba 1 0 0 0.80)
	 (fill-path)
	 
	 (rectangle 0 h/2 w/2 h/2)
	 (set-source-rgba 1 1 0 .60)
	 (fill-path)
	 
	 (rectangle w/2 0 w/2 h/2)
	 (set-source-rgba 0 0 1 .4)
	 (fill-path)

	 (set-source-rgba 0 0 0 1)
	 
	 (with-paragraph (:width w :wrap :pango_wrap_word) 
	   (setf str
		 '("span" (("font_desc" "Century Schoolbook L Roman 20"))
		   "Modern science has proved that the fundamental traits of every individual are indelibly stamped in the shape of his body, head, face and hands--an X-ray by which you can read the characteristics of any person on sight."))
	   
	   (pango:pango_layout_set_markup *layout* (xmls:toxml str) -1)
	   
	   (pango:pango_cairo_update_layout (slot-value *context* 'pointer) *layout*)
	   (pango:pango_cairo_show_layout (slot-value *context* 'pointer) *layout*))))

       (setf mat (create-dynamic-material texR "my-new-mat"))
       (h3d:load-resources-from-disk "/home/brad/work/external/Horde3D_SDK_1.0.0_Beta5/Horde3D/Binaries/Content")
       
       (setf hub (%h3d:add-group-node %h3d:+root-node+ "hub-node"))

       (setf diff (/ 360 (length present)))
       (setf rad-diff (degrees->radians diff))


       (loop
	  for i from 0 
	  for image in present
	  do (setf degrees (* i diff)
		   radians (degrees->radians degrees)
		   tex (create-dynamic-texture (concatenate 'string "myTexRes-" (princ-to-string i)) 800 800)
		   mat (create-dynamic-material tex (concatenate 'string "myNewMat-" (princ-to-string i)))
		   node (create-rectangle (concatenate 'string "dynode-" (princ-to-string i)) mat :node hub))
	    (with-mapped-texture (tex)
	      (setf w (get-texture-resource-width texR)
		    h (get-texture-resource-height texR)
		    w/2 (/ w 2)
		    h/2 (/ h 2))
		
	      (clear-mapped-texture)
	      (funcall image i)
	      (cairo:surface-write-to-png *surface* (format nil "/tmp/qix/qix-~3,'0D.png" i))
	      (h3d:set-node-transform node (* hub-dist (sin radians))  0 (* hub-dist (cos radians)) 0 degrees 0 1 1 1)))
	       

		

       (setf camera (h3d:Add-Camera-Node h3d:+ROOT-NODE+ "Camera" fwd-pipeline))
       ;;(h3d:Set-Node-Transform camera 0 0 5.25 0 0 0 1 1 1)


       (glfw:set-window-size-callback 'window-size-callback)
       (glfw:set-char-callback 'char-callback)
       (glfw:set-key-callback 'key-callback)
       (glfw:set-mouse-button-callback 'mouse-button-callback)
       (glfw:set-mouse-pos-callback 'mouse-pos-callback)
       (setf *queue* nil)
       (print (glfw:get-desktop-mode))
       )



    (incf count)
    ;;(h3d:set-node-transform dynode 0 0 0 0 (mod count 360) 0 1 1 1)
    	  
    ;; do work in queue...
    (setf *old-time* *time*)
    (setf *time* (glfw:get-time))
    
    (setf *delta-time*
	  (when (and *old-time* *time*)
	    (- *time* *old-time*)))
      

    (when *queue*
      (setf *queue* (remove-if-not (lambda (x)
				     (funcall x))
				   *queue*)))
	    
    (when move-d
      (incf dist move-d))

    (h3d:Set-Node-Transform camera 0 0 dist 0 0 0 1 1 1)
    (h3d:set-node-transform hub 0 0 0 0 (funcall moving *time*) 0 1 1 1)
    ;;(h3d:set-node-transform dynode 0 0 4 0 0 0 1 1 1)
    ;(h3d:set-node-transform dynode 0 0 0 0 0 (mod (/ count 5) 360) 1 1 1)
    ;; Set camera parameters
;;_x = 5; _y = 3; _z = 19; _rx = 7; _ry = 15; _velocity

    ;; (let ((tex (h3d:find-resource :texture "models/skybox/skybox.dds")))
    ;;   (print (list tex
    ;; 		   (get-texture-resource-width tex)
    ;; 		   (get-texture-resource-height tex))))
    ;; Render scene
    (h3d:Render camera)

    ;; Finish rendering of frame
    (h3d:Finalize-Frame)

    ;; Write all messages to log file
    (h3d:dump-messages))
  (h3d:release))

      
(defun t-start ()
  (bordeaux-threads:make-thread #'start))