(setf *slides* 
      (list
       (as-slide (i w h)
	 (cairo:move-to 0 200)
	 (pango:print-text '("span" nil
			     ("span" (("font_desc" "Century Schoolbook L Roman bold 150") ("fgcolor" "#FF0000"))
			      "C")
			     ("span" (("font_desc" "Century Schoolbook L Roman bold 150") ("fgcolor" "#00FF00"))
			      "L")
			     ("span" (("font_desc" "Century Schoolbook L Roman bold 150") ("fgcolor" "#0000FF"))
			      "I")
			     ("span" (("font_desc" "Century Schoolbook L Roman bold 150") ("fgcolor" "#FF0000"))
			      "N")
			     ("span" (("font_desc" "Century Schoolbook L Roman bold 150") ("fgcolor" "#00FF00"))
			      "C")
			     ("span" (("font_desc" "Century Schoolbook L Roman bold 150") ("fgcolor" "#0000FF"))
			      "H"))
			   :width w
			   :alignment :pango_align_center))

       (as-slide (i w h)
	 (pango:print-text `("span" nil
				    ("span" (("font_desc" "Century Schoolbook L Roman bold 50"))
					    "Brad Beer")
				    ("span" (("font_desc" "Century Schoolbook L Roman bold 50"))
					    "IRC: warweasle")
				    ("span" (("font_desc" "Century Schoolbook L Roman bold 50"))
					    "Email: warweasle1@gmail.com"))
			   :width 800
			   :alignment :pango_align_center))
       (as-slide (i w h)
	 (pango:print-text `("span" (("font_desc" "Century Schoolbook L Roman bold 100"))
				    "What is CLINCH?")
			   :width 800
			   :alignment :pango_align_left))

       (as-slide (i w h)
	 (pp "What does it do?" w h))

       (as-slide (i w h)
	 (pp "What is CLINCH:" w h)
	 (nl w h)
	 (pp "3D/2D Game Engine" w h))

       (as-slide (i w h)
	 (pp "What is CLINCH:" w h)
	 (nl w h)
	 (pp "3D/2D Productivity Tool?" w h))
       
       (as-slide (i w h)
	 (pp "What does it do:" w h)
	 (nl w h)
	 (cairo:rel-move-to 100 0)
	 (pp "3D Meshes" w h)
	 (nl w h)
	 (pp "2D Images" w h)
	 (nl w h)
	 (pp "2D Vector Graphics" w h)
	 (nl w h)
	 (pp "Rich Text" w h))
       
       (as-slide (i w h)
	 (pp "Purpose:" w h))
       
       (as-slide (i w h)
	 (pp "Purpose:" w h)
	 (nl w h)
	 (pp "Graphics Engine to 3Dmacs/QIX" 700 h))

       (as-slide (i w h)
	 (pp "Reality:" w h)
	 (nl w h)
	 (pp "Game Engine" w h)
	 (nl w h)
	 (pp "" w h)
	 (pango:print-text `("span" (("font_desc" "Century Schoolbook L Roman bold 40"))
				    "Because I got bored and overwhelmed...")
			   :width w
			   :alignment :pango_align_left))

       (as-slide (i w h)
	 (pp "Now:" w h)
	 (nl w h)
	 (pp "Multithreaded Engine to Get Things Running Quickly and Simply." w h))

       (as-slide (i w h)
	 (pp "Now:" w h)
	 (nl w h)
	 (pp "But Still Has Its Roots as a Productivity Tool..." 700 h))

       (as-slide (i w h)
	 (pp "Now:" 700 h)
	 (nl 700 h)
	 (pp "You're Soaking in it Now!" 700 h))

       (as-slide (i w h)
	 (pp "Plugins and Libraries Made to Work With CLINCH:" 700 h))

       (as-slide (i w h)
	 (pp "CLINCH-CAIRO" 700 h)
	 (nl 700 h)
	 (pp "Wrapper Around CL-CAIRO2" 700 h))

       (as-slide (i w h)
	 (pp "CLINCH-PANGO" 700 h)
	 (nl 700 h)
	 (pp "Wraper Around CL-PANGO" 700 h))

       (as-slide (i w h)
	 (pp "CLINCH-FREEIMAGE" 700 h)
	 (nl 700 h)
	 (pp "Wraper Around CL-FREEIMAGE" 700 h)
	 (nl 700 h)
	 (pango:print-text `("span" (("font_desc" "Century Schoolbook L Roman bold 40"))
				    "Yuck!")
			   :width w
			   :alignment :pango_align_left))
       (as-slide (i w h)
	 (pp "CL-SQUIRL" 700 h)
	 (nl 700 h)
	 (pp "2D Library Modified to Work With CLINCH" 700 h))
       
       (as-slide (i w h)
	 (pp "CL-ODE" 700 h)
	 (nl 700 h)
	 (pp "3D Wrapper Which Works Well With CLINCH" 700 h))
       
       (as-slide (i w h)
	 (pp "CLINCH-CLASSIMP" 700 h)
	 (nl 700 h)
	 (pp "Wrapper Around CLASSIMP" 700 h)
	 (nl 700 h)
	 (pango:print-text `("span" (("font_desc" "Century Schoolbook L Roman bold 40"))
				    "Animation not Working...yet.")
			   :width w
			   :alignment :pango_align_left))

     (as-slide (i w h)
	 (pp "Math Libraries" 700 h)
	 (nl 700 h)
	 (pp "There are so many and non have all of what I need..." 700 h)
	 (nl 700 h)
	 (pp "Using RTG-MATH now." 700 h))

     (as-slide (i w h)
       (pp "Path Forward:" 700 h)
       (nl 700 h)
       (pp "Finish Current Libraries & Plugins" 700 h))

     (as-slide (i w h)
       (pp "Path Forward:" 700 h)
       (nl 700 h)
       (pp "3D Animations" 700 h)
       (nl 700 h)
       (pango:print-text `("span" (("font_desc" "Century Schoolbook L Roman bold 40"))
				  "2D Animations Actually Work!")
			 :width w
			 :alignment :pango_align_left))

     (as-slide (i w h)
       (pp "Path Forward:" 700 h)
       (nl 700 h)
       (pp "GUI!" 700 h))

     
     (as-slide (i w h)
       (pp "Examples:" 700 h))

     (as-slide (i w h)
       (pp "Questions?" 700 h))

     (as-slide (i w h)
       (cairo:move-to 0 200)
       (pango:print-text '("span" (("font_desc" "Century Schoolbook L Roman bold 150") ("fgcolor" "#0000FF"))
			   "THANK YOU!")
			   :width w
			   :alignment :pango_align_center))
))

