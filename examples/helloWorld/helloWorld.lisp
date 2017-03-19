(ql:quickload :clinch)
(ql:quickload :clinch-cairo)                                                                                          
(ql:quickload :clinch-pango)                                                                                          
                                                                                                                      
(use-package :clinch)

(init :init-controllers nil)

(! (fast-draw () 
     (pango:print-text '("span" nil
			 ("span" (("font_desc" "Century Schoolbook L Roman bold 75") ("fgcolor" "#0000FF"))
			  "Hello World")))))
