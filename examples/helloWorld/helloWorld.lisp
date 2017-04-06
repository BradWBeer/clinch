(ql:quickload :clinch)
(ql:quickload :clinch-cairo)                                                                                          
(ql:quickload :clinch-pango)                                                                                          
                                                                                                                      
(use-package :clinch)

(init :init-controllers nil)

(! (fast-draw () 
     (print-text '("span" nil
		   ("span" (("font_desc" "Monospace bold 75") ("fgcolor" "#0000FF"))
		    "Hello World")))))
