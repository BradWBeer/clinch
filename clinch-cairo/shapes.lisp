;;;; clinch-cairo/shapes.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)


(defun draw-rounded-rectangle (width height
			       &key
				 (context cairo:*context*)
				 (line-width 20)
				 (line '(.1 .1 .1 1))
				 (fill '(0 0 0 0)))
  (let* ((aspect 1.0)
	 (corner-radius (/ height 5))
	 (radius (/ corner-radius aspect)))
    (cairo:with-context (context)
      
      (cairo:save)
      (cairo:new-path)
      (cairo:arc (+ radius line-width)
		 (+ radius line-width)
		 radius
                 (degrees->radians 180)
                 (degrees->radians -90))
      (cairo:arc (- width (+ radius line-width))
                 (+ radius line-width)
                 radius
                 (degrees->radians 270)
                 (degrees->radians 0))

      (cairo:arc (- width (+ radius line-width))
                 (- height (+ radius line-width))
                 radius
                 (degrees->radians 0)
                 (degrees->radians 90))

      (cairo:arc (+ radius line-width)
                 (- height (+ radius line-width))
                 radius
                 (degrees->radians 90)
                 (degrees->radians 180))
      (cairo:close-path)

      (apply #'cairo:set-source-rgba fill)
      (cairo:fill-preserve)

      (apply #'cairo:set-source-rgba line)
      (cairo:set-line-width line-width)

      (cairo:stroke)
      (cairo:restore))))
