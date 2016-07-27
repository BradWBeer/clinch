(ql:quickload :clinch)
(ql:quickload :clinch-cairo)
(ql:quickload :clinch-pango)

(use-package :clinch)

(defun draw-ring (r len w h &optional (offset 0))
  (let ((o (* 2 pi offset)))
    (cairo:arc (/ w 2) (/ h 2) r (+ (/ pi -2) o) (+ (/ pi -2)  (* 2 pi len) o))))

(defun draw-clock (hours minutes seconds)
  (let ((s (/ (mod seconds 60) 60))
	(m (/ (mod minutes 60) 60))
	(hr (/ (mod hours 12) 12)))
   
 
    (fast-draw (:width-var w :height-var h)
      (let ((min-wh (min w h)))
      (clear-cairo-context 0.1 0.1 0.1)
      (cairo:set-source-rgb 1 1 1)

      (cairo:save)
      
      (cairo:scale (/ min-wh 600) (/ min-wh 600))
      (if (> hours 11)
      	  (pango:print-text '("span" (("foreground" "midnight blue") ("font" "250")) "PM") :alignment :pango_align_left)
      	  (pango:print-text '("span" (("foreground" "orange") ("font" "250")) "AM") :alignment :pango_align_left))
      (cairo:restore)

      (cairo:new-path)
      (cairo:set-line-width (/ min-wh 10))
      
      (cairo:set-source-rgb .9 0 0)
      (draw-ring (* 3/8 min-wh) hr w h s)
      (cairo:stroke)
      
      (cairo:set-source-rgb 0 .9 0)
      (draw-ring (* 4/16 (min w h)) m w h (* -2 s))
      (cairo:stroke)
      
      (cairo:set-source-rgb 0 0 .9)
      (draw-ring (* 1/8 (min w h)) s w h (* 4 s))
      (cairo:stroke)

      ))))

(clinch:defevent clinch:*on-idle* ()

  (multiple-value-bind (seconds minutes hours) (get-decoded-time)
    (incf seconds (/ (nth-value 1 (get-time-of-day)) 1000000))
    (incf minutes (/ seconds 60))
    (incf hours (/ minutes 120))

    (draw-clock hours minutes seconds))
  
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (clinch:render *root* :projection *projection*)
  (when *entity*
    (clinch:render *entity* :projection clinch::*ortho-projection*)))

(clinch:init :init-controllers nil)
