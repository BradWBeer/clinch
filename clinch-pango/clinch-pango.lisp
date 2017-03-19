;;;; clinch-pango.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)
(defvar *layout* '*layout*)

(defun get-available-fonts ()
  (map 'list (lambda (x)
	       (cons (pango:pango_font_family_get_name x)
		     (map 'list #'pango:pango_font_face_get_face_name
			  (pango:list-font-faces x))))
       (pango::list-font-families)))

(defun position-text (text &key (width nil) (height nil) (wrap :pango_wrap_word) (alignment :PANGO_ALIGN_CENTER) (%x .5) (%y .5))
  (pango::print-with-markup (text :width width :wrap wrap :alignment alignment)
    (multiple-value-bind (xt yt wt ht) (pango:get-layout-extents)
      (let* ((x-end (- width wt))
             (y-start yt)
             (y-end (- height ht))

             (x-pos (+ (- xt)
                       (* %x x-end)))

             (y-pos (+ (* %y
                          (- y-end y-start))
                       y-start)))
        (cairo:move-to x-pos y-pos)))))


(defmacro print-with-attributes (&rest args)
  `(pango:print-with-attributes ,@args)) 

(defmacro print-text (&rest args)
  `(pango:print-text ,@args)) 

(defmacro print-raw-text (&rest args)
  `(pango:print-raw-text ,@args)) 
