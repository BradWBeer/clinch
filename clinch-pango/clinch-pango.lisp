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
        (cairo:move-to x-pos y-pos)
	(values (list xt yt wt ht)
		(list x-pos y-pos))))))

(defmacro print-text (text &key (width nil) (wrap :pango_wrap_word) (alignment :PANGO_ALIGN_LEFT))
  "Print a block of text with markup."
  `(with-paragraph (:width ,width :wrap ,wrap :alignment ,alignment)
     (cairo:save)

     (pango::pango_layout_set_markup ,*layout* (xmls:toxml
					 ,text) -1)
     
     (pango::pango_cairo_update_layout (slot-value cairo:*context* 'cairo::pointer) ,*layout*)
     (pango::pango_cairo_show_layout (slot-value cairo:*context* 'cairo::pointer) ,*layout*)
     (cairo:restore)
     (unless (cairo:has-current-point) (cairo:move-to 0 0))
     (cairo:rel-move-to 0 (nth-value 1 (pango::get-layout-size ,*layout*)))
     (pango::get-layout-extents ,*layout*)))

(defmacro print-text-with-attributes (text attributes &key (width nil) (wrap :pango_wrap_word) (alignment :pango_ALIGN_LEFT))
  "Print a block of text with markup."
  `(with-paragraph (:width ,width :wrap ,wrap :alignment ,alignment)
     (cairo:save)
     
     (pango_layout_set_text *layout* ,text -1)
     (with-attribute-list ()
       (map nil 
	    (lambda (a)
	      (apply
	       (cdr (assoc (car a) *alist-attributes*))
	       (cdr a)))
	    ,attributes)
       
       (pango_layout_set_attributes *layout* *attribute-list*)
       (pango_cairo_update_layout (slot-value cairo:*context* 'cairo::pointer) *layout*)
       
       (pango_cairo_update_layout (slot-value cairo:*context* 'cairo::pointer) ,*layout*)
       (pango_cairo_show_layout (slot-value cairo:*context* 'cairo::pointer) ,*layout*)
       (cairo:restore)
       (unless (cairo:has-current-point) (cairo:move-to 0 0))
       (cairo:rel-move-to 0 (nth-value 1 (get-layout-size ,*layout*)))
       (pango::get-layout-extents ,*layout*))))



(defmacro print-text-with-attributes (&rest args)
  `(pango:print-text-with-attributes ,@args)) 

(defmacro print-text (&rest args)
  `(pango:print-text ,@args)) 

(defmacro print-raw-text (&rest args)
  `(pango:print-raw-text ,@args)) 
