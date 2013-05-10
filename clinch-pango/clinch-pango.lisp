;;;; clinch-pango.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)
(defvar *layout* '*layout*)

(defmacro print-text (text &key (width nil) (wrap :pango_wrap_word) (alignment :PANGO_ALIGN_CENTER))
  "Print a block of text."
  `(with-paragraph (:width ,width :wrap ,wrap :alignment ,alignment)
     (cairo:save)
     (pango:pango_layout_set_markup ,*layout* (xmls:toxml
					      ,text) -1)
    
     (pango:pango_cairo_update_layout (slot-value cairo:*context* 'cairo::pointer) ,*layout*)
     (pango:pango_cairo_show_layout (slot-value cairo:*context* 'cairo::pointer) ,*layout*)
     (cairo:restore)
     (cairo:rel-move-to 0 (nth-value 1 (pango:get-layout-size ,*layout*)))))


(defmacro with-paragraph ((&key (layout *layout*)  (context 'cairo:*context*) (alignment :PANGO_ALIGN_CENTER) width (wrap :pango_wrap_word)) &body body)
  "Create a paragraph of text"
  (let ((gwidth (gensym))
	(gwrap (gensym)))

  `(let ((,layout (pango:pango_cairo_create_layout
		   (slot-value ,context 'cairo::pointer)))
	 (,gwidth (* pango:pango_scale ,width))
	 (,gwrap ,wrap))
     
     (when (and ,gwidth ,gwrap)
       (pango:pango_layout_set_wrap ,layout ,gwrap)
       (pango:pango_layout_set_width ,layout ,gwidth))
     
     (pango:pango_layout_set_alignment ,layout ,alignment)
     (unwind-protect 
	  (progn ,@body)
       (pango:g_object_unref ,layout)))))
