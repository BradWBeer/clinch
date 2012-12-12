;;;; text.lisp

(in-package #:clinch)

(defmacro print-text (text &key (width nil) (wrap :pango_wrap_word))
  `(with-paragraph (:width ,width :wrap ,wrap)
     (save)
     (pango:pango_layout_set_markup *layout* (xmls:toxml
					      ,text) -1)
    
     (pango:pango_cairo_update_layout (slot-value *context* 'pointer) *layout*)
     (pango:pango_cairo_show_layout (slot-value *context* 'pointer) *layout*)
     (restore)
     (cairo:rel-move-to 0 (nth-value 1 (pango:get-layout-size *layout*)))))


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
