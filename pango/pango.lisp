(in-package #:pango)

(cffi:defcfun ("g_object_unref" g_object_unref) :void
  (object :pointer))

;;;SWIG wrapper code starts here

(cl:defmacro defanonenum (&body enums)
   "Converts anonymous enums to defconstants."
  `(cl:progn ,@(cl:loop for value in enums
                        for index = 0 then (cl:1+ index)
                        when (cl:listp value) do (cl:setf index (cl:second value)
                                                          value (cl:first value))
                        collect `(cl:defconstant ,value ,index))))

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:unless (cl:fboundp 'swig-lispify)
    (cl:defun swig-lispify (name flag cl:&optional (package cl:*package*))
      (cl:labels ((helper (lst last rest cl:&aux (c (cl:car lst)))
                    (cl:cond
                      ((cl:null lst)
                       rest)
                      ((cl:upper-case-p c)
                       (helper (cl:cdr lst) 'upper
                               (cl:case last
                                 ((lower digit) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:lower-case-p c)
                       (helper (cl:cdr lst) 'lower (cl:cons (cl:char-upcase c) rest)))
                      ((cl:digit-char-p c)
                       (helper (cl:cdr lst) 'digit 
                               (cl:case last
                                 ((upper lower) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:char-equal c #\_)
                       (helper (cl:cdr lst) '_ (cl:cons #\- rest)))
                      (cl:t
                       (cl:error "Invalid character: ~A" c)))))
        (cl:let ((fix (cl:case flag
                        ((constant enumvalue) "+")
                        (variable "*")
                        (cl:t ""))))
          (cl:intern
           (cl:concatenate
            'cl:string
            fix
            (cl:nreverse (helper (cl:concatenate 'cl:list name) cl:nil cl:nil))
            fix)
           package))))))

;;;SWIG wrapper code ends here


(cffi:defcvar ("PangoContext" PangoContext)
 :pointer)

(cffi:defcvar ("PangoItem" PangoItem)
 :pointer)

(cffi:defcvar ("PangoAnalysis" PangoAnalysis)
 :pointer)

(cffi:defcstruct GSList
	(data :pointer)
	(next :pointer))	

(cffi:defcstruct GList
	(data :pointer)
	(next :pointer)
	(prev :pointer))


(cffi:defcstruct PangoLogAttr
	(is_line_break :pointer)
	(is_mandatory_break :pointer)
	(is_char_break :pointer)
	(is_white :pointer)
	(is_cursor_position :pointer)
	(is_word_start :pointer)
	(is_word_end :pointer)
	(is_sentence_boundary :pointer)
	(is_sentence_start :pointer)
	(is_sentence_end :pointer)
	(backspace_deletes_character :pointer)
	(is_expandable_space :pointer)
	(is_word_boundary :pointer))

(cffi:defcstruct PangoLayoutLine
  (layout :pointer)
  (start_index :int)  
  (length :int)	
  (runs :pointer)
  (is_paragraph_start :uint)
  (resolved_dir :uint))

(cffi:defcfun ("pango_itemize" pango_itemize) :pointer
  (context :pointer)
  (text :string)
  (start_index :int)
  (length :int)
  (attrs :pointer)
  (cached_iter :pointer))

(cffi:defcfun ("pango_itemize_with_base_dir" pango_itemize_with_base_dir) :pointer
  (context :pointer)
  (base_dir :pointer)
  (text :string)
  (start_index :int)
  (length :int)
  (attrs :pointer)
  (cached_iter :pointer))

(cffi:defcfun ("pango_item_free" pango_item_free) :void
  (item :pointer))

(cffi:defcfun ("pango_item_copy" pango_item_copy) :pointer
  (item :pointer))

(cffi:defcfun ("pango_item_new" pango_item_new) :pointer)

(cffi:defcfun ("pango_item_split" pango_item_split) :pointer
  (orig :pointer)
  (split_index :int)
  (split_offset :int))

(cffi:defcfun ("pango_reorder_items" pango_reorder_items) :pointer
  (logical_items :pointer))

(cffi:defcfun ("pango_context_new" pango_context_new) :pointer)

(cffi:defcfun ("pango_context_set_font_map" pango_context_set_font_map) :void
  (context :pointer)
  (font_map :pointer))

(cffi:defcfun ("pango_context_get_font_map" pango_context_get_font_map) :pointer
  (context :pointer))

(cffi:defcfun ("pango_context_get_font_description" pango_context_get_font_description) :pointer
  (context :pointer))

(cffi:defcfun ("pango_context_set_font_description" pango_context_set_font_description) :void
  (context :pointer)
  (desc :pointer))

(cffi:defcfun ("pango_context_get_language" pango_context_get_language) :pointer
  (context :pointer))

(cffi:defcfun ("pango_context_set_language" pango_context_set_language) :void
  (context :pointer)
  (language :pointer))

(cffi:defcfun ("pango_context_get_base_dir" pango_context_get_base_dir) :pointer
  (context :pointer))

(cffi:defcfun ("pango_context_set_base_dir" pango_context_set_base_dir) :void
  (context :pointer)
  (direction :pointer))

(cffi:defcfun ("pango_context_get_base_gravity" pango_context_get_base_gravity) :pointer
  (context :pointer))

(cffi:defcfun ("pango_context_set_base_gravity" pango_context_set_base_gravity) :void
  (context :pointer)
  (gravity :pointer))

(cffi:defcfun ("pango_context_get_gravity" pango_context_get_gravity) :pointer
  (context :pointer))

(cffi:defcfun ("pango_context_get_gravity_hint" pango_context_get_gravity_hint) :pointer
  (context :pointer))

(cffi:defcfun ("pango_context_set_gravity_hint" pango_context_set_gravity_hint) :void
  (context :pointer)
  (hint :pointer))

(cffi:defcfun ("pango_context_get_matrix" pango_context_get_matrix) :pointer
  (context :pointer))

(cffi:defcfun ("pango_context_set_matrix" pango_context_set_matrix) :void
  (context :pointer)
  (matrix :pointer))

(cffi:defcfun ("pango_context_load_font" pango_context_load_font) :pointer
  (context :pointer)
  (desc :pointer))

(cffi:defcfun ("pango_context_load_fontset" pango_context_load_fontset) :pointer
  (context :pointer)
  (desc :pointer)
  (language :pointer))

(cffi:defcfun ("pango_context_get_metrics" pango_context_get_metrics) :pointer
  (context :pointer)
  (desc :pointer)
  (language :pointer))

(cffi:defcfun ("pango_context_list_families" pango_context_list_families) :void
  (context :pointer)
  (families :pointer)
  (n_families :pointer))

(cffi:defcfun ("pango_break" pango_break) :void
  (text :pointer)
  (length :int)
  (analysis :pointer)
  (attrs :pointer)
  (attrs_len :int))

(cffi:defcfun ("pango_get_log_attrs" pango_get_log_attrs) :void
  (text :string)
  (length :int)
  (level :int)
  (language :pointer)
  (log_attrs :pointer)
  (attrs_len :int))

(cffi:defcfun ("pango_find_paragraph_boundary" pango_find_paragraph_boundary) :void
  (text :pointer)
  (length :int)
  (paragraph_delimiter_index :pointer)
  (next_paragraph_start :pointer))

(cffi:defcfun ("pango_default_break" pango_default_break) :void
  (text :pointer)
  (length :int)
  (analysis :pointer)
  (attrs :pointer)
  (attrs_len :int))

(cffi:defcfun ("pango_shape" pango_shape) :void
  (text :pointer)
  (length :int)
  (analysis :pointer)
  (glyphs :pointer))

(cl:defconstant PANGO_SCALE 1024)

(cffi:defcfun ("pango_units_to_double" pango_units_to_double) :double
  (i :int))

(cffi:defcfun ("pango_units_from_double" pango_units_from_double) :int
  (d :double))

(cffi:defcstruct PangoRectangle
	(x :int)
	(y :int)
	(width :int)
	(height :int))

(cffi:defcfun ("pango_extents_to_pixels" pango_extents_to_pixels) :void
  (inclusive :pointer)
  (nearest :pointer))

(cffi:defcstruct PangoMatrix
	(xx :double)
	(xy :double)
	(yx :double)
	(yy :double)
	(x0 :double)
	(y0 :double))

(cffi:defcfun ("pango_matrix_copy" pango_matrix_copy) :pointer
  (matrix :pointer))

(cffi:defcfun ("pango_matrix_free" pango_matrix_free) :void
  (matrix :pointer))

(cffi:defcfun ("pango_matrix_translate" pango_matrix_translate) :void
  (matrix :pointer)
  (tx :double)
  (ty :double))

(cffi:defcfun ("pango_matrix_scale" pango_matrix_scale) :void
  (matrix :pointer)
  (scale_x :double)
  (scale_y :double))

(cffi:defcfun ("pango_matrix_rotate" pango_matrix_rotate) :void
  (matrix :pointer)
  (degrees :double))

(cffi:defcfun ("pango_matrix_concat" pango_matrix_concat) :void
  (matrix :pointer)
  (new_matrix :pointer))

(cffi:defcfun ("pango_matrix_transform_point" pango_matrix_transform_point) :void
  (matrix :pointer)
  (x :pointer)
  (y :pointer))

(cffi:defcfun ("pango_matrix_transform_distance" pango_matrix_transform_distance) :void
  (matrix :pointer)
  (dx :pointer)
  (dy :pointer))

(cffi:defcfun ("pango_matrix_transform_rectangle" pango_matrix_transform_rectangle) :void
  (matrix :pointer)
  (rect :pointer))

(cffi:defcfun ("pango_matrix_transform_pixel_rectangle" pango_matrix_transform_pixel_rectangle) :void
  (matrix :pointer)
  (rect :pointer))

(cffi:defcfun ("pango_matrix_get_font_scale_factor" pango_matrix_get_font_scale_factor) :double
  (matrix :pointer))

(cffi:defcvar ("PangoGlyph" PangoGlyph)
 :unsigned-int)

(cffi:defcstruct PangoGlyphInfo
	(glyph :pointer)
	(geometry :pointer)
	(attr :pointer))

(cffi:defcstruct PangoGlyphGeometry
	(width :pointer)
	(x_offset :pointer)
	(y_offset :pointer))

(cffi:defcvar ("PangoGlyphUnit" PangoGlyphUnit)
 :int)

(cffi:defcstruct PangoGlyphVisAttr
	(is_cluster_start :pointer))

(cffi:defcstruct PangoGlyphString
	(num_glyphs :int)
	(glyphs :pointer)
	(log_clusters :pointer))

(cffi:defcstruct PangoGlyphItemIter
	(glyph_item :pointer)
	(text :pointer)
	(start_glyph :int)
	(start_index :int)
	(start_char :int)
	(end_glyph :int)
	(end_index :int)
	(end_char :int))

(cffi:defcfun ("pango_glyph_string_new" pango_glyph_string_new) :pointer)

(cffi:defcfun ("pango_glyph_string_copy" pango_glyph_string_copy) :pointer
  (string :pointer))

(cffi:defcfun ("pango_glyph_string_set_size" pango_glyph_string_set_size) :void
  (string :pointer)
  (new_len :int))

(cffi:defcfun ("pango_glyph_string_free" pango_glyph_string_free) :void
  (string :pointer))

(cffi:defcfun ("pango_glyph_string_extents" pango_glyph_string_extents) :void
  (glyphs :pointer)
  (font :pointer)
  (ink_rect :pointer)
  (logical_rect :pointer))

(cffi:defcfun ("pango_glyph_string_extents_range" pango_glyph_string_extents_range) :void
  (glyphs :pointer)
  (start :int)
  (end :int)
  (font :pointer)
  (ink_rect :pointer)
  (logical_rect :pointer))

(cffi:defcfun ("pango_glyph_string_get_width" pango_glyph_string_get_width) :int
  (glyphs :pointer))

(cffi:defcfun ("pango_glyph_string_index_to_x" pango_glyph_string_index_to_x) :void
  (glyphs :pointer)
  (text :string)
  (length :int)
  (analysis :pointer)
  (index_ :int)
  (trailing :int)
  (x_pos :pointer))

(cffi:defcfun ("pango_glyph_string_x_to_index" pango_glyph_string_x_to_index) :void
  (glyphs :pointer)
  (text :string)
  (length :int)
  (analysis :pointer)
  (x_pos :int)
  (index_ :pointer)
  (trailing :pointer))

(cffi:defcfun ("pango_glyph_string_get_logical_widths" pango_glyph_string_get_logical_widths) :void
  (glyphs :pointer)
  (text :string)
  (length :int)
  (embedding_level :int)
  (logical_widths :pointer))

(cffi:defcfun ("pango_glyph_item_copy" pango_glyph_item_copy) :pointer
  (orig :pointer))

(cffi:defcfun ("pango_glyph_item_free" pango_glyph_item_free) :void
  (glyph_item :pointer))

(cffi:defcfun ("pango_glyph_item_split" pango_glyph_item_split) :pointer
  (orig :pointer)
  (text :string)
  (split_index :int))

(cffi:defcfun ("pango_glyph_item_apply_attrs" pango_glyph_item_apply_attrs) :pointer
  (glyph_item :pointer)
  (text :string)
  (list :pointer))

(cffi:defcfun ("pango_glyph_item_letter_space" pango_glyph_item_letter_space) :void
  (glyph_item :pointer)
  (text :string)
  (log_attrs :pointer)
  (letter_spacing :int))

(cffi:defcfun ("pango_glyph_item_get_logical_widths" pango_glyph_item_get_logical_widths) :void
  (glyph_item :pointer)
  (text :string)
  (logical_widths :pointer))

(cffi:defcfun ("pango_glyph_item_iter_copy" pango_glyph_item_iter_copy) :pointer
  (orig :pointer))

(cffi:defcfun ("pango_glyph_item_iter_free" pango_glyph_item_iter_free) :void
  (iter :pointer))

(cffi:defcfun ("pango_glyph_item_iter_init_start" pango_glyph_item_iter_init_start) :int
  (iter :pointer)
  (glyph_item :pointer)
  (text :string))

(cffi:defcfun ("pango_glyph_item_iter_init_end" pango_glyph_item_iter_init_end) :int
  (iter :pointer)
  (glyph_item :pointer)
  (text :string))

(cffi:defcfun ("pango_glyph_item_iter_next_cluster" pango_glyph_item_iter_next_cluster) :int
  (iter :pointer))

(cffi:defcfun ("pango_glyph_item_iter_prev_cluster" pango_glyph_item_iter_prev_cluster) :int
  (iter :pointer))

(cffi:defcvar ("PangoFontDescription" PangoFontDescription)
 :pointer)

(cffi:defcenum PangoStyle
	:PANGO_STYLE_NORMAL
	:PANGO_STYLE_OBLIQUE
	:PANGO_STYLE_ITALIC)

(cffi:defcenum PangoWeight
	(:PANGO_WEIGHT_THIN 100)
	(:PANGO_WEIGHT_ULTRALIGHT 200)
	(:PANGO_WEIGHT_LIGHT 300)
	(:PANGO_WEIGHT_BOOK 380)
	(:PANGO_WEIGHT_NORMAL 400)
	(:PANGO_WEIGHT_MEDIUM 500)
	(:PANGO_WEIGHT_SEMIBOLD 600)
	(:PANGO_WEIGHT_BOLD 700)
	(:PANGO_WEIGHT_ULTRABOLD 800)
	(:PANGO_WEIGHT_HEAVY 900)
	(:PANGO_WEIGHT_ULTRAHEAVY 1000))

(cffi:defcenum PangoVariant
	:PANGO_VARIANT_NORMAL
	:PANGO_VARIANT_SMALL_CAPS)

(cffi:defcenum PangoStretch
	:PANGO_STRETCH_ULTRA_CONDENSED
	:PANGO_STRETCH_EXTRA_CONDENSED
	:PANGO_STRETCH_CONDENSED
	:PANGO_STRETCH_SEMI_CONDENSED
	:PANGO_STRETCH_NORMAL
	:PANGO_STRETCH_SEMI_EXPANDED
	:PANGO_STRETCH_EXPANDED
	:PANGO_STRETCH_EXTRA_EXPANDED
	:PANGO_STRETCH_ULTRA_EXPANDED)

(cffi:defcenum PangoFontMask
	(:PANGO_FONT_MASK_FAMILY 1)
	(:PANGO_FONT_MASK_STYLE 2)
	(:PANGO_FONT_MASK_VARIANT 4)
	(:PANGO_FONT_MASK_WEIGHT 8)
	(:PANGO_FONT_MASK_STRETCH 16)
	(:PANGO_FONT_MASK_SIZE 32)
	(:PANGO_FONT_MASK_GRAVITY 64))

(cffi:defcstruct PangoFontMetrics
	(ref_count :pointer)
	(ascent :int)
	(descent :int)
	(approximate_char_width :int)
	(approximate_digit_width :int)
	(underline_position :int)
	(underline_thickness :int)
	(strikethrough_position :int)
	(strikethrough_thickness :int))

(cffi:defcstruct PangoFontsetClass
	(parent_class :pointer)
	(get_font :pointer)
	(get_metrics :pointer)
	(get_language :pointer)
	(foreach :pointer))

(cffi:defcstruct PangoFontMapClass
	(parent_class :pointer)
	(load_font :pointer)
	(list_families :pointer)
	(load_fontset :pointer)
	(shape_engine_type :string))

(cffi:defcfun ("pango_font_description_new" pango_font_description_new) :pointer)

(cffi:defcfun ("pango_font_description_copy" pango_font_description_copy) :pointer
  (desc :pointer))

(cffi:defcfun ("pango_font_description_copy_static" pango_font_description_copy_static) :pointer
  (desc :pointer))

(cffi:defcfun ("pango_font_description_hash" pango_font_description_hash) :pointer
  (desc :pointer))

(cffi:defcfun ("pango_font_description_equal" pango_font_description_equal) :int
  (desc1 :pointer)
  (desc2 :pointer))

(cffi:defcfun ("pango_font_description_free" pango_font_description_free) :void
  (desc :pointer))

(cffi:defcfun ("pango_font_descriptions_free" pango_font_descriptions_free) :void
  (descs :pointer)
  (n_descs :int))

(cffi:defcfun ("pango_font_description_set_family" pango_font_description_set_family) :void
  (desc :pointer)
  (family :string))

(cffi:defcfun ("pango_font_description_set_family_static" pango_font_description_set_family_static) :void
  (desc :pointer)
  (family :string))

(cffi:defcfun ("pango_font_description_get_family" pango_font_description_get_family) :string
  (desc :pointer))

(cffi:defcfun ("pango_font_description_set_style" pango_font_description_set_style) :void
  (desc :pointer)
  (style PangoStyle))

(cffi:defcfun ("pango_font_description_get_style" pango_font_description_get_style) PangoStyle
  (desc :pointer))

(cffi:defcfun ("pango_font_description_set_variant" pango_font_description_set_variant) :void
  (desc :pointer)
  (variant PangoVariant))

(cffi:defcfun ("pango_font_description_get_variant" pango_font_description_get_variant) PangoVariant
  (desc :pointer))

(cffi:defcfun ("pango_font_description_set_weight" pango_font_description_set_weight) :void
  (desc :pointer)
  (weight PangoWeight))

(cffi:defcfun ("pango_font_description_get_weight" pango_font_description_get_weight) PangoWeight
  (desc :pointer))

(cffi:defcfun ("pango_font_description_set_stretch" pango_font_description_set_stretch) :void
  (desc :pointer)
  (stretch PangoStretch))

(cffi:defcfun ("pango_font_description_get_stretch" pango_font_description_get_stretch) PangoStretch
  (desc :pointer))

(cffi:defcfun ("pango_font_description_set_size" pango_font_description_set_size) :void
  (desc :pointer)
  (size :int))

(cffi:defcfun ("pango_font_description_get_size" pango_font_description_get_size) :int
  (desc :pointer))

(cffi:defcfun ("pango_font_description_set_absolute_size" pango_font_description_set_absolute_size) :void
  (desc :pointer)
  (size :double))

(cffi:defcfun ("pango_font_description_get_size_is_absolute" pango_font_description_get_size_is_absolute) :int
  (desc :pointer))

(cffi:defcfun ("pango_font_description_set_gravity" pango_font_description_set_gravity) :void
  (desc :pointer)
  (gravity :pointer))

(cffi:defcfun ("pango_font_description_get_gravity" pango_font_description_get_gravity) :pointer
  (desc :pointer))

(cffi:defcfun ("pango_font_description_get_set_fields" pango_font_description_get_set_fields) PangoFontMask
  (desc :pointer))

(cffi:defcfun ("pango_font_description_unset_fields" pango_font_description_unset_fields) :void
  (desc :pointer)
  (to_unset PangoFontMask))

(cffi:defcfun ("pango_font_description_merge" pango_font_description_merge) :void
  (desc :pointer)
  (desc_to_merge :pointer)
  (replace_existing :int))

(cffi:defcfun ("pango_font_description_merge_static" pango_font_description_merge_static) :void
  (desc :pointer)
  (desc_to_merge :pointer)
  (replace_existing :int))

(cffi:defcfun ("pango_font_description_better_match" pango_font_description_better_match) :int
  (desc :pointer)
  (old_match :pointer)
  (new_match :pointer))

(cffi:defcfun ("pango_font_description_from_string" pango_font_description_from_string) :pointer
  (str :string))

(cffi:defcfun ("pango_font_description_to_string" pango_font_description_to_string) :string
  (desc :pointer))

(cffi:defcfun ("pango_font_description_to_filename" pango_font_description_to_filename) :string
  (desc :pointer))

(cffi:defcfun ("pango_font_metrics_ref" pango_font_metrics_ref) :pointer
  (metrics :pointer))

(cffi:defcfun ("pango_font_metrics_unref" pango_font_metrics_unref) :void
  (metrics :pointer))

(cffi:defcfun ("pango_font_metrics_get_ascent" pango_font_metrics_get_ascent) :int
  (metrics :pointer))

(cffi:defcfun ("pango_font_metrics_get_descent" pango_font_metrics_get_descent) :int
  (metrics :pointer))

(cffi:defcfun ("pango_font_metrics_get_approximate_char_width" pango_font_metrics_get_approximate_char_width) :int
  (metrics :pointer))

(cffi:defcfun ("pango_font_metrics_get_approximate_digit_width" pango_font_metrics_get_approximate_digit_width) :int
  (metrics :pointer))

(cffi:defcfun ("pango_font_metrics_get_underline_thickness" pango_font_metrics_get_underline_thickness) :int
  (metrics :pointer))

(cffi:defcfun ("pango_font_metrics_get_underline_position" pango_font_metrics_get_underline_position) :int
  (metrics :pointer))

(cffi:defcfun ("pango_font_metrics_get_strikethrough_thickness" pango_font_metrics_get_strikethrough_thickness) :int
  (metrics :pointer))

(cffi:defcfun ("pango_font_metrics_get_strikethrough_position" pango_font_metrics_get_strikethrough_position) :int
  (metrics :pointer))

(cffi:defcvar ("PangoFont" PangoFont)
 :pointer)

(cffi:defcfun ("pango_font_find_shaper" pango_font_find_shaper) :pointer
  (font :pointer)
  (language :pointer)
  (ch :pointer))

(cffi:defcfun ("pango_font_describe" pango_font_describe) :pointer
  (font :pointer))

(cffi:defcfun ("pango_font_describe_with_absolute_size" pango_font_describe_with_absolute_size) :pointer
  (font :pointer))

(cffi:defcfun ("pango_font_get_coverage" pango_font_get_coverage) :pointer
  (font :pointer)
  (language :pointer))

(cffi:defcfun ("pango_font_get_glyph_extents" pango_font_get_glyph_extents) :void
  (font :pointer)
  (glyph :pointer)
  (ink_rect :pointer)
  (logical_rect :pointer))

(cffi:defcfun ("pango_font_get_metrics" pango_font_get_metrics) :pointer
  (font :pointer)
  (language :pointer))

(cffi:defcfun ("pango_font_get_font_map" pango_font_get_font_map) :pointer
  (font :pointer))

(cffi:defcvar ("PangoFontFamily" PangoFontFamily)
 :pointer)

(cffi:defcfun ("pango_font_family_get_name" pango_font_family_get_name) :string
  (family :pointer))

(cffi:defcfun ("pango_font_family_is_monospace" pango_font_family_is_monospace) :int
  (family :pointer))

(cffi:defcfun ("pango_font_family_list_faces" pango_font_family_list_faces) :void
  (family :pointer)
  (faces :pointer)
  (n_faces :pointer))

(cffi:defcvar ("PangoFontFace" PangoFontFace)
 :pointer)

(cffi:defcfun ("pango_font_face_get_face_name" pango_font_face_get_face_name) :string
  (face :pointer))

(cffi:defcfun ("pango_font_face_list_sizes" pango_font_face_list_sizes) :void
  (face :pointer)
  (sizes :pointer)
  (n_sizes :pointer))

(cffi:defcfun ("pango_font_face_describe" pango_font_face_describe) :pointer
  (face :pointer))

(cffi:defcfun ("pango_font_face_is_synthesized" pango_font_face_is_synthesized) :int
  (face :pointer))

(cffi:defcvar ("PangoFontMap" PangoFontMap)
 :pointer)

(cffi:defcfun ("pango_font_map_create_context" pango_font_map_create_context) :pointer
  (fontmap :pointer))

(cffi:defcfun ("pango_font_map_load_font" pango_font_map_load_font) :pointer
  (fontmap :pointer)
  (context :pointer)
  (desc :pointer))

(cffi:defcfun ("pango_font_map_load_fontset" pango_font_map_load_fontset) :pointer
  (fontmap :pointer)
  (context :pointer)
  (desc :pointer)
  (language :pointer))

(cffi:defcfun ("pango_font_map_list_families" pango_font_map_list_families) :void
  (fontmap :pointer)
  (families :pointer)
  (n_families :pointer))

(cffi:defcfun ("pango_font_map_get_shape_engine_type" pango_font_map_get_shape_engine_type) :string
  (fontmap :pointer))

(cffi:defcvar ("PangoFontset" PangoFontset)
 :pointer)

(cffi:defcfun ("pango_fontset_get_font" pango_fontset_get_font) :pointer
  (fontset :pointer)
  (wc :pointer))

(cffi:defcfun ("pango_fontset_get_metrics" pango_fontset_get_metrics) :pointer
  (fontset :pointer))

(cffi:defcvar ("PangoFontsetForeachFunc" PangoFontsetForeachFunc)
 :pointer)

(cffi:defcfun ("pango_fontset_foreach" pango_fontset_foreach) :void
  (fontset :pointer)
  (func :pointer)
  (data :pointer))

(cffi:defcvar ("PangoFontsetSimple" PangoFontsetSimple)
 :pointer)

(cffi:defcfun ("pango_fontset_simple_new" pango_fontset_simple_new) :pointer
  (language :pointer))

(cffi:defcfun ("pango_fontset_simple_append" pango_fontset_simple_append) :void
  (fontset :pointer)
  (font :pointer))

(cffi:defcfun ("pango_fontset_simple_size" pango_fontset_simple_size) :int
  (fontset :pointer))

(cffi:defcenum PangoAttrType
	:PANGO_ATTR_INVALID
	:PANGO_ATTR_LANGUAGE
	:PANGO_ATTR_FAMILY
	:PANGO_ATTR_STYLE
	:PANGO_ATTR_WEIGHT
	:PANGO_ATTR_VARIANT
	:PANGO_ATTR_STRETCH
	:PANGO_ATTR_SIZE
	:PANGO_ATTR_FONT_DESC
	:PANGO_ATTR_FOREGROUND
	:PANGO_ATTR_BACKGROUND
	:PANGO_ATTR_UNDERLINE
	:PANGO_ATTR_STRIKETHROUGH
	:PANGO_ATTR_RISE
	:PANGO_ATTR_SHAPE
	:PANGO_ATTR_SCALE
	:PANGO_ATTR_FALLBACK
	:PANGO_ATTR_LETTER_SPACING
	:PANGO_ATTR_UNDERLINE_COLOR
	:PANGO_ATTR_STRIKETHROUGH_COLOR
	:PANGO_ATTR_ABSOLUTE_SIZE
	:PANGO_ATTR_GRAVITY
	:PANGO_ATTR_GRAVITY_HINT)

(cffi:defcenum PangoUnderline
	:PANGO_UNDERLINE_NONE
	:PANGO_UNDERLINE_SINGLE
	:PANGO_UNDERLINE_DOUBLE
	:PANGO_UNDERLINE_LOW
	:PANGO_UNDERLINE_ERROR)

(cffi:defcstruct PangoAttrClass
	(type PangoAttrType)
	(copy :pointer)
	(destroy :pointer)
	(equal :pointer))

(cffi:defcstruct PangoAttribute
	(klass :pointer)
	(start_index :uint)
	(end_index :uint))

(cl:defconstant PANGO_ATTR_INDEX_FROM_TEXT_BEGINNING 0)
(cl:defconstant PANGO_ATTR_INDEX_TO_TEXT_END -1)

(cffi:defcstruct PangoAttrString
	(attr PangoAttribute)
	(value :string))

(cffi:defcstruct PangoAttrLanguage
	(attr PangoAttribute)
	(value :pointer))

(cffi:defcstruct PangoAttrColor
	(attr PangoAttribute)
	(color :pointer))

(cffi:defcstruct PangoAttrInt
	(attr PangoAttribute)
	(value :int))

(cffi:defcstruct PangoAttrFloat
	(attr PangoAttribute)
	(value :double))

(cffi:defcstruct PangoAttrFontDesc
	(attr PangoAttribute)
	(desc :pointer))

(cffi:defcstruct PangoAttrShape
	(attr PangoAttribute)
	(ink_rect PangoRectangle)
	(logical_rect PangoRectangle)
	(data :pointer)
	(copy_func :pointer)
	(destroy_func :pointer))

(cffi:defcstruct PangoAttrSize
	(attr PangoAttribute)
	(size :int)
	(absolute :pointer))

(cffi:defcstruct PangoColor
	(red :pointer)
	(green :pointer)
	(blue :pointer))

(cffi:defcfun ("pango_parse_markup" pango_parse_markup) :int
  (markup_text :string)
  (length :int)
  (accel_marker :pointer)
  (attr_list :pointer)
  (text :pointer)
  (accel_char :pointer)
  (error :pointer))

(cffi:defcfun ("pango_attr_type_register" pango_attr_type_register) PangoAttrType
  (name :pointer))

(cffi:defcfun ("pango_attr_type_get_name" pango_attr_type_get_name) :string
  (type PangoAttrType))

(cffi:defcfun ("pango_attribute_init" pango_attribute_init) :void
  (attr :pointer)
  (klass :pointer))

(cffi:defcfun ("pango_attribute_copy" pango_attribute_copy) :pointer
  (attr :pointer))

(cffi:defcfun ("pango_attribute_equal" pango_attribute_equal) :int
  (attr1 :pointer)
  (attr2 :pointer))

(cffi:defcfun ("pango_attribute_destroy" pango_attribute_destroy) :void
  (attr :pointer))

(cffi:defcfun ("pango_attr_language_new" pango_attr_language_new) :pointer
  (language :pointer))

(cffi:defcfun ("pango_attr_family_new" pango_attr_family_new) :pointer
  (family :string))

(cffi:defcfun ("pango_attr_style_new" pango_attr_style_new) :pointer
  (style PangoStyle))

(cffi:defcfun ("pango_attr_variant_new" pango_attr_variant_new) :pointer
  (variant PangoVariant))

(cffi:defcfun ("pango_attr_stretch_new" pango_attr_stretch_new) :pointer
  (stretch PangoStretch))

(cffi:defcfun ("pango_attr_weight_new" pango_attr_weight_new) :pointer
  (weight PangoWeight))

(cffi:defcfun ("pango_attr_size_new" pango_attr_size_new) :pointer
  (size :int))

(cffi:defcfun ("pango_attr_size_new_absolute" pango_attr_size_new_absolute) :pointer
  (size :int))

(cffi:defcfun ("pango_attr_font_desc_new" pango_attr_font_desc_new) :pointer
  (desc :pointer))

(cffi:defcfun ("pango_attr_foreground_new" pango_attr_foreground_new) :pointer
  (red :uint)
  (green :uint)
  (blue :uint))

(cffi:defcfun ("pango_attr_background_new" pango_attr_background_new) :pointer
  (red :uint)
  (green :uint)
  (blue :uint))

(cffi:defcfun ("pango_attr_strikethrough_new" pango_attr_strikethrough_new) :pointer
  (strikethrough :int))

(cffi:defcfun ("pango_attr_strikethrough_color_new" pango_attr_strikethrough_color_new) :pointer
  (red :uint)
  (green :uint)
  (blue :uint))

(cffi:defcfun ("pango_attr_underline_new" pango_attr_underline_new) :pointer
  (underline PangoUnderline))

(cffi:defcfun ("pango_attr_underline_color_new" pango_attr_underline_color_new) :pointer
  (red :uint)
  (green :uint)
  (blue :uint))

(cffi:defcfun ("pango_attr_shape_new" pango_attr_shape_new) :pointer
  (ink_rect :pointer)
  (logical_rect :pointer))

(cffi:defcfun ("pango_attr_shape_new_with_data" pango_attr_shape_new_with_data) :pointer
  (ink_rect :pointer)
  (logical_rect :pointer)
  (data :pointer)
  (copy_func :pointer)
  (destroy_func :pointer))

(cffi:defcvar ("PangoAttrDataCopyFunc" PangoAttrDataCopyFunc)
 :pointer)

(cffi:defcfun ("pango_attr_scale_new" pango_attr_scale_new) :pointer
  (scale_factor :double))

(cl:defconstant PANGO_SCALE_XX_SMALL 0.5787037037037d0)

(cl:defconstant PANGO_SCALE_X_SMALL 0.6444444444444d0)

(cl:defconstant PANGO_SCALE_SMALL 0.8333333333333d0)

(cl:defconstant PANGO_SCALE_MEDIUM 1.0d0)

(cl:defconstant PANGO_SCALE_LARGE 1.2d0)

(cl:defconstant PANGO_SCALE_X_LARGE 1.4399999999999d0)

(cl:defconstant PANGO_SCALE_XX_LARGE 1.728d0)

(cffi:defcfun ("pango_attr_rise_new" pango_attr_rise_new) :pointer
  (rise :int))

(cffi:defcfun ("pango_attr_letter_spacing_new" pango_attr_letter_spacing_new) :pointer
  (letter_spacing :int))

(cffi:defcfun ("pango_attr_fallback_new" pango_attr_fallback_new) :pointer
  (enable_fallback :int))

(cffi:defcfun ("pango_attr_gravity_new" pango_attr_gravity_new) :pointer
  (gravity :pointer))

(cffi:defcfun ("pango_attr_gravity_hint_new" pango_attr_gravity_hint_new) :pointer
  (hint :pointer))

(cffi:defcfun ("pango_color_parse" pango_color_parse) :int
  (color :pointer)
  (spec :string))

(cffi:defcfun ("pango_color_copy" pango_color_copy) :pointer
  (src :pointer))

(cffi:defcfun ("pango_color_free" pango_color_free) :void
  (color :pointer))

(cffi:defcfun ("pango_color_to_string" pango_color_to_string) :pointer
  (color :pointer))

(cffi:defcvar ("PangoAttrList" PangoAttrList)
 :pointer)

(cffi:defcfun ("pango_attr_list_new" pango_attr_list_new) :pointer)

(cffi:defcfun ("pango_attr_list_ref" pango_attr_list_ref) :pointer
  (list :pointer))

(cffi:defcfun ("pango_attr_list_unref" pango_attr_list_unref) :void
  (list :pointer))

(cffi:defcfun ("pango_attr_list_copy" pango_attr_list_copy) :pointer
  (list :pointer))

(cffi:defcfun ("pango_attr_list_insert" pango_attr_list_insert) :void
  (list :pointer)
  (attr :pointer))

(cffi:defcfun ("pango_attr_list_insert_before" pango_attr_list_insert_before) :void
  (list :pointer)
  (attr :pointer))

(cffi:defcfun ("pango_attr_list_change" pango_attr_list_change) :void
  (list :pointer)
  (attr :pointer))

(cffi:defcfun ("pango_attr_list_splice" pango_attr_list_splice) :void
  (list :pointer)
  (other :pointer)
  (pos :int)
  (len :int))

(cffi:defcfun ("pango_attr_list_filter" pango_attr_list_filter) :pointer
  (list :pointer)
  (func :pointer)
  (data :pointer))

(cffi:defcvar ("PangoAttrFilterFunc" PangoAttrFilterFunc)
 :pointer)

(cffi:defcfun ("pango_attr_list_get_iterator" pango_attr_list_get_iterator) :pointer
  (list :pointer))

(cffi:defcvar ("PangoAttrIterator" PangoAttrIterator)
 :pointer)

(cffi:defcfun ("pango_attr_iterator_copy" pango_attr_iterator_copy) :pointer
  (iterator :pointer))

(cffi:defcfun ("pango_attr_iterator_next" pango_attr_iterator_next) :int
  (iterator :pointer))

(cffi:defcfun ("pango_attr_iterator_range" pango_attr_iterator_range) :void
  (iterator :pointer)
  (start :pointer)
  (end :pointer))

(cffi:defcfun ("pango_attr_iterator_get" pango_attr_iterator_get) :pointer
  (iterator :pointer)
  (type PangoAttrType))

(cffi:defcfun ("pango_attr_iterator_get_font" pango_attr_iterator_get_font) :void
  (iterator :pointer)
  (desc :pointer)
  (language :pointer)
  (extra_attrs :pointer))

(cffi:defcfun ("pango_attr_iterator_get_attrs" pango_attr_iterator_get_attrs) :pointer
  (iterator :pointer))

(cffi:defcfun ("pango_attr_iterator_destroy" pango_attr_iterator_destroy) :void
  (iterator :pointer))

(cffi:defcvar ("PangoTabArray" PangoTabArray)
 :pointer)

(cffi:defcenum PangoTabAlign
	:PANGO_TAB_LEFT)

(cffi:defcfun ("pango_tab_array_new" pango_tab_array_new) :pointer
  (initial_size :int)
  (positions_in_pixels :int))

(cffi:defcfun ("pango_tab_array_new_with_positions" pango_tab_array_new_with_positions) :pointer
  (size :int)
  (positions_in_pixels :int)
  (first_alignment PangoTabAlign)
  (first_position :int)
  &rest)

(cffi:defcfun ("pango_tab_array_copy" pango_tab_array_copy) :pointer
  (src :pointer))

(cffi:defcfun ("pango_tab_array_free" pango_tab_array_free) :void
  (tab_array :pointer))

(cffi:defcfun ("pango_tab_array_get_size" pango_tab_array_get_size) :int
  (tab_array :pointer))

(cffi:defcfun ("pango_tab_array_resize" pango_tab_array_resize) :void
  (tab_array :pointer)
  (new_size :int))

(cffi:defcfun ("pango_tab_array_set_tab" pango_tab_array_set_tab) :void
  (tab_array :pointer)
  (tab_index :int)
  (alignment PangoTabAlign)
  (location :int))

(cffi:defcfun ("pango_tab_array_get_tab" pango_tab_array_get_tab) :void
  (tab_array :pointer)
  (tab_index :int)
  (alignment :pointer)
  (location :pointer))

(cffi:defcfun ("pango_tab_array_get_tabs" pango_tab_array_get_tabs) :void
  (tab_array :pointer)
  (alignments :pointer)
  (locations :pointer))

(cffi:defcfun ("pango_tab_array_get_positions_in_pixels" pango_tab_array_get_positions_in_pixels) :int
  (tab_array :pointer))

(cffi:defcvar ("PangoLayout" PangoLayout)
 :pointer)

(cffi:defcvar ("PangoLayoutIter" PangoLayoutIter)
 :pointer)

(cffi:defcenum PangoWrapMode
	:PANGO_WRAP_WORD
	:PANGO_WRAP_CHAR
	:PANGO_WRAP_WORD_CHAR)

(cffi:defcenum PangoEllipsizeMode
	:PANGO_ELLIPSIZE_NONE
	:PANGO_ELLIPSIZE_START
	:PANGO_ELLIPSIZE_MIDDLE
	:PANGO_ELLIPSIZE_END)

(cffi:defcenum PangoAlignment
	:PANGO_ALIGN_LEFT
	:PANGO_ALIGN_CENTER
	:PANGO_ALIGN_RIGHT)

(cffi:defcstruct PangoGlyphItem
	(item :pointer)
	(glyphs :pointer))

(cffi:defcfun ("pango_layout_new" pango_layout_new) :pointer
  (context :pointer))

(cffi:defcfun ("pango_layout_copy" pango_layout_copy) :pointer
  (src :pointer))

(cffi:defcfun ("pango_layout_get_context" pango_layout_get_context) :pointer
  (layout :pointer))

(cffi:defcfun ("pango_layout_context_changed" pango_layout_context_changed) :void
  (layout :pointer))

(cffi:defcfun ("pango_layout_set_text" pango_layout_set_text) :void
  (layout :pointer)
  (text :string)
  (length :int))

(cffi:defcfun ("pango_layout_get_text" pango_layout_get_text) :string
  (layout :pointer))

(cffi:defcfun ("pango_layout_set_markup" pango_layout_set_markup) :void
  (layout :pointer)
  (markup :string)
  (length :int))

(cffi:defcfun ("pango_layout_set_markup_with_accel" pango_layout_set_markup_with_accel) :void
  (layout :pointer)
  (markup :string)
  (length :int)
  (accel_marker :pointer)
  (accel_char :pointer))

(cffi:defcfun ("pango_layout_set_attributes" pango_layout_set_attributes) :void
  (layout :pointer)
  (attrs :pointer))

(cffi:defcfun ("pango_layout_get_attributes" pango_layout_get_attributes) :pointer
  (layout :pointer))

(cffi:defcfun ("pango_layout_set_font_description" pango_layout_set_font_description) :void
  (layout :pointer)
  (desc :pointer))

(cffi:defcfun ("pango_layout_get_font_description" pango_layout_get_font_description) :pointer
  (layout :pointer))

(cffi:defcfun ("pango_layout_set_width" pango_layout_set_width) :void
  (layout :pointer)
  (width :int))

(cffi:defcfun ("pango_layout_get_width" pango_layout_get_width) :int
  (layout :pointer))

(cffi:defcfun ("pango_layout_set_height" pango_layout_set_height) :void
  (layout :pointer)
  (height :int))

(cffi:defcfun ("pango_layout_get_height" pango_layout_get_height) :int
  (layout :pointer))

(cffi:defcfun ("pango_layout_set_wrap" pango_layout_set_wrap) :void
  (layout :pointer)
  (wrap PangoWrapMode))

(cffi:defcfun ("pango_layout_get_wrap" pango_layout_get_wrap) PangoWrapMode
  (layout :pointer))

(cffi:defcfun ("pango_layout_is_wrapped" pango_layout_is_wrapped) :int
  (layout :pointer))

(cffi:defcfun ("pango_layout_set_ellipsize" pango_layout_set_ellipsize) :void
  (layout :pointer)
  (ellipsize PangoEllipsizeMode))

(cffi:defcfun ("pango_layout_get_ellipsize" pango_layout_get_ellipsize) PangoEllipsizeMode
  (layout :pointer))

(cffi:defcfun ("pango_layout_is_ellipsized" pango_layout_is_ellipsized) :int
  (layout :pointer))

(cffi:defcfun ("pango_layout_set_indent" pango_layout_set_indent) :void
  (layout :pointer)
  (indent :int))

(cffi:defcfun ("pango_layout_get_indent" pango_layout_get_indent) :int
  (layout :pointer))

(cffi:defcfun ("pango_layout_get_spacing" pango_layout_get_spacing) :int
  (layout :pointer))

(cffi:defcfun ("pango_layout_set_spacing" pango_layout_set_spacing) :void
  (layout :pointer)
  (spacing :int))

(cffi:defcfun ("pango_layout_set_justify" pango_layout_set_justify) :void
  (layout :pointer)
  (justify :int))

(cffi:defcfun ("pango_layout_get_justify" pango_layout_get_justify) :int
  (layout :pointer))

(cffi:defcfun ("pango_layout_set_auto_dir" pango_layout_set_auto_dir) :void
  (layout :pointer)
  (auto_dir :int))

(cffi:defcfun ("pango_layout_get_auto_dir" pango_layout_get_auto_dir) :int
  (layout :pointer))

(cffi:defcfun ("pango_layout_set_alignment" pango_layout_set_alignment) :void
  (layout :pointer)
  (alignment PangoAlignment))

(cffi:defcfun ("pango_layout_get_alignment" pango_layout_get_alignment) PangoAlignment
  (layout :pointer))

(cffi:defcfun ("pango_layout_set_tabs" pango_layout_set_tabs) :void
  (layout :pointer)
  (tabs :pointer))

(cffi:defcfun ("pango_layout_get_tabs" pango_layout_get_tabs) :pointer
  (layout :pointer))

(cffi:defcfun ("pango_layout_set_single_paragraph_mode" pango_layout_set_single_paragraph_mode) :void
  (layout :pointer)
  (setting :int))

(cffi:defcfun ("pango_layout_get_single_paragraph_mode" pango_layout_get_single_paragraph_mode) :int
  (layout :pointer))

(cffi:defcfun ("pango_layout_get_unknown_glyphs_count" pango_layout_get_unknown_glyphs_count) :int
  (layout :pointer))

(cffi:defcfun ("pango_layout_get_log_attrs" pango_layout_get_log_attrs) :void
  (layout :pointer)
  (attrs :pointer)
  (n_attrs :pointer))

(cffi:defcfun ("pango_layout_index_to_pos" pango_layout_index_to_pos) :void
  (layout :pointer)
  (index_ :int)
  (pos :pointer))

(cffi:defcfun ("pango_layout_index_to_line_x" pango_layout_index_to_line_x) :void
  (layout :pointer)
  (index_ :int)
  (trailing :int)
  (line :pointer)
  (x_pos :pointer))

(cffi:defcfun ("pango_layout_xy_to_index" pango_layout_xy_to_index) :int
  (layout :pointer)
  (x :int)
  (y :int)
  (index_ :pointer)
  (trailing :pointer))

(cffi:defcfun ("pango_layout_get_cursor_pos" pango_layout_get_cursor_pos) :void
  (layout :pointer)
  (index_ :int)
  (strong_pos PangoRectangle)
  (weak_pos PangoRectangle))

(cffi:defcfun ("pango_layout_move_cursor_visually" pango_layout_move_cursor_visually) :void
  (layout :pointer)
  (strong :int)
  (old_index :int)
  (old_trailing :int)
  (direction :int)
  (new_index :pointer)
  (new_trailing :pointer))

(cffi:defcfun ("pango_layout_get_extents" pango_layout_get_extents) :void
  (layout :pointer)
  (ink_rect :pointer)
  (logical_rect :pointer))

(cffi:defcfun ("pango_layout_get_pixel_extents" pango_layout_get_pixel_extents) :void
  (layout :pointer)
  (ink_rect :pointer)
  (logical_rect :pointer))

(cffi:defcfun ("pango_layout_get_size" pango_layout_get_size) :void
  (layout :pointer)
  (width :pointer)
  (height :pointer))

(cffi:defcfun ("pango_layout_get_pixel_size" pango_layout_get_pixel_size) :void
  (layout :pointer)
  (width :pointer)
  (height :pointer))

(cffi:defcfun ("pango_layout_get_baseline" pango_layout_get_baseline) :int
  (layout :pointer))

(cffi:defcfun ("pango_layout_get_line_count" pango_layout_get_line_count) :int
  (layout :pointer))

(cffi:defcfun ("pango_layout_get_line" pango_layout_get_line) PangoLayoutLine
  (layout :pointer)
  (line :int))

(cffi:defcfun ("pango_layout_get_line_readonly" pango_layout_get_line_readonly) PangoLayoutLine
  (layout :pointer)
  (line :int))

(cffi:defcfun ("pango_layout_get_lines" pango_layout_get_lines) GSList
  (layout :pointer))

(cffi:defcfun ("pango_layout_get_lines_readonly" pango_layout_get_lines_readonly) GSList
  (layout :pointer))

(cffi:defcfun ("pango_layout_get_iter" pango_layout_get_iter) :pointer
  (layout :pointer))

(cffi:defcfun ("pango_layout_iter_copy" pango_layout_iter_copy) :pointer
  (iter :pointer))

(cffi:defcfun ("pango_layout_iter_free" pango_layout_iter_free) :void
  (iter :pointer))

(cffi:defcfun ("pango_layout_iter_next_run" pango_layout_iter_next_run) :int
  (iter :pointer))

(cffi:defcfun ("pango_layout_iter_next_char" pango_layout_iter_next_char) :int
  (iter :pointer))

(cffi:defcfun ("pango_layout_iter_next_cluster" pango_layout_iter_next_cluster) :int
  (iter :pointer))

(cffi:defcfun ("pango_layout_iter_next_line" pango_layout_iter_next_line) :int
  (iter :pointer))

(cffi:defcfun ("pango_layout_iter_at_last_line" pango_layout_iter_at_last_line) :int
  (iter :pointer))

(cffi:defcfun ("pango_layout_iter_get_index" pango_layout_iter_get_index) :int
  (iter :pointer))

(cffi:defcfun ("pango_layout_iter_get_baseline" pango_layout_iter_get_baseline) :int
  (iter :pointer))

(cffi:defcfun ("pango_layout_iter_get_run" pango_layout_iter_get_run) :pointer
  (iter :pointer))

(cffi:defcfun ("pango_layout_iter_get_run_readonly" pango_layout_iter_get_run_readonly) :pointer
  (iter :pointer))

(cffi:defcfun ("pango_layout_iter_get_line" pango_layout_iter_get_line) :pointer
  (iter :pointer))

(cffi:defcfun ("pango_layout_iter_get_line_readonly" pango_layout_iter_get_line_readonly) :pointer
  (iter :pointer))

(cffi:defcfun ("pango_layout_iter_get_layout" pango_layout_iter_get_layout) :pointer
  (iter :pointer))

(cffi:defcfun ("pango_layout_iter_get_char_extents" pango_layout_iter_get_char_extents) :void
  (iter :pointer)
  (logical_rect :pointer))

(cffi:defcfun ("pango_layout_iter_get_cluster_extents" pango_layout_iter_get_cluster_extents) :void
  (iter :pointer)
  (ink_rect :pointer)
  (logical_rect :pointer))

(cffi:defcfun ("pango_layout_iter_get_run_extents" pango_layout_iter_get_run_extents) :void
  (iter :pointer)
  (ink_rect :pointer)
  (logical_rect :pointer))

(cffi:defcfun ("pango_layout_iter_get_line_yrange" pango_layout_iter_get_line_yrange) :void
  (iter :pointer)
  (y0_ :pointer)
  (y1_ :pointer))

(cffi:defcfun ("pango_layout_iter_get_line_extents" pango_layout_iter_get_line_extents) :void
  (iter :pointer)
  (ink_rect :pointer)
  (logical_rect :pointer))

(cffi:defcfun ("pango_layout_iter_get_layout_extents" pango_layout_iter_get_layout_extents) :void
  (iter :pointer)
  (ink_rect :pointer)
  (logical_rect :pointer))

(cffi:defcfun ("pango_layout_line_ref" pango_layout_line_ref) PangoLayoutLine
  (line :pointer))

(cffi:defcfun ("pango_layout_line_unref" pango_layout_line_unref) :void
  (line PangoLayoutLine))

(cffi:defcfun ("pango_layout_line_get_extents" pango_layout_line_get_extents) :void
  (line PangoLayoutLine)
  (ink_rect :pointer)
  (logical_rect :pointer))

(cffi:defcfun ("pango_layout_line_get_pixel_extents" pango_layout_line_get_pixel_extents) :void
  (layout_line PangoLayoutLine)
  (ink_rect :pointer)
  (logical_rect :pointer))

(cffi:defcfun ("pango_layout_line_index_to_x" pango_layout_line_index_to_x) :void
  (line PangoLayoutLine)
  (index_ :int)
  (trailing :int)
  (x_pos :pointer))

(cffi:defcfun ("pango_layout_line_x_to_index" pango_layout_line_x_to_index) :int
  (line PangoLayoutLine)
  (x_pos :int)
  (index_ :pointer)
  (trailing :pointer))

(cffi:defcfun ("pango_layout_line_get_x_ranges" pango_layout_line_get_x_ranges) :void
  (line PangoLayoutLine)
  (start_index :int)
  (end_index :int)
  (ranges :pointer)
  (n_ranges :pointer))

(cffi:defcenum PangoScript
	(:PANGO_SCRIPT_INVALID_CODE -1)
	(:PANGO_SCRIPT_COMMON 0)
	:PANGO_SCRIPT_INHERITED
	:PANGO_SCRIPT_ARABIC
	:PANGO_SCRIPT_ARMENIAN
	:PANGO_SCRIPT_BENGALI
	:PANGO_SCRIPT_BOPOMOFO
	:PANGO_SCRIPT_CHEROKEE
	:PANGO_SCRIPT_COPTIC
	:PANGO_SCRIPT_CYRILLIC
	:PANGO_SCRIPT_DESERET
	:PANGO_SCRIPT_DEVANAGARI
	:PANGO_SCRIPT_ETHIOPIC
	:PANGO_SCRIPT_GEORGIAN
	:PANGO_SCRIPT_GOTHIC
	:PANGO_SCRIPT_GREEK
	:PANGO_SCRIPT_GUJARATI
	:PANGO_SCRIPT_GURMUKHI
	:PANGO_SCRIPT_HAN
	:PANGO_SCRIPT_HANGUL
	:PANGO_SCRIPT_HEBREW
	:PANGO_SCRIPT_HIRAGANA
	:PANGO_SCRIPT_KANNADA
	:PANGO_SCRIPT_KATAKANA
	:PANGO_SCRIPT_KHMER
	:PANGO_SCRIPT_LAO
	:PANGO_SCRIPT_LATIN
	:PANGO_SCRIPT_MALAYALAM
	:PANGO_SCRIPT_MONGOLIAN
	:PANGO_SCRIPT_MYANMAR
	:PANGO_SCRIPT_OGHAM
	:PANGO_SCRIPT_OLD_ITALIC
	:PANGO_SCRIPT_ORIYA
	:PANGO_SCRIPT_RUNIC
	:PANGO_SCRIPT_SINHALA
	:PANGO_SCRIPT_SYRIAC
	:PANGO_SCRIPT_TAMIL
	:PANGO_SCRIPT_TELUGU
	:PANGO_SCRIPT_THAANA
	:PANGO_SCRIPT_THAI
	:PANGO_SCRIPT_TIBETAN
	:PANGO_SCRIPT_CANADIAN_ABORIGINAL
	:PANGO_SCRIPT_YI
	:PANGO_SCRIPT_TAGALOG
	:PANGO_SCRIPT_HANUNOO
	:PANGO_SCRIPT_BUHID
	:PANGO_SCRIPT_TAGBANWA
	:PANGO_SCRIPT_BRAILLE
	:PANGO_SCRIPT_CYPRIOT
	:PANGO_SCRIPT_LIMBU
	:PANGO_SCRIPT_OSMANYA
	:PANGO_SCRIPT_SHAVIAN
	:PANGO_SCRIPT_LINEAR_B
	:PANGO_SCRIPT_TAI_LE
	:PANGO_SCRIPT_UGARITIC
	:PANGO_SCRIPT_NEW_TAI_LUE
	:PANGO_SCRIPT_BUGINESE
	:PANGO_SCRIPT_GLAGOLITIC
	:PANGO_SCRIPT_TIFINAGH
	:PANGO_SCRIPT_SYLOTI_NAGRI
	:PANGO_SCRIPT_OLD_PERSIAN
	:PANGO_SCRIPT_KHAROSHTHI
	:PANGO_SCRIPT_UNKNOWN
	:PANGO_SCRIPT_BALINESE
	:PANGO_SCRIPT_CUNEIFORM
	:PANGO_SCRIPT_PHOENICIAN
	:PANGO_SCRIPT_PHAGS_PA
	:PANGO_SCRIPT_NKO
	:PANGO_SCRIPT_KAYAH_LI
	:PANGO_SCRIPT_LEPCHA
	:PANGO_SCRIPT_REJANG
	:PANGO_SCRIPT_SUNDANESE
	:PANGO_SCRIPT_SAURASHTRA
	:PANGO_SCRIPT_CHAM
	:PANGO_SCRIPT_OL_CHIKI
	:PANGO_SCRIPT_VAI
	:PANGO_SCRIPT_CARIAN
	:PANGO_SCRIPT_LYCIAN
	:PANGO_SCRIPT_LYDIAN)

(cffi:defcvar ("PangoScriptIter" PangoScriptIter)
 :pointer)

(cffi:defcfun ("pango_script_for_unichar" pango_script_for_unichar) PangoScript
  (ch :pointer))

(cffi:defcfun ("pango_script_get_sample_language" pango_script_get_sample_language) :pointer
  (script PangoScript))

(cffi:defcfun ("pango_script_iter_new" pango_script_iter_new) :pointer
  (text :string)
  (length :int))

(cffi:defcfun ("pango_script_iter_get_range" pango_script_iter_get_range) :void
  (iter :pointer)
  (start :pointer)
  (end :pointer)
  (script :pointer))

(cffi:defcfun ("pango_script_iter_next" pango_script_iter_next) :int
  (iter :pointer))

(cffi:defcfun ("pango_script_iter_free" pango_script_iter_free) :void
  (iter :pointer))

(cffi:defcvar ("PangoLanguage" PangoLanguage)
 :pointer)

(cffi:defcfun ("pango_language_from_string" pango_language_from_string) :pointer
  (language :string))

(cffi:defcfun ("pango_language_to_string" pango_language_to_string) :string
  (language :pointer))

(cffi:defcfun ("pango_language_matches" pango_language_matches) :int
  (language :pointer)
  (range_list :string))

(cffi:defcfun ("pango_language_includes_script" pango_language_includes_script) :int
  (language :pointer)
  (script PangoScript))

(cffi:defcfun ("pango_language_get_scripts" pango_language_get_scripts) :pointer
  (language :pointer)
  (num_scripts :pointer))

(cffi:defcfun ("pango_language_get_default" pango_language_get_default) :pointer)

(cffi:defcfun ("pango_language_get_sample_string" pango_language_get_sample_string) :string
  (language :pointer))

(cffi:defcenum PangoDirection
	:PANGO_DIRECTION_LTR
	:PANGO_DIRECTION_RTL
	:PANGO_DIRECTION_TTB_LTR
	:PANGO_DIRECTION_TTB_RTL
	:PANGO_DIRECTION_WEAK_LTR
	:PANGO_DIRECTION_WEAK_RTL
	:PANGO_DIRECTION_NEUTRAL)

(cffi:defcfun ("pango_unichar_direction" pango_unichar_direction) PangoDirection
  (ch :pointer))

(cffi:defcfun ("pango_find_base_dir" pango_find_base_dir) PangoDirection
  (text :pointer)
  (length :int))

(cffi:defcfun ("pango_get_mirror_char" pango_get_mirror_char) :int
  (ch :pointer)
  (mirrored_ch :pointer))

(cffi:defcenum PangoBidiType
	:PANGO_BIDI_TYPE_L
	:PANGO_BIDI_TYPE_LRE
	:PANGO_BIDI_TYPE_LRO
	:PANGO_BIDI_TYPE_R
	:PANGO_BIDI_TYPE_AL
	:PANGO_BIDI_TYPE_RLE
	:PANGO_BIDI_TYPE_RLO
	:PANGO_BIDI_TYPE_PDF
	:PANGO_BIDI_TYPE_EN
	:PANGO_BIDI_TYPE_ES
	:PANGO_BIDI_TYPE_ET
	:PANGO_BIDI_TYPE_AN
	:PANGO_BIDI_TYPE_CS
	:PANGO_BIDI_TYPE_NSM
	:PANGO_BIDI_TYPE_BN
	:PANGO_BIDI_TYPE_B
	:PANGO_BIDI_TYPE_S
	:PANGO_BIDI_TYPE_WS
	:PANGO_BIDI_TYPE_ON)

(cffi:defcfun ("pango_bidi_type_for_unichar" pango_bidi_type_for_unichar) PangoBidiType
  (ch :pointer))

(cffi:defcenum PangoGravity
	:PANGO_GRAVITY_SOUTH
	:PANGO_GRAVITY_EAST
	:PANGO_GRAVITY_NORTH
	:PANGO_GRAVITY_WEST
	:PANGO_GRAVITY_AUTO)

(cffi:defcenum PangoGravityHint
	:PANGO_GRAVITY_HINT_NATURAL
	:PANGO_GRAVITY_HINT_STRONG
	:PANGO_GRAVITY_HINT_LINE)

(cffi:defcfun ("pango_gravity_get_for_matrix" pango_gravity_get_for_matrix) PangoGravity
  (matrix :pointer))

(cffi:defcfun ("pango_gravity_get_for_script" pango_gravity_get_for_script) PangoGravity
  (script PangoScript)
  (base_gravity PangoGravity)
  (hint PangoGravityHint))

(cffi:defcfun ("pango_gravity_get_for_script_and_width" pango_gravity_get_for_script_and_width) PangoGravity
  (script PangoScript)
  (wide :int)
  (base_gravity PangoGravity)
  (hint PangoGravityHint))

(cffi:defcfun ("pango_gravity_to_rotation" pango_gravity_to_rotation) :double
  (gravity PangoGravity))

(cffi:defcvar ("PangoCairoFont" PangoCairoFont)
 :pointer)

(cffi:defcvar ("PangoCairoFontMap" PangoCairoFontMap)
 :pointer)

(cffi:defcvar ("cairo_t" cairo_t)
 :pointer)

(cffi:defcvar ("cairo_font_type_t" cairo_font_type_t)
 :pointer)

(cffi:defcfun ("pango_cairo_font_map_get_default" pango_cairo_font_map_get_default) :pointer)

(cffi:defcfun ("pango_cairo_font_map_set_default" pango_cairo_font_map_set_default) :void
  (fontmap :pointer))

(cffi:defcfun ("pango_cairo_font_map_new" pango_cairo_font_map_new) :pointer)

(cffi:defcfun ("pango_cairo_font_map_new_for_font_type" pango_cairo_font_map_new_for_font_type) :pointer
  (fonttype :pointer))

(cffi:defcfun ("pango_cairo_font_map_get_font_type" pango_cairo_font_map_get_font_type) :pointer
  (fontmap :pointer))

(cffi:defcfun ("pango_cairo_font_map_set_resolution" pango_cairo_font_map_set_resolution) :void
  (fontmap :pointer)
  (dpi :double))

(cffi:defcfun ("pango_cairo_font_map_get_resolution" pango_cairo_font_map_get_resolution) :double
  (fontmap :pointer))

(cffi:defcfun ("pango_cairo_font_map_create_context" pango_cairo_font_map_create_context) :pointer
  (fontmap :pointer))

(cffi:defcfun ("pango_cairo_font_get_scaled_font" pango_cairo_font_get_scaled_font) :pointer
  (font :pointer))

(cffi:defcfun ("pango_cairo_context_set_resolution" pango_cairo_context_set_resolution) :void
  (context :pointer)
  (dpi :double))

(cffi:defcfun ("pango_cairo_context_get_resolution" pango_cairo_context_get_resolution) :double
  (context :pointer))

(cffi:defcfun ("pango_cairo_context_set_font_options" pango_cairo_context_set_font_options) :void
  (context :pointer)
  (options :pointer))

(cffi:defcfun ("pango_cairo_context_get_font_options" pango_cairo_context_get_font_options) :pointer
  (context :pointer))

(cffi:defcvar ("PangoCairoShapeRendererFunc" PangoCairoShapeRendererFunc)
 :pointer)

(cffi:defcfun ("pango_cairo_context_set_shape_renderer" pango_cairo_context_set_shape_renderer) :void
  (context :pointer)
  (func :pointer)
  (data :pointer)
  (dnotify :pointer))

(cffi:defcfun ("pango_cairo_context_get_shape_renderer" pango_cairo_context_get_shape_renderer) :pointer
  (context :pointer)
  (data :pointer))

(cffi:defcfun ("pango_cairo_create_context" pango_cairo_create_context) :pointer
  (cr :pointer))

(cffi:defcfun ("pango_cairo_update_context" pango_cairo_update_context) :void
  (cr :pointer)
  (context :pointer))

(cffi:defcfun ("pango_cairo_create_layout" pango_cairo_create_layout) :pointer
  (cr :pointer))

(cffi:defcfun ("pango_cairo_update_layout" pango_cairo_update_layout) :void
  (cr :pointer)
  (layout :pointer))

(cffi:defcfun ("pango_cairo_show_glyph_string" pango_cairo_show_glyph_string) :void
  (cr :pointer)
  (font :pointer)
  (glyphs :pointer))

(cffi:defcfun ("pango_cairo_show_glyph_item" pango_cairo_show_glyph_item) :void
  (cr :pointer)
  (text :string)
  (glyph_item :pointer))

(cffi:defcfun ("pango_cairo_show_layout_line" pango_cairo_show_layout_line) :void
  (cr :pointer)
  (line :pointer))

(cffi:defcfun ("pango_cairo_show_layout" pango_cairo_show_layout) :void
  (cr :pointer)
  (layout :pointer))

(cffi:defcfun ("pango_cairo_show_error_underline" pango_cairo_show_error_underline) :void
  (cr :pointer)
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(cffi:defcfun ("pango_cairo_glyph_string_path" pango_cairo_glyph_string_path) :void
  (cr :pointer)
  (font :pointer)
  (glyphs :pointer))

(cffi:defcfun ("pango_cairo_layout_line_path" pango_cairo_layout_line_path) :void
  (cr :pointer)
  (line :pointer))

(cffi:defcfun ("pango_cairo_layout_path" pango_cairo_layout_path) :void
  (cr :pointer)
  (layout :pointer))

(cffi:defcfun ("pango_cairo_error_underline_path" pango_cairo_error_underline_path) :void
  (cr :pointer)
  (x :double)
  (y :double)
  (width :double)
  (height :double))


(defmacro add-pango-attribute (attr-list attr &optional start end)
  (let ((attrs (gensym))
	(a (gensym))
	(s (gensym))
	(e (gensym)))
    
    `(let ((,attrs ,attr-list)
	   (,a ,attr)
	   (,s ,start)
	   (,e ,end))
       ,(if start `(setf (cffi:foreign-slot-value ,a 'pangoattribute 'start_index) ,s))
       ,(if end `(setf (cffi:foreign-slot-value ,a 'pangoattribute 'end_index) ,e))
       (pango_attr_list_insert ,attrs ,a))))
	     
(defun ezcolor (value)
  (* 257 value))
	   

(defun get-line-from-position (layout pos)
  (cffi:with-foreign-objects ((line :int)
			      (x_pos :int))
    (pango_layout_index_to_line_x layout pos 0 line x_pos)
    (values (cffi:mem-aref line :int)
	    (/ (cffi:mem-aref x_pos :int) PANGO_SCALE))))


(defun get-layout-size (layout)
  (cffi:with-foreign-objects ((w :int)
			      (h :int))
    (pango_layout_get_size layout w h)
    (values 
     (/ (cffi:mem-aref w :int) PANGO_SCALE)
     (/ (cffi:mem-aref h :int) PANGO_SCALE))))
	

(defun get-layout-line-extents (line)
  (cffi:with-foreign-objects ((ink :pointer)
			      (logical :pointer))
    (pango_layout_line_get_extents line ink logical)      
    (values (/ (cffi:foreign-slot-value ink 'PangoRectangle 'x) PANGO_SCALE)
	    (/ (cffi:foreign-slot-value ink 'PangoRectangle 'y) PANGO_SCALE))))

(defun get-layout-lines-data (layout)
  (let* ((i 0)
	 (ret (loop with itr = (pango_layout_get_lines layout)
		 until (cffi-sys:null-pointer-p itr)
		 collect (let ((data (cffi:foreign-slot-value itr 'GSList 'data)))
			   (append (multiple-value-list 
				    (get-layout-line-extents data))
				   (list (cffi:foreign-slot-value data 'PangoLayoutLine 'start_index)
					 (cffi:foreign-slot-value data 'PangoLayoutLine 'length)
					 data)))
				 
		 do (setf itr (cffi:foreign-slot-value itr 'GSList 'next))
		 do (incf i))))
    (list i ret)))

(defun move-cursor-visually (layout pos &key (forward t) (strong t) (trailing nil))
  (let ((direction (if forward 1 -1))
	(strong (if strong 1 0))
	(old-trailing (if trailing 1 0)))
    
    (cffi:with-foreign-objects ((index :int)
				(trailing :int))
      (pango_layout_move_cursor_visually layout strong pos old-trailing direction index trailing)
      (values (cffi:mem-aref index :int)
	      (cffi:mem-aref trailing :int)))))

(defun get-layout-lines (layout)
  (loop with itr = (pango_layout_get_lines layout)
     until (cffi-sys:null-pointer-p itr)
     collect (cffi:foreign-slot-value itr 'GSList 'data)
     do (setf itr (cffi:foreign-slot-value itr 'GSList 'next))))
	 

(defun get-previous-line (layout index)
  (let ((lines (get-layout-lines layout)))
    (loop for i in lines
       for x from 0
       until (let ((start (cffi:foreign-slot-value i 'pangolayoutline 'start_index))
		   (len (cffi:foreign-slot-value i 'pangolayoutline 'length)))
	       (and (>= index start)
		    (< index (+ start len))))
       finally (return (list x
			     (cffi:with-foreign-object (x-pos :int)
			       (pango_layout_line_index_to_x 
				i 
				index 
				0 
				x-pos)
			       (/ (cffi:mem-aref x-pos :int) PANGO_SCALE)))))))

(defun layout-line-index-to-x (line index &key (trailing 0))
  (cffi:with-foreign-object (x :int)
    (pango_layout_line_index_to_x line index trailing x)
    (cffi:mem-aref x :int)))

(defun layout-line-x-to-index (line x) 
  (cffi:with-foreign-objects ((index :int)
			      (trailing :int))
    (pango_layout_line_x_to_index line x index trailing)
    (values (cffi:mem-aref index :int)
	    (cffi:mem-aref trailing :int))))

(defun get-cursor-pos (layout index)
	   (cffi:with-foreign-objects ((strong :pointer)
				       (weak :pointer))
	     (pango_layout_get_cursor_pos layout index strong weak)
	     (values (unless (cffi-sys:null-pointer-p strong)
		       (list (/ (cffi:foreign-slot-value strong 'PangoRectangle 'x) PANGO_SCALE)
			     (/ (cffi:foreign-slot-value strong 'PangoRectangle 'y) PANGO_SCALE)
			     (/ (cffi:foreign-slot-value strong 'PangoRectangle 'width) PANGO_SCALE)
			     (/ (cffi:foreign-slot-value strong 'PangoRectangle 'height) PANGO_SCALE)))
		     (unless (cffi-sys:null-pointer-p weak)
		       (list (/ (cffi:foreign-slot-value weak 'PangoRectangle 'x) PANGO_SCALE)
			     (/ (cffi:foreign-slot-value weak 'PangoRectangle 'y) PANGO_SCALE)
			     (/ (cffi:foreign-slot-value weak 'PangoRectangle 'width) PANGO_SCALE)
			     (/ (cffi:foreign-slot-value weak 'PangoRectangle 'height) PANGO_SCALE))))))


(defun list-font-families (&optional (font-map (pango_cairo_font_map_get_default))) 
  (cffi:with-foreign-objects ((c :int)
			      (l :pointer))
    (pango_font_map_list_families font-map l c)
    (let ((len (cffi:mem-aref c :int)))
      (prog1 (loop for i from 0 to (1- len)
		collect (cffi:mem-aref (cffi:mem-aref l :pointer ) :pointer i))
	(g_object_unref l)))))
		     
(defun list-font-family-names (&optional (font-map (pango_cairo_font_map_get_default))) 
  (cffi:with-foreign-objects ((c :int)
			      (l :pointer))
    (pango_font_map_list_families font-map l c)
    (let ((len (cffi:mem-aref c :int)))
      (prog1 (loop for i from 0 to (1- len)
		collect (pango_font_family_get_name (cffi:mem-aref (cffi:mem-aref l :pointer ) :pointer i)))
	(g_object_unref l)))))

(defun list-font-faces (font-family)
  (cffi:with-foreign-objects ((l :pointer)
			      (c :int))
    (pango_font_family_list_faces font-family l c)
    (loop for i from 0 to (1- (cffi:mem-aref c :int))
       collect (cffi:mem-aref (cffi:mem-aref l :pointer) :pointer i))))
