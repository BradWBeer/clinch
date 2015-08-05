;;;; clinch-cairo.lisp
;;;; Please see the licence.txt for the CLinch 

(in-package #:clinch)


(DEFUN ARRAY-OF-ARRAY->LIST (AOA)
  (LOOP FOR I IN (COERCE AOA 'LIST)
     APPEND (COERCE I 'LIST)))

;;(setf scene (classimp:import-into-lisp "/home/warweasle/work/lisp/hw01/FedInt/Federation\ Interceptor\ HN48/Federation\ Interceptor\ HN48\ flying.obj"))
;;(setf scene (classimp:import-into-lisp "/home/warweasle/work/assets/sphere.obj" :processing-flags '(:ai-process-triangulate)))
(setf scene (classimp:import-into-lisp "/home/warweasle/work/lisp/hw01/FedInt/Federation\ Interceptor\ HN48/Federation\ Interceptor\ HN48\ flying.obj" :processing-flags '(:ai-process-triangulate)))

(defun load-mesh (path)
  (classimp:import-into-lisp path :processing-flags '(:ai-process-triangulate)))


(defmethod ai-mesh->clinch-entity ((mesh classimp:mesh))

  (with-accessors ((primitive primitive-types)
		   (vertices vertices)
		   (normals normals)
		   (tangents tangents)
		   (bitangents bitangents)
		   (colors colors)
		   (texture-coords texture-coords)
		   (per-tex components-per-texture-coord)
		   (faces faces)
		   (bones bones)
		   (material classimp:material-index)) mesh
    
    (unless (= (classimp:primitive-types mesh) 4)
      (error "Only triangles are supported at this time!"))

    material))

(defun load-scene-as-node (path)

  (let ((scene (import-into-lisp path :processing-flags '(:ai-process-preset-target-realtime-quality :ai-process-triangulate))))

    (unless scene
      (error (format nil "Could not load mesh ~S!" path)))

    (let ((directory (directory-namestring path)))

      directory)))

;; (load-scene-as-node "FedInt/Federation\ Interceptor\ HN48/Federation\ Interceptor\ HN48\ flying.obj")  


;; PRIMITIVE-TYPES              = 4
;; VERTICES                     = #(#(13.7161 68.3071 87.3337) #(13.7254 67.7288 85.6473)..
;; NORMALS                      = #(#(-0.998733 -0.0162137 3.55404e-5) #(-0.998178 -0.0161228 2.40517e-5..
;; TANGENTS                     = NIL
;; BITANGENTS                   = NIL
;; COLORS                       = #()
;; TEXTURE-COORDS               = #(#(#(0.88562995 0.43052 0.0) #(0.88246995 0.42542 0.0)..
;; COMPONENTS-PER-TEXTURE-COORD  = #(2 0 0 0 0 0 0 0)
;; FACES                        = #(#(0 1 2) #(3 4 5) #(6 7 8) #(9 10 11) #(12 13 14) #(15 16 17)..
;; BONES                        = NIL
;; MATERIAL-INDEX               = 1


;; (("$mat.refracti" . 0.0) ("$mat.opacity" . 1.0) ("$mat.shininess" . 3.0)
;;  ("$clr.specular" . #(1.0 1.0 1.0))
;;  ("$clr.diffuse" . #(0.27451 0.27451 0.27451))
;;  ("$clr.ambient" . #(0.0 0.0 0.0)) ("$mat.shadingm" . :AI-SHADING-MODE-GOURAUD)
;;  ("?mat.name" . "default")) 

;; (("$tex.file" (:AI-TEXTURE-TYPE-DIFFUSE 0 "/Maps/yx1_02_01_01_02.jpg"))
;;  ("$mat.refracti" . 0.0) ("$mat.opacity" . 1.0) ("$mat.shininess" . 3.0)
;;  ("$clr.specular" . #(1.0 1.0 1.0)) ("$clr.diffuse" . #(1.0 1.0 1.0))
;;  ("$clr.ambient" . #(0.0 0.0 0.0)) ("$mat.shadingm" . :AI-SHADING-MODE-GOURAUD)
;;  ("?mat.name" . "default_cockpits_1_cushions")) 

;; (("$tex.file" (:AI-TEXTURE-TYPE-DIFFUSE 0 "/Maps/ggt1.jpg"))
;;  ("$mat.refracti" . 0.0) ("$mat.opacity" . 1.0) ("$mat.shininess" . 3.0)
;;  ("$clr.specular" . #(1.0 1.0 1.0)) ("$clr.diffuse" . #(1.0 1.0 1.0))
;;  ("$clr.ambient" . #(1.0 1.0 1.0)) ("$mat.shadingm" . :AI-SHADING-MODE-GOURAUD)
;;  ("?mat.name" . "default_cockpits_1_display")) 

;; (("$mat.refracti" . 0.0) ("$mat.opacity" . 1.0) ("$mat.shininess" . 3.0)
;;  ("$clr.specular" . #(1.0 1.0 1.0)) ("$clr.diffuse" . #(0.0 0.0 0.0))
;;  ("$clr.ambient" . #(0.0 0.0 0.0)) ("$mat.shadingm" . :AI-SHADING-MODE-GOURAUD)
;;  ("?mat.name" . "default_cockpits_joysticks")) 

;; (("$tex.file" (:AI-TEXTURE-TYPE-DIFFUSE 0 "/Maps/ahm1_01_01.jpg"))
;;  ("$mat.refracti" . 0.0) ("$mat.opacity" . 1.0) ("$mat.shininess" . 3.0)
;;  ("$clr.specular" . #(1.0 1.0 1.0)) ("$clr.diffuse" . #(1.0 1.0 1.0))
;;  ("$clr.ambient" . #(0.0 0.0 0.0)) ("$mat.shadingm" . :AI-SHADING-MODE-GOURAUD)
;;  ("?mat.name" . "default_skin")) 

;; (("$mat.refracti" . 0.0) ("$mat.opacity" . 0.65) ("$mat.shininess" . 3.0)
;;  ("$clr.specular" . #(1.0 1.0 1.0)) ("$clr.diffuse" . #(0.501961 1.0 0.0))
;;  ("$clr.ambient" . #(0.0 0.0 0.0)) ("$mat.shadingm" . :AI-SHADING-MODE-GOURAUD)
;;  ("?mat.name" . "default_cabins1glass")) 

;; (("$tex.file" (:AI-TEXTURE-TYPE-DIFFUSE 0 "/Maps/as1_02.jpg"))
;;  ("$mat.refracti" . 0.0) ("$mat.opacity" . 1.0) ("$mat.shininess" . 4.0)
;;  ("$clr.specular" . #(1.0 1.0 1.0)) ("$clr.diffuse" . #(1.0 1.0 1.0))
;;  ("$clr.ambient" . #(0.0 0.0 0.0)) ("$mat.shadingm" . :AI-SHADING-MODE-GOURAUD)
;;  ("?mat.name" . "default_ship_door")) 

;; (("$tex.file" (:AI-TEXTURE-TYPE-DIFFUSE 0 "/Maps/as1_02.jpg"))
;;  ("$mat.refracti" . 0.0) ("$mat.opacity" . 1.0) ("$mat.shininess" . 3.0)
;;  ("$clr.specular" . #(1.0 1.0 1.0)) ("$clr.diffuse" . #(1.0 1.0 1.0))
;;  ("$clr.ambient" . #(0.0 0.0 0.0)) ("$mat.shadingm" . :AI-SHADING-MODE-GOURAUD)
;;  ("?mat.name" . "default_door")) 

;; (("$tex.file" (:AI-TEXTURE-TYPE-DIFFUSE 0 "/Maps/rmp1.jpg"))
;;  ("$mat.refracti" . 0.0) ("$mat.opacity" . 1.0) ("$mat.shininess" . 4.0)
;;  ("$clr.specular" . #(1.0 1.0 1.0)) ("$clr.diffuse" . #(1.0 1.0 1.0))
;;  ("$clr.ambient" . #(0.0 0.0 0.0)) ("$mat.shadingm" . :AI-SHADING-MODE-GOURAUD)
;;  ("?mat.name" . "default_steps")) 



;; #<MESH {100546FEE3}>
;;   [standard-object]

;; Slots with :INSTANCE allocation:
;;   PRIMITIVE-TYPES              = 4
;;   VERTICES                     = #(#(13.7161 68.3071 87.3337) #(13.7254 67.7288 85.6473)..
;;   NORMALS                      = #(#(-0.998733 -0.0162137 3.55404e-5) #(-0.998178 -0.0161228 2.40517e-5..
;;   TANGENTS                     = NIL
;;   BITANGENTS                   = NIL
;;   COLORS                       = #()
;;   TEXTURE-COORDS               = #(#(#(0.88562995 0.43052 0.0) #(0.88246995 0.42542 0.0)..
;;   COMPONENTS-PER-TEXTURE-COORD  = #(2 0 0 0 0 0 0 0)
;;   FACES                        = #(#(0 1 2) #(3 4 5) #(6 7 8) #(9 10 11) #(12 13 14) #(15 16 17)..
;;   BONES                        = NIL
;;   MATERIAL-INDEX               = 1
;; #<MESH {100546FF03}>
;;   [standard-object]

;; Slots with :INSTANCE allocation:
;;   PRIMITIVE-TYPES              = 8
;;   VERTICES                     = #(#(15.7649 91.5069 59.7873) #(15.4417 91.5069 57.377098)..
;;   NORMALS                      = #(#(0.219805 0.972894 -0.0425206) #(0.219842 0.966412 -0.11971)..
;;   TANGENTS                     = NIL
;;   BITANGENTS                   = NIL
;;   COLORS                       = #()
;;   TEXTURE-COORDS               = #(#(#(0.0 0.0 0.0) #(0.0 0.0 0.0) #(0.0 0.0 0.0) #(0.0 0.0 0.0)..
;;   COMPONENTS-PER-TEXTURE-COORD  = #(2 0 0 0 0 0 0 0)
;;   FACES                        = #(#(0 1 2 3) #(4 5 6 7) #(8 9 10 11) #(12 13 14 15) #(16 17 18 19)..
;;   BONES                        = NIL
;;   MATERIAL-INDEX               = 2
;; #<MESH {100546FF23}>
;;   [standard-object]

;; Slots with :INSTANCE allocation:
;;   PRIMITIVE-TYPES              = 8
;;   VERTICES                     = #(#(1.97091 66.0274 114.26) #(1.93256 65.3902 114.355)..
;;   NORMALS                      = #(#(0.174387 -0.133046 -0.97393197) #(0.178872 -0.395491 -0.90046)..
;;   TANGENTS                     = NIL
;;   BITANGENTS                   = NIL
;;   COLORS                       = #()
;;   TEXTURE-COORDS               = #(#(#(0.16356 0.068059996 0.0) #(0.19179 0.068059996 0.0)..
;;   COMPONENTS-PER-TEXTURE-COORD  = #(2 0 0 0 0 0 0 0)
;;   FACES                        = #(#(0 1 2 3) #(4 5 6 7) #(8 9 10 11) #(12 13 14 15) #(16 17 18 19)..
;;   BONES                        = NIL
;;   MATERIAL-INDEX               = 3
;; #<MESH {100546FF43}>
;;   [standard-object]

;; Slots with :INSTANCE allocation:
;;   PRIMITIVE-TYPES              = 8
;;   VERTICES                     = #(#(-0.908278 68.9006 123.284) #(0.885962 68.9006 123.284)..
;;   NORMALS                      = #(#(0.0 0.632809 -0.772126) #(0.0 0.632809 -0.772126)..
;;   TANGENTS                     = NIL
;;   BITANGENTS                   = NIL
;;   COLORS                       = #()
;;   TEXTURE-COORDS               = #(#(#(0.62007 0.999999 0.0) #(0.37993 0.999999 0.0)..
;;   COMPONENTS-PER-TEXTURE-COORD  = #(2 0 0 0 0 0 0 0)
;;   FACES                        = #(#(0 1 2 3) #(4 5 6 7) #(8 9 10 11) #(12 13 14 15) #(16 17 18 19)..
;;   BONES                        = NIL
;;   MATERIAL-INDEX               = 4
;; #<MESH {100546FF63}>
;;   [standard-object]

;; Slots with :INSTANCE allocation:
;;   PRIMITIVE-TYPES              = 8
;;   VERTICES                     = #(#(-3.31833 70.7569 120.387) #(-3.16448 70.7569 120.387)..
;;   NORMALS                      = #(#(-0.354327 -0.205618 0.911106) #(0.354435 -0.20565599 0.911057)..
;;   TANGENTS                     = NIL
;;   BITANGENTS                   = NIL
;;   COLORS                       = #()
;;   TEXTURE-COORDS               = #(#(#(0.0 0.0 0.0) #(0.0 0.0 0.0) #(0.0 0.0 0.0) #(0.0 0.0 0.0)..
;;   COMPONENTS-PER-TEXTURE-COORD  = #(2 0 0 0 0 0 0 0)
;;   FACES                        = #(#(0 1 2 3) #(4 5 6 7) #(8 9 10 11) #(12 13 14 15) #(16 17 18 19)..
;;   BONES                        = NIL
;;   MATERIAL-INDEX               = 5
;; #<MESH {100546FF83}>
;;   [standard-object]

;; Slots with :INSTANCE allocation:
;;   PRIMITIVE-TYPES              = 8
;;   VERTICES                     = #(#(-1.48738 72.6476 114.598) #(1.46508 72.6476 114.598)..
;;   NORMALS                      = #(#(-0.468258 0.867635 -0.164313) #(0.383331 0.921391 -0.063869104)..
;;   TANGENTS                      = NIL
;;   BITANGENTS                   = NIL
;;   COLORS                       = #()
;;   TEXTURE-COORDS               = #(#(#(0.67388 0.8055 0.0) #(0.32612 0.8055 0.0) #(0.32612 0.79472 0.0)..
;;   COMPONENTS-PER-TEXTURE-COORD  = #(2 0 0 0 0 0 0 0)
;;   FACES                        = #(#(0 1 2 3) #(4 5 6 7) #(8 9 10 11) #(12 13 14 15) #(16 17 18 19)..
;;   BONES                        = NIL
;;   MATERIAL-INDEX               = 1
;; #<MESH {100546FFA3}>
;;   [standard-object]

;; Slots with :INSTANCE allocation:
;;   PRIMITIVE-TYPES              = 8
;;   VERTICES                     = #(#(23.2325 114.756 8.95546) #(24.3486 111.202 8.95546)..
;;   NORMALS                      = #(#(0.454504 0.874731 0.160694) #(0.951774 0.29889598 0.0376323)..
;;   TANGENTS                     = NIL
;;   BITANGENTS                   = NIL
;;   COLORS                       = #()
;;   TEXTURE-COORDS               = #(#(#(0.755324 0.869577 0.0) #(0.755324 0.837812 0.0)..
;;   COMPONENTS-PER-TEXTURE-COORD  = #(2 0 0 0 0 0 0 0)
;;   FACES                        = #(#(0 1 2 3) #(4 5 6 7) #(8 9 10 11) #(12 13 14 15) #(16 17 18 19)..
;;   BONES                        = NIL
;;   MATERIAL-INDEX               = 6
;; #<MESH {100546FFC3}>
;;   [standard-object]

;; Slots with :INSTANCE allocation:
;;   PRIMITIVE-TYPES              = 8
;;   VERTICES                     = #(#(-0.00263464 83.5208 123.659) #(-0.0026337 84.1404 120.497)..
;;   NORMALS                      = #(#(-0.0055948 0.982451 0.177238) #(-0.00736683 0.979164 0.19452)..
;;   TANGENTS                     = NIL
;;   BITANGENTS                   = NIL
;;   COLORS                       = #()
;;   TEXTURE-COORDS               = #(#(#(0.0 0.0 0.0) #(0.0 0.0 0.0) #(0.0 0.0 0.0) #(0.0 0.0 0.0)..
;;   COMPONENTS-PER-TEXTURE-COORD  = #(2 0 0 0 0 0 0 0)
;;   FACES                        = #(#(0 1 2 3) #(4 5 6 7) #(8 9 10 11) #(12 13 14 15) #(16 17 18 19)..
;;   BONES                        = NIL
;;   MATERIAL-INDEX               = 7
;; #<MESH {100546FFE3}>
;;   [standard-object]

;; Slots with :INSTANCE allocation:
;;   PRIMITIVE-TYPES              = 8
;;   VERTICES                     = #(#(-5.61433 79.2323 -100.939) #(-5.61433 79.5153 -100.548)..
;;   NORMALS                      = #(#(0.998309 0.0 0.0) #(0.998309 0.0 0.0) #(0.998309 0.0 0.0)..
;;   TANGENTS                     = NIL
;;   BITANGENTS                   = NIL
;;   COLORS                       = #()
;;   TEXTURE-COORDS               = #(#(#(0.0 0.0 0.0) #(0.0 0.0 0.0) #(0.0 0.0 0.0) #(0.0 0.0 0.0)..
;;   COMPONENTS-PER-TEXTURE-COORD  = #(2 0 0 0 0 0 0 0)
;;   FACES                        = #(#(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23)..
;;   BONES                        = NIL
;;   MATERIAL-INDEX               = 1
;; #<MESH {10057B8003}>
;;   [standard-object]

;; Slots with :INSTANCE allocation:
;;   PRIMITIVE-TYPES              = 8
;;   VERTICES                     = #(#(0.0187293 59.2045 -152.385) #(-2.96669 59.183 -152.359)..
;;   NORMALS                      = #(#(-2.32623e-9 0.90554297 -0.420255) #(-0.0213599 0.905533 -0.419736)..
;;   TANGENTS                     = NIL
;;   BITANGENTS                   = NIL
;;   COLORS                       = #()
;;   TEXTURE-COORDS               = #(#(#(0.0 0.0 0.0) #(0.0 0.0 0.0) #(0.0 0.0 0.0) #(0.0 0.0 0.0)..
;;   COMPONENTS-PER-TEXTURE-COORD  = #(2 0 0 0 0 0 0 0)
;;   FACES                        = #(#(0 1 2 3) #(4 5 6 7) #(8 9 10 11) #(12 13 14 15) #(16 17 18 19)..
;;   BONES                        = NIL
;;   MATERIAL-INDEX               = 8
;; #<MESH {10057B8023}>
;;   [standard-object]

;; Slots with :INSTANCE allocation:
;;   PRIMITIVE-TYPES              = 8
;;   VERTICES                     = #(#(0.0187311 59.2249 -147.996) #(-3.54886 59.1994 -147.96)..
;;   NORMALS                      = #(#(1.82038e-4 -0.910928 0.408454) #(0.0216777 -0.911089 0.407518)..
;;   TANGENTS                     = NIL
;;   BITANGENTS                   = NIL
;;   COLORS                       = #()
;;   TEXTURE-COORDS               = #(#(#(0.443661 0.350009 0.0) #(0.443721 0.349771 0.0)..
;;   COMPONENTS-PER-TEXTURE-COORD  = #(2 0 0 0 0 0 0 0)
;;   FACES                        = #(#(0 1 2 3) #(4 5 6 7) #(8 9 10 11) #(12 13 14 15) #(16 17 18 19)..
;;   BONES                        = NIL
;;   MATERIAL-INDEX               = 9
;; #<MESH {10057B8043}>
;;   [standard-object]

;; Slots with :INSTANCE allocation:
;;   PRIMITIVE-TYPES              = 8
;;   VERTICES                     = #(#(-7.78721 69.4578 -136.431) #(7.78721 69.4578 -136.431)..
;;   NORMALS                      = #(#(-0.407634 0.9115 0.0134352) #(0.407847 0.911404 0.0134338)..
;;   TANGENTS                     = NIL
;;   BITANGENTS                   = NIL
;;   COLORS                       = #()
;;   TEXTURE-COORDS               = #(#(#(2.6157e-4 1.0 0.0) #(2.6157e-4 -8.7378e-9 0.0)..
;;   COMPONENTS-PER-TEXTURE-COORD  = #(2 0 0 0 0 0 0 0)
;;   FACES                        = #(#(0 1 2 3) #(4 5 6 7) #(8 9 10 11) #(12 13 14 15) #(16 17 18 19)..
;;   BONES                        = NIL
;;   MATERIAL-INDEX               = 10


;; (labels ((replace-$ (x)
;; 	   (ppcre:regex-replace-all "\\$" x ""))
;; 	 (replace-. (x)
;; 	   (ppcre:regex-replace-all "\\." x "-")))
;;   (remove-duplicates (map 'list (lambda (x)
;; 				  (map 'list (lambda (x)
;; 					       (replace-. (replace-$ x))) (alexandria:hash-table-keys x))) (classimp:materials scene)) :test #'equal))

(defun remove$ (x)
  (ppcre:regex-replace-all "\\$" x ""))

(defun remove. (x)
  (ppcre:regex-replace-all "\\." x "_"))

(defun remove.$ (x)
  (remove.
   (remove$ x)))


(defmethod material->fingerprint ((mat hash-table))
  
  (let ((lst))
    (alexandria:maphash-keys (lambda (x)
			       (push (remove.$ x) 
				     lst))
			     mat)

    (sort lst #'string-lessp)))

(defvar prepackaged-shaders)
(setf prepackaged-shaders
  `((("?mat_name" "clr_ambient" "clr_diffuse" "clr_specular" "mat_opacity"
		  "mat_refracti" "mat_shadingm" "mat_shininess") . simple-lighting-with-single-color-shader)
    (("?mat_name" "clr_ambient" "clr_diffuse" "clr_specular" "mat_opacity"
		  "mat_refracti" "mat_shadingm" "mat_shininess" "tex_file") . simple-lighting-with-texture-shader)))


(defmethod get-shader ((material hash-table) (path string))
  (let* ((fingerprint (material->fingerprint material))
	 (shader-name (cdr (assoc fingerprint prepackaged-shaders :test #'equal))))
    (when shader-name
      (values
       shader-name
       (append (loop for key being the hash-keys of material
		  using (hash-value value)
		  for val = (let ((name  (remove.$ key)))
			      (cond
			       ((string-equal name "clr_ambient") `(:uniform ,name ,value))
			       ((string-equal name "clr_diffuse") `(:uniform ,name ,value))
			       ;;((string-equal name "clr_specular") `(:uniform ,name ,value))
			       ((string-equal name "mat_opacity") `(:uniform ,name ,value))
			       ;;((string-equal name "mat_refracti") `(:uniform ,name ,value))
			       ;;((string-equal name "mat_shadingm") `(:uniform ,name ,value))
			       ;;((string-equal name "mat_shininess") `(:uniform ,name ,value))
			       ((string-equal name "tex_file") `(:uniform ,name (get-texture ,(concatenate 'string (directory-namestring path) (third (car value))))))))
		 if val collect val)
	      '((:uniform "ambientLight"   ambientLight)
		(:uniform "lightIntensity" lightIntensity)
		(:uniform "lightDirection" (lambda (&optional a b c) lightDirection))))))))
  

(defmethod mesh->entity ((this classimp:scene) (item integer) (path string))
  (let ((mesh (elt (meshes this) item)))
    (multiple-value-bind (shader shader-values) (get-shader (elt (materials this) (material-index mesh)) path)
    
    `(make-instance
      'clinch:entity
      :shader ,shader
      
      :indexes (make-instance 'clinch:buffer :qtype :unsigned-int
      			      :target :element-array-buffer
      			      :Stride 1
      			      :data ',(array-of-array->list (FACES mesh)))
      :vertices (make-instance 'clinch:buffer 
      			       :Stride 3
      			       :data (map 'list (lambda (x)
      						  (coerce x 'single-float)) ',(array-of-array->list (VERTICES mesh))))
      :normals (make-instance 'clinch:buffer 
      			      :Stride 3
      			      :data (map 'list (lambda (x)
      						 (coerce x 'single-float)) ',(array-of-array->list (NORMALS mesh))))
      :values (,@shader-values
      	       ,@(when (eql shader 'simple-lighting-with-texture-shader)
		       (loop 
      			  for x from 0
      			  for tex across (components-per-texture-coord mesh)
      			  while (> tex 0)
      			  collect `(:attribute ,(format nil "tc~A" x)
      					       (clinch:buffer 
      						 :Stride ,tex
      						 :data ',(array-of-array->list (elt (texture-coords mesh) x)))))))))))
