;;;; shapes.lisp
;;;; Please see the licence.txt for the CLinch 
;; 
(in-package #:clinch)

(defun make-quad-indices ()
  (make-instance 'clinch:index-buffer :data '(0 1 2 0 2 3)))

(defun make-quad-vertexes (width height &key (center :center))

  (let* ((x/2 (float (/ width 2)))
	 (y/2 (float (/ height 2)))
	 (x)
	 (y) 
	 (-x)
	 (-y))

    (if (typep center 'SIMPLE-ARRAY)
	(let ((x-offset (aref center 0))
	      (y-offset (aref center 1)))
	  (setf x  (+ x/2 x-offset)
		-x (+ (- x/2) x-offset)
		y  (+ y/2 y-offset)
		-y (+ (- y/2) y-offset)))
    (case center
      (:center (setf x  x/2
		     -x (- x/2)
		     y  y/2
		     -y (- y/2)))
      (:top-center (setf x x/2
			 -x (- x/2)
			 y 0.0 
			 -y (- height)))
      (:bottom-center (setf x  x/2
			    -x (- x/2)
			    y  height
			    -y 0.0))
      (:center-left (setf x  width
			  -x 0.0
			  y  y/2
			  -y (- y/2)))
      (:center-right (setf x 0.0
			   -x (- width)
			   y  y/2
			   -y (- y/2)))
      (:top-left (setf x width
		       -x 0.0
		       y 0.0
		       -y (- height)))
      (:bottom-left (setf x width
			  -x 0.0
			  y height
			  -y 0.0))
      (:top-right (setf x 0.0
			-x (- width)
			y 0.0
			-y (- height)))
      
      (:bottom-right (setf x 0.0
			   -x (- width)
			   y height
			   -y 0.0))))
    

    (make-instance 'clinch:buffer 
		   :Stride 3
		   :data (map 'list (lambda (x)
				      (coerce x 'single-float))
			      (list  -x  y 0
				     -x  -y 0
				     x  -y 0
				     x   y 0)))))



(defun make-quad-texture-coordinates ()
  (make-instance 'clinch:buffer 
		 :Stride 2
		 :data (map 'list (lambda (x)
				    (coerce x 'single-float))
			    '(0.0   1.0
			      0.0   0.0
			      1.0   0.0
			      1.0   1.0))))

(defun make-quad (width height &key (center :center) shader-program texture (parent *root*))
  "Creates a quad entity of width and height."
  (make-instance 'clinch:entity
		 :parent parent
		 :shader-program (or shader-program (get-generic-single-texture-shader))
		 :indexes (make-quad-indices)
		 :attributes (copy-list `(("v" . ,(make-quad-vertexes width height :center center))
					  ("tc1" . ,(make-quad-texture-coordinates))))
		 :uniforms (copy-list `(("M" . :model)
					("P" . :projection)
					("t1" . ,(or texture (get-identity-texture)))))))

(defmethod make-quad-for-texture ((this texture) &key width height (center :center) shader-program parent no-parent)
  "Creates a quad for a texture which defaults to texture's width and height."
  (make-quad (or width (width this))
	     (or height (height this))
	     :center center
	     :shader-program shader-program
	     :texture this
	     :parent parent))

(defmethod make-quad-and-texture (width height &key (center :center) shader-program (parent *root*) no-parent)
  (make-quad-for-texture
   (make-instance 'texture :width width :height height)
   :center center :shader-program shader-program :parent parent :no-parent no-parent))


(defun get-default-texture ()
  (unless *texture* 
    (setf *texture* 
	  (make-instance 'texture 
			 :width (width *viewport*)
			 :height (height *viewport*))))
  (unless *entity*
    (setf *entity*
	  (make-quad-for-texture *texture* :parent nil)))
  *texture*)


(defun make-sphere ()
  (multiple-value-bind (verts indices normals) (get-sphere-vertex-normal-and-index-buffers)
    
    (make-instance 'clinch:entity
		   :parent nil
		   :shader-program (get-generic-single-diffuse-light-shader)
		   :indexes indices
		   :attributes (copy-list `(("v" . ,verts)
					    ("n" . ,normals)
					    ("c" . (1.0 1.0 1.0 1.0))
					    ("tc1" . (0 0))))
		   :uniforms (copy-list `(("M" . :model)
					  ("P" . :projection)
					  ("N" . :normal)
					  ("t1" . ,(get-identity-texture))
					  ("ambientLight" . (.2 .2 .2))
					  ("lightDirection" . (0.5772705 -0.5772705 -0.5772705))
					  ("lightIntensity" . (.8 .8 .8)))))))



;; Interesting note: the normals are the same as the vertices.    
(defun get-sphere-vertex-normal-and-index-buffers ()
  (let* ((ICX 0.525731112119133606)
	 (ICZ 0.850650808352039932)
	 (v (make-instance 'buffer :data
			   (map 'list (lambda (x)
					(coerce x 'single-float))
				
				`( ,(- ICX) 0 ,ICZ
				    ,ICX 0 ,ICZ
				    ,(- ICX) 0 ,(- ICZ)
				    ,ICX 0 ,(- ICZ)
				    0 ,ICZ ,ICX
				    0 ,ICZ ,(- ICX)
				    0 ,(- ICZ) ,ICX
				    0 ,(- ICZ) ,(- ICX)
				    ,ICZ ,ICX 0
				    ,(- ICZ) ,ICX 0
				    ,ICZ ,(- ICX) 0
				    ,(- ICZ) ,(- ICX) 0)))))
    (values v 
	    (make-instance 'index-buffer 
			   :data '(0 1 4	  
				   0 4 9
				   9 4 5	
				   4 8 5
				   4 1 8
				   8 1 10
				   8 10 3  
				   5 8 3
				   5 3 2
				   2 3 7
				   7 3 10
				   7 10 6
				   7 6 11
				   11 6 0 
				   0 6 1 	
				   6 10 1
				   9 11 0
				   9 2 11
				   9 5 2	
				   7 11 2))
	    v)))
    

(defun make-box ()
  (multiple-value-bind (verts indices) (get-box-vertex-normal-texture-and-index-buffers)
    
    (make-instance 'clinch:entity
		   :parent nil
		   :shader-program (get-generic-single-color-shader)
		   :indexes indices
		   :attributes (copy-list `(("v" . ,verts)))
		   :uniforms (copy-list `(("M" . :model)
					  ("P" . :projection)
					  ("color" . (1 1 1 1)))))))


(defun get-box-vertex-normal-texture-and-index-buffers ()
  (! (values 
      (make-instance 'clinch:buffer 
		     :Stride 3
		     :data '(-0.5 -0.5  0.5
			     0.5 -0.5  0.5
			     -0.5  0.5  0.5
			     0.5  0.5  0.5
			     -0.5 -0.5 -0.5
			     0.5 -0.5 -0.5
			     -0.5 -0.5  0.5
			     0.5 -0.5  0.5
			     -0.5  0.5 -0.5
			     0.5  0.5 -0.5
			     -0.5 -0.5 -0.5
			     0.5 -0.5 -0.5
			     -0.5  0.5  0.5
			     0.5  0.5  0.5
			     -0.5  0.5 -0.5
			     0.5  0.5 -0.5
			     0.5 -0.5  0.5
			     0.5 -0.5 -0.5
			     0.5  0.5  0.5
			     0.5  0.5 -0.5
			     -0.5 -0.5 -0.5 
			     -0.5 -0.5  0.5 
			     -0.5  0.5 -0.5 
			     -0.5  0.5  0.5))

      (make-instance 'clinch:index-buffer 
		     :data '(0  1  2 
			     2  1  3
			     4  5  6
			     6  5  7
			     8  9 10 
			     10  9 11
			     12 13 14 
			     14 13 15
			     16 17 18 
			     18 17 19
			     20 21 22 
			     22 21 23))


      (make-instance 'clinch:buffer 
		  :data '(0.0 0.0 1.0
			  0.0 0.0 1.0
			  0.0 0.0 1.0
			  0.0 0.0 1.0
			  0.0 -1.0 0.0
			  0.0 -1.0 0.0
			  0.0 -1.0 0.0
			  0.0 -1.0 0.0
			  0.0 0.0 -1.0
			  0.0 0.0 -1.0
			  0.0 0.0 -1.0
			  0.0 0.0 -1.0
			  0.0 1.0 0.0
			  0.0 1.0 0.0
			  0.0 1.0 0.0
			  0.0 1.0 0.0
			  1.0 0.0 0.0
			  1.0 0.0 0.0
			  1.0 0.0 0.0
			  1.0 0.0 0.0
			  -1.0 0.0 0.0
			  -1.0 0.0 0.0
			  -1.0 0.0 0.0
			  -1.0 0.0 0.0))

      (make-instance 'clinch:buffer 
		  :Stride 2
		  :data '(0.0 1.0
			  1.0 1.0
			  0.0 0.0
			  1.0 0.0
			  0.0 1.0
			  1.0 1.0
			  0.0 0.0
			  1.0 0.0
			  0.0 1.0
			  1.0 1.0
			  0.0 0.0
			  1.0 0.0
			  0.0 1.0
			  1.0 1.0
			  0.0 0.0
			  1.0 0.0
			  0.0 1.0
			  1.0 1.0
			  0.0 0.0
			  1.0 0.0
			  0.0 1.0
			  1.0 1.0
			  0.0 0.0
			  1.0 0.0)))))


(defun make-cylinder (&optional (sides 10))
  (multiple-value-bind (verts indices) (get-cylinder-vertex-normal-and-index-buffers sides)
    
    (make-instance 'clinch:entity
		   :parent nil
		   :shader-program (get-generic-single-color-shader)
		   :indexes indices
		   :attributes (copy-list `(("v" . ,verts)))
		   :uniforms (copy-list `(("M" . :model)
					  ("P" . :projection)
					  ("color" . (1 1 1 1)))))))


(defun get-cylinder-vertex-normal-and-index-buffers (sides)
  (multiple-value-bind (v n)
      (loop 
	 for i from 0 below (* 2 pi) by (/ (* 2 pi) sides)
	 for x = (cos i)
	 for y = (sin i)
	 append (list x y  0.5
		      x y -0.5
		      x y  0.5
		      x y -0.5)  into verts
	 append (list x y 0.0
		      0.0 0.0 1.0
		      x y 0.0
		      x y -1.0) into norms
	   
	 finally (return (values verts norms)))
    (setf v (append v '(0.0 0.0 0.5 0.0 0.0 -0.5)))
    (setf n (append n '(0.0 0.0 0.5 0.0 0.0 -0.5)))
    (let* ((len (/ (length v) 3)))
      (values (make-instance 'buffer :data (map 'list (lambda (x) (coerce x 'float)) v))
	      (make-instance 'index-buffer
			     :data (loop for x from 0 below sides
				      append (let ((offset (* x 4)))						     
					       (map 'list (lambda (y)
							    (if (< y 0)
								(+ len y)
								(mod (+ offset y) (- len 2))))
						    `(0 1 4 1 5 4  ; side
							5 1 -1 
							0 4 -2)))))
	      (make-instance 'buffer :data (map 'list (lambda (x) (coerce x 'float)) n))))))
				 
			
	      

(defun random-heightfield (w h)
  (make-array (* w h 3)
	      :adjustable t
	      :fill-pointer t
	      :element-type 'single-float
	      :initial-contents 
	      (map 'list (lambda (x)
			   (coerce x 'single-float))
		   (loop for i from 0 below w
		      append (loop for j from 0 below h
				append (list j
					     (/ (random 1000) 1000)
					     i))))))





(defun make-height-map-entity (arr x y &key texture)
  (multiple-value-bind (iarr varr narr tarr)

    (! 
      (let ((indexes (make-instance 'index-buffer :data iarr))
	     (points  (make-instance 'buffer :data varr))
	     (normals (make-instance 'buffer :data narr))
	     (tex-coords (make-instance 'buffer :data tarr)))
	 
	 (make-instance 'clinch:entity
			:parent nil
			:shader-program (get-generic-single-diffuse-light-shader)
			:indexes indexes
			:attributes (copy-list `(("v" . ,points)
						 ("n" . ,normals)
						 ("c" . (1.0 1.0 1.0 1.0))
						 ("tc1" . ,tex-coords)))
			:uniforms (copy-list `(("M" . :model)
					       ("P" . :projection)
					       ("N" . :normal)
					       ("t1" . ,(or texture (get-identity-texture)))
					       ("ambientLight" . (.2 .2 .2))
					       ("lightDirection" . (0.5772705 -0.5772705 -0.5772705))
					       ("lightIntensity" . (.8 .8 .8)))))))))

(defmethod delete-entity ((this entity) &key)

  (map nil #'unload (attributes this))
  ;;(map nil #'unload (uniforms this))
  (unload (indexes this))
  (unload this)
  nil)


(defun get-point-or-vert (arr x y w h)
  
  (cond ((< x 0)  nil)
	((>= x w) nil)
	((< y 0)  nil)
	((>= y h) nil)
	(t (let ((pos (* 3 (+ x (* y w)))))
	     (v! (elt arr (+ pos 0))
		 (elt arr (+ pos 1))
		 (elt arr (+ pos 2)))))))


(defun append-v3-to-array (arr v)
  
  (map nil (lambda (a)
	     (vector-push-extend a arr))
       v)
  arr)
		

(defun get-square-corners (arr x y w h)
  (list
   (clinch::get-point-or-vert arr x y w h)
   (clinch::get-point-or-vert arr (1+ x) y w h)
   (clinch::get-point-or-vert arr x (1+ y) w h)
   (clinch::get-point-or-vert arr (1+ x) (1+ y) w h)))


(defun get-cross-corners (arr x y w h)
  (list
   (clinch::get-point-or-vert arr (1- x) y w h)
   (clinch::get-point-or-vert arr (1+ x) y w h)
   (clinch::get-point-or-vert arr x (1- y) w h)
   (clinch::get-point-or-vert arr x (1+ y) w h)))



(defun get-middle-of-squares (arr w h)
  (setf out (make-array 0 :fill-pointer t :adjustable t :element-type :float))
  (loop for i from 0 below (1- h)
     do (loop for j from 0 below (1- w)
	   do (progn
		(append-v3-to-array out
				    (v3:/S (reduce #'v3:+ (get-square-corners arr j i w h))
					   4.0)))))
  out)

(defun get-middle-square-normal (arr x y w h) 
  (let* ((corners (get-square-corners arr x y w h))
	 (bl (first corners))
	 (br (second corners))
	 (tl (third corners))
	 (tr (fourth corners)))
	     
    (v3:normalize 
     (reduce #'v3:+ (list 
		     (cross-x bl tl)
		     (cross-x br bl)
		     (cross-x tr br)
		     (cross-x tl tr))))))


(defun get-middle-square-normals (arr w h) 
  (let ((out 
	 (setf out (make-array 0 :fill-pointer t :adjustable t :element-type :float))))
  (loop for y from 0 below (1- h)
       do (loop for x from 0 below (1- w)
	       do (progn 
		    (append-v3-to-array out 
				      (get-middle-square-normal arr x y w h)))))
  out))

(defun cross-x (a b)
  (if (and a b)
      (v3:cross a b)
      (v! 0 0 0)))
      
(defun get-cross-square-normal (arr x y w h)
  (let* ((corners (get-cross-corners arr x y w h))
	 (l (first corners))
	 (r (second corners))
	 (bottom (third corners))
	 (top (fourth corners)))
    
    (v3:normalize 
     (reduce #'v3:+ (list 
		      (cross-x l top)
		      (cross-x top r)
		      (cross-x bottom r)
		      (cross-x l bottom))))))

(defun get-cross-square-normals (arr w h)
  (let ((out
	 (setf out (make-array 0 :fill-pointer t :adjustable t :element-type :float))))
    (loop for y from 0 below h
       do (loop for x from 0 below w
	     do (progn
		  (append-v3-to-array out
				      (get-cross-square-normal arr x y w h)))))
    out))



(defun get-tex-coords (w h)
  (setf arr (make-array 0 :fill-pointer t :adjustable t :element-type :float))
  (loop for y from 0 below h
       do (loop for x from 0 below w
  	     do (progn 
  		  (vector-push-extend (float x) arr)
  		  (vector-push-extend (float y) arr))))
  
  (loop for y from .5 below (1- h) by 1
     do (loop for x from .5 below (1- w) by 1
	   do (progn 
		(vector-push-extend (float x) arr)
		(vector-push-extend (float y) arr))))
  arr)


(defun make-height-map-quad (x y w h)
  ;;(print (list x y))
  (let* ((quad (+ x (* y (1- w))))
	 (offset (* w h))
	 (c (+ quad offset))
	 (bl (+ (* y w) x))
	 (br (1+ bl))
	 (tl (+ bl w))
	 (tr (1+ tl)))
    (print (list quad c bl br tl tr))
    (list (v! c bl br)
	  (v! c br tr)
	  (v! c tr tl)
	  (v! c tl bl))))
     


(defun make-height-map-indexes (w h)
  (let ((out (make-array 0 :fill-pointer t :adjustable t :element-type :int)))
    (loop
	 with test = 0
       for y from 0 below (1- h)
       append (loop for x from 0 below (1- w)
		 do (progn (print (list x y (incf test)))
			   (map nil (lambda (a)
				      (append-v3-to-array out (map 'vector #'rationalize a)))
				(make-height-map-quad x y w h)))))
       out))


(defun make-height-map-arrays (arr w h)
  (let ((middles (clinch::get-middle-of-squares arr w h))
	(normals (clinch::get-cross-square-normals arr w h))
	(m-norms (clinch::get-middle-square-normals arr w h))
	(tex (clinch::get-tex-coords w h))
	(index (clinch::make-height-map-indexes w h)))
    
    (values 
     index
     (concatenate 'vector arr middles)
     (concatenate 'vector normals m-norms)
     tex)))

(defun make-height-map-entity (arr x y &key texture)
  (multiple-value-bind (iarr varr narr tarr) (clinch::make-height-map-arrays arr x y)
      
    (! 
      (let ((indexes (make-instance 'index-buffer :data iarr))
	     (points  (make-instance 'buffer :data varr))
	     (normals (make-instance 'buffer :data narr))
	     (tex-coords (make-instance 'buffer :data tarr)))
	 
	 (make-instance 'clinch:entity
			:parent nil
			:shader-program (get-generic-single-diffuse-light-shader)
			:indexes indexes
			:attributes (copy-list `(("v" . ,points)
						 ("n" . ,normals)
						 ("c" . (1.0 1.0 1.0 1.0))
						 ("tc1" . ,tex-coords)))
			:uniforms (copy-list `(("M" . :model)
					       ("P" . :projection)
					       ("N" . :normal)
					       ("t1" . ,(or texture (get-identity-texture)))
					       ("ambientLight" . (.2 .2 .2))
					       ("lightDirection" . (0.5772705 -0.5772705 -0.5772705))
					       ("lightIntensity" . (.8 .8 .8)))))))))
	     


