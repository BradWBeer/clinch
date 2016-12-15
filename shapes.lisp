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
		 :attributes `(("v" . ,(make-quad-vertexes width height :center center))
			       ("tc1" . ,(make-quad-texture-coordinates)))
		 :uniforms `(("M" . :model)
			     ("P" . :projection)
			     ("t1" . ,(or texture (get-default-texture))))))

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
  (multiple-value-bind (verts indices) (get-sphere-vertex-normal-and-index-buffers)
    
    (make-instance 'clinch:entity
		   :parent nil
		   :shader-program (get-generic-single-color-shader)
		   :indexes indices
		   :attributes `(("v" . ,verts))
		   :uniforms `(("M" . :model)
			       ("P" . :projection)
			       ("color" . (1 1 1 1))))))


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
			   :data '(0 4 1	  
				   0 9 4
				   9 5 4	
				   4 5 8
				   4 8 1
				   8 10 1
				   8 3 10 
				   5 3 8
				   5 2 3
				   2 7 3
				   7 10 3
				   7 6 10
				   7 11 6
				   11 0 6
				   0 1 6	
				   6 1 10
				   9 0 11
				   9 11 2
				   9 2 5	
				   7 2 11))
	    v)))
    

(defun make-box ()
  (multiple-value-bind (verts indices) (get-box-vertex-normal-texture-and-index-buffers)
    
    (make-instance 'clinch:entity
		   :parent nil
		   :shader-program (get-generic-single-color-shader)
		   :indexes indices
		   :attributes `(("v" . ,verts))
		   :uniforms `(("M" . :model)
			       ("P" . :projection)
			       ("color" . (1 1 1 1))))))


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
		   :attributes `(("v" . ,verts))
		   :uniforms `(("M" . :model)
			       ("P" . :projection)
			       ("color" . (1 1 1 1))))))


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
				 
			
	      

(defun random-heightfield (w h &optional (max 1) (x-offset (/ (1- w) -2)) (y-offset (/ (1- h) -2)))
  (make-array (* w h 3)
	      :element-type 'single-float
	      :initial-contents 
	      (map 'list (lambda (x)
			   (coerce x 'single-float))
		   (loop for i from 0 below w
		      append (loop for j from 0 below h
				append (list (+ i x-offset)
					     (* max (/ (random 1000) 1000))
					     (+ j y-offset)))))))


(defun heightmap->buffers (arr w h)
  (let ((tx (/ (1- w)))
	(ty (/ (1- h)))
	(points)
	(normals)
	(tex-coords))
    
  (labels ((get-point (x)
	     (let ((start (* 3 x)))
	       (subseq arr start (+ 3 start))))
	   
	   (make-triangle (a b c)
	     (let ((n (v3:normalize 
		       (v3:cross (v3:- b a) (v3:- c a)))))
	       (values (concatenate 'vector a b c)
		       (concatenate 'vector n n n))))

	   (add-triangle (a)
	     (multiple-value-bind (p n)
		 (apply #'make-triangle 
			(map 'list #'get-point a))
	        (push p points)
	        (push n normals)))

	   (add-tc (x y)
	     (push (v! (* tx x) (* ty y)) tex-coords)
	     (push (v! (* tx (1+ x)) (* ty y)) tex-coords)
	     (push (v! (* tx x) (* ty (1+ y))) tex-coords)

	     (push (v! (* tx (1+ x)) (* ty y)) tex-coords)
	     (push (v! (* tx (1+ x)) (* ty (1+ y))) tex-coords)
	     (push (v! (* tx x) (* ty (1+ y))) tex-coords)))
	     
    (loop for i from 0 below (1- w)
       do (loop for j from 0 below (1- h)
	     for a1 = (+ j (* w i))
	     for a2 = (1+ a1)
	     for b1 = (+ w a1)
	     for b2 = (1+ b1)
	       
	     do (progn (add-triangle (list a1 a2 b1))
		       (add-triangle(list a2 b2 b1))
		       (add-tc i j))))
	
				
    (values (make-instance 'buffer :data (apply #'concatenate 'vector (reverse points)))
	    (make-instance 'index-buffer 
			   :data (loop for i from 0 below (* 3 2 w h)
				    collect i))
	    (make-instance 'buffer :stride 2 :data (apply #'concatenate 'vector (reverse tex-coords)))))))


    
;; (setf diamond '(#(0.0  1.0 0.0) #(-0.70710677 0.0 0.70710677) #(0.70710677 0.0 0.70710677)
;; 		#(0.0  1.0 0.0) #(0.9659259 0.0 0.25881892) #(0.25881892 0.0 -0.9659259)
;; 		#(0.0  1.0 0.0) #(-0.25881928 0.0 -0.9659258) #(-0.9659258 0.0 0.25881928)
;; 		#(0.0 -1.0 0.0) #(-0.70710677 0.0 0.70710677) #(0.70710677 0.0 0.70710677)
;; 		#(0.0 -1.0 0.0) #(0.9659259 0.0 0.25881892) #(0.25881892 0.0 -0.9659259)
;; 		#(0.0 -1.0 0.0) #(-0.25881928 0.0 -0.9659258) #(-0.9659258 0.0 0.25881928)))
 
;; (let ((i '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17)))
;;   (make-array (length i) :initial-contents i))

;; (defun subdivide (vectors indexes)
;;   (let ((midpoint (midpoint vectors)))
;;     (values (list (nth 0 vectors)
;; 		  (nth 1 vectors)
;; 		  (nth 2 vectors)
;; 		  midpoint)
;; 	    '((0 1 3) (1 2 3) (2 0 3)))))

;; ;; returns indexes, vertexes, normals, and texcoords in values
;; (defun make-sphere (radius rings sectors)

;;   (let ((vertices  (make-array (* rings sectors 3)))
;; 	(normals   (make-array (* rings sectors 3)))
;; 	(texcoords (make-array (* rings sectors 2)))
;; 	(indices   (make-array (* rings sectors 6)))
;; 	(slice-r (/ (1- rings)))
;; 	(slice-s (/ (1- sectors))))

;;     (loop for r from 0 to (1- rings)
;;        do (loop for s from 0 to (1- sectors)

;; 	     for y = (sin (+ (/ pi -2)
;; 			     (* pi r slice-r)))

;; 	     for x = (* (cos (* pi 2 s slice-s))
;; 			(sin (* pi r slice-r)))

;; 	     for z =  (* (sin (* 2 pi s slice-s))
;; 			 (sin (* pi r slice-r)))

;; 	     do (let* ((triangle (+ (* r sectors) s))
;; 		       (3t (* 3 triangle))
;; 		       (2t (* 2 triangle))
;; 		       (6t (* 6 triangle)))

;; 		  (when (and (< (abs x) .01)
;; 			     (< (abs y) .01)
;; 			     (< (abs z) .01))

;; 		    (format t "~A ~A was all zeros (~A ~A ~A)~%" r s x y z))

;; 		  (setf (aref vertices (+ 3t 0)) (* radius x)
;; 			(aref vertices (+ 3t 1)) (* radius y)
;; 			(aref vertices (+ 3t 2)) (* radius z)

;; 			(aref normals (+ 3t 0)) x
;; 			(aref normals (+ 3t 1)) y
;; 			(aref normals (+ 3t 2)) z

;; 			(aref texcoords (+ 2t 0)) (* s slice-s)
;; 			(aref texcoords (+ 2t 1)) (* r slice-r)

;; 			(aref indices (+ 6t 0)) (+ (* r sectors) s)
;; 			(aref indices (+ 6t 1)) (+ (* r sectors) s 1)
;; 			(aref indices (+ 6t 2)) (+ (* (1+ r) sectors) s 1)

;; 			(aref indices (+ 6t 3)) (+ (* r sectors) s)
;; 			(aref indices (+ 6t 4)) (+ (* (1+ r) sectors) s 1)
;; 			(aref indices (+ 6t 5)) (+ (* (1+ r) sectors) s)))))


;;     (values (coerce indices 'list)
;; 	    (coerce vertices 'list)
;; 	    (coerce normals 'list)
;; 	    (coerce texcoords 'list))))
