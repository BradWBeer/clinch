(make-instance
 'clinch:entity
 :shader shader
 :indexes (make-instance 'clinch:buffer :qtype :unsigned-int
			 :target :element-array-buffer
			 :Stride 1
			 :data clode:sphere-indexes)
 :vertices (make-instance 'clinch:buffer 
			  :Stride 3
			  :data (map 'list (lambda (x)
					     (coerce x 'single-float)) clode:sphere-vertexes))
 :normals (make-instance 'clinch:buffer 
			 :Stride 3
			 :data (map 'list (lambda (x)
					    (coerce x 'single-float)) clode:sphere-normals))
 :values   `((:uniform "ambientLight"   ,ambientLight)
	     (:uniform "lightIntensity" ,lightIntensity)
	     (:uniform "lightDirection" ,(lambda (&optional a b c) lightDirection))
	     (:uniform "color"  (1.0 0.0 0.0 1.0))))


(defun draw-sphere (radius rings sectors)

  (let ((vertices  (make-array (* rings sectors 3)))
	(normals   (make-array (* rings sectors 3)))
	(texcoords (make-array (* rings sectors 2)))
	(indices   (make-array (* rings sectors 6)))
	(slice-r (/ (1- rings)))
	(slice-s (/ (1- sectors))))
    
    (loop for r from 0 to (1- rings)
	 
       do (loop for s from 0 to (1- sectors)
	     for y = (sin (+ (* -2 pi)
			     (* pi r slice-r)))
	       
	     for x = (* (cos (* 2 pi s slice-s))
			(sin (* pi r slice-r)))
	       
	     for z = (* (sin (* 2 pi s slice-s))
			(sin (* pi r slice-r)))
	       
	     do (let* ((triangle (+ (* r sectors) s))
		       (3t (* 3 triangle))
		       (2t (* 2 triangle))
		       (6t (* 6 triangle)))
		  
		  (format t "ring ~A, sector ~A, MULT ~A~%" r s triangle)
		  (format t "verts: ~A ~A ~A~%" (* radius x) 
			  (* radius y) 
			  (* radius z))
		  (format t "norms: ~A ~A ~A~%" x y z)
		  (format t "texcood: ~A ~A~%" (* s sectors) (* r rings))

		  (setf (aref vertices (+ 3t 0)) (* radius x)
			(aref vertices (+ 3t 1)) (* radius y)
			(aref vertices (+ 3t 2)) (* radius z)

			(aref normals (+ 3t 0)) x
			(aref normals (+ 3t 1)) y
			(aref normals (+ 3t 2)) z
			
			(aref texcoords (+ 2t 0)) (* s slice-s)
			(aref texcoords (+ 2t 1)) (* r slice-r)
			
			(aref indices (+ 6t 0)) (+ (* r sectors) s)
			(aref indices (+ 6t 1)) (+ (* r sectors) s 1)
			(aref indices (+ 6t 2)) (+ (* (1+ r) sectors) s 1)

			(aref indices (+ 6t 3)) (+ (* r sectors) s)
			(aref indices (+ 6t 4)) (+ (* (1+ r) sectors) s 1)
			(aref indices (+ 6t 5)) (+ (* (1+ r) sectors) s)))))
    

    (values indices vertices normals texcoords)))
